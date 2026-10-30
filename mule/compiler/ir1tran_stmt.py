"""Statement, assignment/annotation/delete target, and async construct IR1 lowering."""

from __future__ import annotations

from dataclasses import replace

from mule import ast

from . import fndb
from .ir1tran_expr import _MatchLowering, _star_positions
from .ir1tran_scope import (
    _CONTAINS_BREAK,
    _EXPORT_BINDINGS,
    _IS_GLOBAL_VAR,
    _IS_LITERAL,
    AttributeInfo,
    CallInfo,
    ClassInfo,
    ClassScope,
    InplaceInfo,
    RangeState,
    _AnnotatedDeclaration,
    _ClassBindings,
    _ClassBody,
    _ClassMethods,
    _Docstring,
    _documentation,
    _ForIteration,
    _NamedAnnotations,
    _NativeClassOptIn,
    _ReceiverFields,
    _ReceiverName,
    _RuntimeAnnotation,
    _SlotNames,
    _StaticAttributes,
)
from .node import (
    Cleanup,
    CleanupKind,
    Ctran,
    CtranKind,
    LoopTarget,
    Lvar,
    linked,
)


class _Statements(ast.Visitor):
    """Statement translators. Each links between the frame's start and next;
    one that transfers control elsewhere leaves next unused."""

    def __init__(self, function):
        self.function = function

    @property
    def frame(self):
        return self.function.frames[-1]

    def visit_Node(self, node):
        self.function.error(
            "syntax is not supported by the native compiler yet",
            node,
            unsupported=True,
        )

    def visit_Pass(self, node):
        return None

    def visit_Match(self, node):
        start, next_, _ = self.frame
        self.expanded(
            start, next_, _MatchLowering(self.function, node.span).statements(node)
        )

    def visit_TypeAlias(self, node):
        """``type Name[...] = value``: a TypeAliasType evaluating its value
        lazily, inside an annotation scope holding its type parameters."""
        function = self.function
        start, next_, _ = self.frame
        span = node.span
        statements = _annotation_scope_body(node, span)
        create = ast.Call(
            ast.Attribute(
                ast.Attribute(
                    ast.Name("$typing", span=span), "TypeAliasType", span=span
                ),
                "_lazy",
                span=span,
            ),
            [
                ast.CallArgument(argument, span=span)
                for argument in (
                    ast.TextLiteral(node.name, span=span),
                    ast.Lambda([], node.value, span=span),
                    _type_parameter_tuple(node, span),
                    ast.Name("__name__", span=span),
                )
            ],
            span=span,
        )
        statements.append(ast.Return(create, span=span))
        scope = ast.FunctionDefinition(
            f"<generic parameters of {node.name}>", [], statements, span=span
        )
        lambda_ = function.converter.function(
            scope,
            function.lambda_.policy,
            parent=function,
            defining_class=function.class_scopes[-1] if function.class_scopes else None,
            annotation_scope=True,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(start, ctran, created, lambda_, scope, scope.name)
        following, result = Ctran(), Lvar()
        function.full_call(
            ctran,
            following,
            result,
            created,
            (),
            node,
            CallInfo((), function.converter.location(node)),
        )
        function.store(following, next_, node.name, result, node)

    def visit_Global(self, node):
        return None

    def visit_Nonlocal(self, node):
        return None

    def visit_Delete(self, node):
        function = self.function
        start, next_, _ = self.frame
        current = start
        for index, target in enumerate(node.targets):
            last = index == len(node.targets) - 1
            following = next_ if last else Ctran()
            target.accept(_DeleteTarget(function, current, following))
            current = following

    def visit_Import(self, node):
        function = self.function
        for item in node.items:
            function.converter.constant_binding(
                item.alias or item.name.split(".")[0], item
            )
        start, next_, _ = self.frame
        current = start
        for item in node.items:
            if item.name == "mule_types":
                function._import_directive(ast.Import([item], span=node.span))
                continue
            current, imported = function.import_module(current, item.name, node)
            if "." in item.name and item.alias is None:
                for use in imported.uses:
                    use.lvar = None
                imported.uses = []
                current, imported = function.import_module(
                    current, item.name.split(".")[0], node
                )
            ctran = Ctran()
            function.store(
                current, ctran, item.alias or item.name.split(".")[0], imported, node
            )
            current = ctran
        if current is not start:
            function.flow(current, next_)

    def visit_ImportFrom(self, node):
        function = self.function
        for item in node.items:
            function.converter.constant_binding(item.alias or item.name, item)
        start, next_, _ = self.frame
        if node.level == 0 and node.module == "__future__":
            function._import_directive(node)
            return
        if node.level == 0 and node.module == "mule_types":
            directives = [item for item in node.items if item.name in _EXPORT_BINDINGS]
            runtime = [item for item in node.items if item.name not in _EXPORT_BINDINGS]
            if directives:
                function._import_directive(replace(node, items=directives))
            if not runtime:
                return
            node = replace(node, items=runtime)
        wildcard = any(item.name == "*" for item in node.items)
        if wildcard and (not function.is_module or function.class_scopes):
            function.error("import * only allowed at module level", node)
        if function.converter.imports is None:
            function.error(
                "runtime module imports are not supported by standalone lowering",
                node,
                unsupported=True,
            )
        imports = function.converter.imports
        module_name = imports.import_name(function.converter, node)
        if wildcard:
            function.component.mutates_module_namespace = True
            current, module = function.import_module(start, module_name, node)
            function.known(current, next_, None, fndb.IMPORT_STAR, (module,), node)
            return
        namespace = function.temporary()
        current, module = function.import_module(start, module_name, node)
        ctran = Ctran()
        function.set_var(current, ctran, namespace, module, node)
        current = ctran
        for item in node.items:
            child = module_name + "." + item.name
            if imports.path_for(child) is not None:
                exists, absent, joined = (
                    function.block(),
                    function.block(),
                    function.block(),
                )
                ctran, loaded = Ctran(), Lvar()
                function.ref(current, ctran, loaded, namespace, node)
                following, present = Ctran(), Lvar()
                function.known(
                    ctran,
                    following,
                    present,
                    fndb.HAS_BINDING,
                    (loaded,),
                    node,
                    item.name,
                )
                function.if_node(following, present, exists, absent, node)
                current, imported = function.import_module(absent.start, child, item)
                ctran = Ctran()
                function.store(current, ctran, item.alias or item.name, imported, item)
                function.jump(ctran, joined)
                ctran = function.import_attribute(exists.start, namespace, item)
                function.jump(ctran, joined)
                current = joined.start
            else:
                current = function.import_attribute(current, namespace, item)
        function.discard(current, next_, namespace.name, node)

    def visit_Assert(self, node):
        function = self.function
        start, next_, _ = self.frame
        passed, failed = function.block(next_), function.block()
        function.condition(start, node.condition, passed, failed)
        if node.message is not None:
            ctran, message = function.value(failed.start, node.message)
        else:
            ctran, message = Ctran(), Lvar()
            function.constant(
                failed.start, ctran, message, ast.NoneLiteral(None, span=node.span)
            )
        following = Ctran()
        function.known(
            ctran,
            following,
            None,
            fndb.ASSERTION_FAILURE,
            (message,),
            node,
            data=node.message is not None,
        )
        # The failure operation always branches to owned-reference cleanup.
        function.jump(following, passed)

    def visit_AnnotatedAssignment(self, node):
        start, next_, _ = self.frame
        node.target.accept(_AnnotationTarget(self.function, node, start, next_))

    def visit_ExpressionStatement(self, node):
        # A literal statement, such as a docstring, has no effect: Python's
        # compiler discards it, and converting it would build and release
        # the value on every execution.
        if node.expression.accept(_IS_LITERAL):
            return
        start, next_, _ = self.frame
        self.function.convert(start, next_, None, node.expression)

    def visit_Assignment(self, node):
        function = self.function
        start, next_, _ = self.frame
        ctran, value = function.value(start, node.value)
        if len(node.targets) == 1:
            node.targets[0].accept(_AssignmentTarget(function, value, ctran, next_))
            return
        temporary = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, temporary, value, node)
        for index, target in enumerate(node.targets):
            last = index == len(node.targets) - 1
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, temporary, node, final=last)
            current = next_ if last else Ctran()
            target.accept(_AssignmentTarget(function, loaded, ctran, current))

    def visit_AugmentedAssignment(self, node):
        start, next_, _ = self.frame
        node.target.accept(_AugmentedTarget(self.function, node, start, next_))

    def visit_If(self, node):
        function = self.function
        start, next_, _ = self.frame
        then, other, join = function.block(), function.block(), function.block(next_)
        function.condition(start, node.condition, then, other)
        frontier = function.convert_body(linked(then.start, then, "start"), node.body)
        if frontier is not None:
            function.jump(frontier, join)
        frontier = function.convert_body(
            linked(other.start, other, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, join)

    def visit_Raise(self, node):
        function = self.function
        start, _, _ = self.frame
        if node.exception is None:
            self.terminal(start, fndb.RERAISE, (), node)
            return
        ctran, value = function.value(start, node.exception)
        if node.cause is not None:
            # ``raise X from Y`` chains on the instance, so create it first.
            following, instance = Ctran(), Lvar()
            function.protocol(
                ctran, following, instance, "exception_instance", (value,), node
            )
            temporary = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, temporary, instance, node)
            following, cause = function.value(ctran, node.cause)
            ctran, loaded = Ctran(), Lvar()
            function.ref(following, ctran, loaded, temporary, node)
            following = Ctran()
            function.protocol(
                ctran, following, None, "set_exception_cause", (loaded, cause), node
            )
            ctran, value = Ctran(), Lvar()
            function.ref(following, ctran, value, temporary, node, final=True)
        self.terminal(ctran, fndb.RAISE, (value,), node)

    def terminal(self, start, info, operands, node):
        """Emit a call that never returns; it ends its block with no successors."""
        self.function.known(start, None, None, info, operands, node)

    def visit_Try(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.finally_body:
            self.try_finally(start, next_, node)
            return
        if not node.handlers:
            frontier = function.convert_body(start, node.body)
            if frontier is not None:
                frontier = function.convert_body(frontier, node.else_body)
            if frontier is not None:
                function.flow(frontier, next_)
            return
        if node.exception_groups:
            self.try_except_star(start, next_, node)
            return
        self.try_handlers(start, next_, node)

    def try_finally(self, start, next_, node):
        """``try``/``finally``: the protected statement runs inside a FINALLY
        cleanup whose exits run the suite; its error dispatch runs the suite
        and re-raises unless the suite transfers control."""
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, done = function.block(), function.block()
        region = Cleanup(
            CleanupKind.FINALLY,
            target=dispatch,
            parent=outer,
            info=node.finally_body,
            loops=function.lexenv.loops,
        )
        protected = ast.Try(
            node.body,
            node.handlers,
            node.else_body,
            [],
            exception_groups=node.exception_groups,
            span=node.span,
        )
        with function.with_cleanup(region):
            body = function.block()
            function.jump(start, body)
            following = Ctran()
            body_start = linked(body.start, body, "start")
            function.convert_statement(body_start, following, protected)
            if body_start.next is None:
                function.flow(body_start, following)
            if following.kind is not CtranKind.UNUSED:
                function.jump(following, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(current, ctran, raised, exception, node)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), node)
        unwind, reraise = function.block(), function.block()
        active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=unwind,
            parent=outer,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(active):
            finally_block = function.block()
            function.jump(entered, finally_block)
            current = linked(finally_block.start, finally_block, "start")
            active.mess_up = current.next
            frontier = function.convert_body(current, node.finally_body)
            if frontier is not None:
                function.jump(frontier, reraise)
        ctran, value = Ctran(), Lvar()
        function.ref(reraise.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.known(unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), node)
        self.terminal(ctran, fndb.UNWIND, (), node)
        function.flow(linked(done.start, done, "start"), next_)

    def try_except_star(self, start, next_, node):
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, success, done = function.block(), function.block(), function.block()
        region = Cleanup(CleanupKind.HANDLER_REGION, target=dispatch, parent=outer)
        with function.with_cleanup(region):
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(
                linked(body.start, body, "start"), node.body
            )
            if frontier is not None:
                function.jump(frontier, success)
        frontier = function.convert_body(
            linked(success.start, success, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        following, initial_state = Ctran(), Lvar()
        function.protocol(
            ctran, following, initial_state, "except_star_start", (caught,), node
        )
        state = function.temporary()
        current = Ctran()
        function.set_var(following, current, state, initial_state, node)
        for handler in node.handlers:
            body, unmatched = function.block(), function.block()
            ctran, expected = function.value(current, handler.exception)
            following, state_val = Ctran(), Lvar()
            function.ref(ctran, following, state_val, state, handler)
            ctran, matches = Ctran(), Lvar()
            function.protocol(
                following,
                ctran,
                matches,
                "except_star_match",
                (state_val, expected),
                handler,
            )
            function.if_node(ctran, matches, body, unmatched, handler)
            ctran, state_val = Ctran(), Lvar()
            function.ref(body.start, ctran, state_val, state, handler)
            matched_ctran, matched_val = Ctran(), Lvar()
            function.protocol(
                ctran,
                matched_ctran,
                matched_val,
                "except_star_matched",
                (state_val,),
                handler,
            )
            entered = Ctran()
            function.known(
                matched_ctran,
                entered,
                None,
                fndb.ENTER_HANDLER,
                (matched_val,),
                handler,
            )
            unwind = function.block()
            active = Cleanup(
                CleanupKind.HANDLER_BODY,
                target=unwind,
                binding=handler.binding,
                parent=outer,
            )
            with function.with_cleanup(active):
                inner = function.block()
                function.jump(entered, inner)
                current = linked(inner.start, inner, "start")
                active.mess_up = current.next
                if handler.binding is not None:
                    function.converter.reserved(handler.binding, handler)
                    ctran, state_val = Ctran(), Lvar()
                    function.ref(current, ctran, state_val, state, handler)
                    matched_ctran, matched_val = Ctran(), Lvar()
                    function.protocol(
                        ctran,
                        matched_ctran,
                        matched_val,
                        "except_star_matched",
                        (state_val,),
                        handler,
                    )
                    current = Ctran()
                    function.store(
                        matched_ctran, current, handler.binding, matched_val, handler
                    )
                frontier = function.convert_body(current, handler.body)
                if frontier is not None:
                    function.jump(frontier, unmatched)
            current = linked(unwind.start, unwind, "start")
            ctran, handler_exc = Ctran(), Lvar()
            function.known(
                current, ctran, handler_exc, fndb.CATCH_EXCEPTION, (), handler
            )
            current = ctran
            if handler.binding is not None:
                ctran = Ctran()
                function.unbind(
                    current, ctran, handler.binding, handler, missing_ok=True
                )
                current = ctran
            ctran = Ctran()
            function.known(current, ctran, None, fndb.LEAVE_HANDLER, (), handler)
            following, state_val = Ctran(), Lvar()
            function.ref(ctran, following, state_val, state, handler)
            caught_ctran, discarded = Ctran(), Lvar()
            function.protocol(
                following,
                caught_ctran,
                discarded,
                "except_star_caught",
                (state_val, handler_exc),
                handler,
            )
            for use in discarded.uses:
                use.lvar = None
            discarded.uses = []
            function.jump(caught_ctran, unmatched)
            current = linked(unmatched.start, unmatched, "start")
        ctran, state_val = Ctran(), Lvar()
        function.ref(current, ctran, state_val, state, node, final=True)
        following, remaining = Ctran(), Lvar()
        function.protocol(
            ctran, following, remaining, "except_star_finish", (state_val,), node
        )
        unhandled = function.temporary()
        stored = Ctran()
        function.set_var(following, stored, unhandled, remaining, node)
        ctran, unhandled_val = Ctran(), Lvar()
        function.ref(stored, ctran, unhandled_val, unhandled, node)
        none_ctran, none_val = Ctran(), Lvar()
        function.constant(
            ctran, none_ctran, none_val, ast.NoneLiteral(None, span=node.span)
        )
        check_ctran, has_unhandled = Ctran(), Lvar()
        function.known(
            none_ctran,
            check_ctran,
            has_unhandled,
            fndb.COMPARE[ast.ComparisonOperator.IS_NOT],
            (unhandled_val, none_val),
            node,
        )
        reraise_block = function.block()
        function.if_node(check_ctran, has_unhandled, reraise_block, done, node)
        ctran, value = Ctran(), Lvar()
        function.ref(reraise_block.start, ctran, value, unhandled, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.set_var(done.start, ctran, state, None, node)
        end_ctran = Ctran()
        function.set_var(ctran, end_ctran, unhandled, None, node)
        function.flow(end_ctran, next_)

    def try_handlers(self, start, next_, node):
        function = self.function
        outer = function.lexenv.cleanup
        dispatch, success, done = function.block(), function.block(), function.block()
        region = Cleanup(CleanupKind.HANDLER_REGION, target=dispatch, parent=outer)
        with function.with_cleanup(region):
            # A handler covers only the body: its blocks start here.
            body = function.block()
            function.jump(start, body)
            frontier = function.convert_body(
                linked(body.start, body, "start"), node.body
            )
            if frontier is not None:
                function.jump(frontier, success)
        frontier = function.convert_body(
            linked(success.start, success, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        current = Ctran()
        function.set_var(ctran, current, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(current, ctran, raised, exception, node)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), node)
        match_unwind, reraise = function.block(), function.block()
        unwinds = [
            function.block() if handler.binding is not None else None
            for handler in node.handlers
        ]
        match_active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=match_unwind,
            parent=outer,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(match_active):
            check_entry = function.block()
            function.jump(entered, check_entry)
            current = linked(check_entry.start, check_entry, "start")
            match_active.mess_up = current.next
            for handler, unwind in zip(node.handlers, unwinds, strict=True):
                body, unmatched = function.block(), function.block()
                if handler.exception is None:
                    function.jump(current, body)
                else:
                    ctran, expected = function.value(current, handler.exception)
                    following, raised = Ctran(), Lvar()
                    function.ref(ctran, following, raised, exception, handler)
                    ctran, matches = Ctran(), Lvar()
                    function.protocol(
                        following,
                        ctran,
                        matches,
                        "exception_matches",
                        (raised, expected),
                        handler,
                    )
                    function.if_node(ctran, matches, body, unmatched, handler)
                if handler.binding is None:
                    frontier = function.convert_body(
                        linked(body.start, body, "start"), handler.body
                    )
                    if frontier is not None:
                        # An empty suite still needs an exit in the active cleanup,
                        # so environment analysis can emit the handler-leaving code.
                        function.jump(frontier, done)
                else:
                    active = Cleanup(
                        CleanupKind.HANDLER_BODY,
                        target=unwind,
                        binding=handler.binding,
                        parent=match_active,
                        loops=function.lexenv.loops,
                        info=False,
                    )
                    with function.with_cleanup(active):
                        # The handler body's own error destination starts a new block.
                        inner = function.block()
                        function.jump(linked(body.start, body, "start"), inner)
                        current = linked(inner.start, inner, "start")
                        active.mess_up = current.next
                        function.converter.reserved(handler.binding, handler)
                        ctran, raised = Ctran(), Lvar()
                        function.ref(current, ctran, raised, exception, handler)
                        current = Ctran()
                        function.store(ctran, current, handler.binding, raised, handler)
                        frontier = function.convert_body(current, handler.body)
                        if frontier is not None:
                            function.jump(frontier, done)
                current = linked(unmatched.start, unmatched, "start")
            function.jump(current, reraise)
        for handler, unwind in zip(node.handlers, unwinds, strict=True):
            if unwind is None:
                continue
            current = linked(unwind.start, unwind, "start")
            ctran = Ctran()
            function.unbind(current, ctran, handler.binding, handler, missing_ok=True)
            following = Ctran()
            function.known(ctran, following, None, fndb.LEAVE_HANDLER, (), handler)
            self.terminal(following, fndb.UNWIND, (), handler)
        # No handler matched: reinstall the original caught exception.
        ctran, value = Ctran(), Lvar()
        function.ref(reraise.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.known(match_unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), node)
        self.terminal(ctran, fndb.UNWIND, (), node)
        ctran = Ctran()
        function.set_var(done.start, ctran, exception, None, node)
        function.flow(ctran, next_)

    def visit_With(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.is_async:
            if (
                function.body_kind not in ("coroutine", "async_generator")
                or function.class_scopes
            ):
                function.error("'async with' outside async function", node)
            self.expanded(start, next_, _async_with(function, node))
            return
        self.with_items(start, next_, node, 0)

    def expanded(self, start, next_, statements):
        """Convert statements a construct expands into, in its place."""
        frontier = self.function.convert_body(start, statements)
        if frontier is not None:
            self.function.flow(frontier, next_)

    def with_items(self, start, next_, node, index):
        """Enter one ``with`` item; nested items wrap the body in turn."""
        function = self.function
        if index == len(node.items):
            frontier = function.convert_body(start, node.body)
            if frontier is not None:
                function.flow(frontier, next_)
            return
        item = node.items[index]
        outer = function.lexenv.cleanup
        manager, exit_method = function.temporary(), function.temporary()
        ctran, value = function.value(start, item.context)
        current = Ctran()
        function.set_var(ctran, current, manager, value, item)
        ctran, loaded = Ctran(), Lvar()
        function.ref(current, ctran, loaded, manager, item)
        following, bound_exit = Ctran(), Lvar()
        function.protocol(
            ctran,
            following,
            bound_exit,
            "context_exit_method",
            (loaded,),
            item,
        )
        current = Ctran()
        function.set_var(following, current, exit_method, bound_exit, item)
        ctran, loaded = Ctran(), Lvar()
        function.ref(current, ctran, loaded, manager, item)
        following, entered = Ctran(), Lvar()
        function.protocol(
            ctran,
            following,
            entered,
            "context_enter",
            (loaded,),
            item,
        )
        ctran = following
        if item.target is not None:
            current = Ctran()
            item.target.accept(_AssignmentTarget(function, entered, ctran, current))
        else:
            for use in entered.uses:
                use.lvar = None
            entered.uses = []
            current = ctran
        dispatch, done = function.block(), function.block()
        region = Cleanup(
            CleanupKind.WITH,
            target=dispatch,
            parent=outer,
            info=exit_method,
            loops=function.lexenv.loops,
        )
        with function.with_cleanup(region):
            body = function.block()
            function.jump(current, body)
            following = Ctran()
            body_start = linked(body.start, body, "start")
            self.with_items(body_start, following, node, index + 1)
            region.mess_up = body_start.next
            if body_start.next is None:
                function.flow(body_start, following)
            if following.kind is not CtranKind.UNUSED:
                function.jump(following, done)
        # An exception inside the body goes to __exit__; a true result suppresses it.
        ctran, caught = Ctran(), Lvar()
        function.known(dispatch.start, ctran, caught, fndb.CATCH_EXCEPTION, (), item)
        exception = function.temporary()
        following = Ctran()
        function.set_var(ctran, following, exception, caught, item)
        ctran, raised = Ctran(), Lvar()
        function.ref(following, ctran, raised, exception, item)
        entered = Ctran()
        function.known(ctran, entered, None, fndb.ENTER_HANDLER, (raised,), item)
        unwind = function.block()
        active = Cleanup(
            CleanupKind.HANDLER_BODY,
            target=unwind,
            parent=outer,
        )
        with function.with_cleanup(active):
            body = function.block()
            function.jump(entered, body)
            ctran, method = Ctran(), Lvar()
            function.ref(body.start, ctran, method, exit_method, item)
            following, raised = Ctran(), Lvar()
            function.ref(ctran, following, raised, exception, item)
            ctran, suppressed = Ctran(), Lvar()
            function.protocol(
                following, ctran, suppressed, "context_exit", (method, raised), item
            )
            reraise = function.block()
            function.if_node(ctran, suppressed, done, reraise, item)
            ctran, value = Ctran(), Lvar()
            function.ref(reraise.start, ctran, value, exception, item, final=True)
            self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), item)
        ctran = Ctran()
        function.known(unwind.start, ctran, None, fndb.LEAVE_HANDLER, (), item)
        self.terminal(ctran, fndb.UNWIND, (), item)
        function.flow(linked(done.start, done, "start"), next_)

    def visit_For(self, node):
        function = self.function
        start, next_, _ = self.frame
        if node.is_async:
            if (
                function.body_kind not in ("coroutine", "async_generator")
                or function.class_scopes
            ):
                function.error("'async for' outside async function", node)
            self.expanded(start, next_, _async_for(function, node))
            return
        if function.comprehension_scopes:
            prepared = function.comprehension_scopes[-1].outer_iteration
            if prepared is not None and prepared[0] is node:
                prepared[1].convert(self, start, next_, node)
                return
        genexpr = (
            function.fusible_generator_expression(node.iterable)
            if not node.else_body
            else None
        )
        if genexpr is not None and (
            len(genexpr.clauses) == 1
            or not any(stmt.accept(_CONTAINS_BREAK) for stmt in node.body)
        ):
            self.for_generator(start, next_, node, genexpr)
            return
        current, iteration = function.prepare_iteration(start, node.iterable, node)
        self.for_iteration(current, next_, node, iteration)

    def for_generator(self, start, next_, node, genexpr, iteration=None):
        """Inline yielded values into the enclosing loop's existing consumer."""
        function = self.function
        span = node.span
        item_temp = function.temporary()
        item_name = ast.Name(item_temp.name, span=span)
        marker = ast.Pass(span=span)
        enclosing_body = (
            ast.Assignment([node.target], item_name, span=span),
            *node.body,
        )
        function.drive_iterable(
            start,
            next_,
            genexpr,
            lambda elem, s: [
                ast.Assignment([item_name], elem, span=s),
                marker,
            ],
            node,
            iteration=iteration,
            enclosing_statement=marker,
            enclosing_body=enclosing_body,
        )

    def for_iteration(self, current, next_, node, iteration: _ForIteration):
        """Consume prepared iterator ownership through the shared for loop."""
        function = self.function
        iterator = iteration.iterator
        candidate, callee, arguments = (
            iteration.candidate,
            iteration.callee,
            iteration.arguments,
        )
        generic, range_done = function.block(), function.block()
        if callee is not None:
            state = RangeState(
                *_range_interval(candidate),
                tuple(arguments),
                step_sign=1 if len(arguments) <= 2 else 0,
            )
            range_test, range_body = function.block(), function.block()
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, callee, candidate, final=True)
            following, iterator_value = Ctran(), Lvar()
            function.ref(ctran, following, iterator_value, iterator, node)
            ctran, accepted = Ctran(), Lvar()
            function.known(
                following,
                ctran,
                accepted,
                fndb.RANGE_PREPARE,
                (loaded, iterator_value),
                node,
                state,
            )
            function.if_node(ctran, accepted, range_test, generic, node)
            range_else = function.block()
            ctran, available = Ctran(), Lvar()
            function.known(
                range_test.start,
                ctran,
                available,
                fndb.RANGE_AVAILABLE,
                (),
                node,
                state,
            )
            # Break skips else; exhaustion runs it.
            function.if_node(ctran, available, range_body, range_else, node)
            ctran, item = Ctran(), Lvar()
            function.known(
                range_body.start, ctran, item, fndb.RANGE_ADVANCE, (), node, state
            )
            following = Ctran()
            node.target.accept(_AssignmentTarget(function, item, ctran, following))
            with function.with_loop(
                LoopTarget(range_test, range_done, function.lexenv.cleanup)
            ):
                frontier = function.convert_body(following, node.body)
            if frontier is not None:
                function.jump(frontier, range_test)
            frontier = function.convert_body(
                linked(range_else.start, range_else, "start"), node.else_body
            )
            if frontier is not None:
                function.jump(frontier, range_done)
        else:
            function.jump(current, generic)
        test, loop, exhausted, done = (
            function.block(),
            function.block(),
            function.block(),
            function.block(),
        )
        failed, finished, else_block = (
            function.block(),
            function.block(),
            function.block(),
        )
        function.jump(linked(generic.start, generic, "start"), test)
        outer = function.lexenv.cleanup
        region = Cleanup(CleanupKind.HANDLER_REGION, target=exhausted, parent=outer)
        with function.with_cleanup(region):
            ctran, iterator_value = Ctran(), Lvar()
            function.ref(test.start, ctran, iterator_value, iterator, node)
            following, available = Ctran(), Lvar()
            function.known(
                ctran,
                following,
                available,
                fndb.SEQUENCE_AVAILABLE,
                (iterator_value,),
                node,
            )
            function.if_node(following, available, loop, else_block, node)
        ctran, dummy = Ctran(), Lvar()
        function.constant(loop.start, ctran, dummy, ast.NoneLiteral(None))
        following, item = Ctran(), Lvar()
        function.protocol(ctran, following, item, "next_item", (dummy,), node, iterator)
        assigned = Ctran()
        node.target.accept(_AssignmentTarget(function, item, following, assigned))
        with function.with_loop(LoopTarget(test, done, function.lexenv.cleanup)):
            frontier = function.convert_body(assigned, node.body)
        if frontier is not None:
            function.jump(frontier, test)
        ctran, caught = Ctran(), Lvar()
        function.known(exhausted.start, ctran, caught, fndb.CATCH_EXCEPTION, (), node)
        exception = function.temporary()
        following = Ctran()
        function.set_var(ctran, following, exception, caught, node)
        ctran, raised = Ctran(), Lvar()
        function.ref(following, ctran, raised, exception, node)
        following, matches = Ctran(), Lvar()
        function.protocol(
            ctran, following, matches, "iteration_exhausted", (raised,), node
        )
        function.if_node(following, matches, finished, failed, node)
        ctran, value = Ctran(), Lvar()
        function.ref(failed.start, ctran, value, exception, node, final=True)
        self.terminal(ctran, fndb.RESUME_EXCEPTION, (value,), node)
        ctran = Ctran()
        function.discard(finished.start, ctran, exception.name, node)
        function.jump(ctran, else_block)
        frontier = function.convert_body(
            linked(else_block.start, else_block, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)
        function.jump(linked(done.start, done, "start"), range_done)
        function.discard(range_done.start, next_, iterator.name, node)

    def visit_While(self, node):
        function = self.function
        start, next_, _ = self.frame
        test, loop, other, done = (
            function.block(),
            function.block(),
            function.block(),
            function.block(next_),
        )
        function.jump(start, test)
        function.condition(test.start, node.condition, loop, other)
        with function.with_loop(LoopTarget(test, done, function.lexenv.cleanup)):
            frontier = function.convert_body(
                linked(loop.start, loop, "start"), node.body
            )
        if frontier is not None:
            function.jump(frontier, test)
        frontier = function.convert_body(
            linked(other.start, other, "start"), node.else_body
        )
        if frontier is not None:
            function.jump(frontier, done)

    def visit_Break(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'break' outside loop", node)
        target = function.lexenv.loops[-1]
        function.jump(start, target.break_block)

    def visit_Continue(self, node):
        function = self.function
        start, _, _ = self.frame
        if not function.lexenv.loops:
            function.error("'continue' outside loop", node)
        target = function.lexenv.loops[-1]
        function.jump(start, target.continue_block)

    def visit_Return(self, node):
        function = self.function
        start, _, _ = self.frame
        if function.is_module:
            function.error("'return' outside function", node)
        lambda_ = function.lambda_
        return_ = linked(lambda_.return_, lambda_, "return")
        result = return_.result
        annotation = lambda_.signature.implementation_return_annotation
        ctran = Ctran()
        delivered = Lvar() if annotation is not None else result
        if node.value is None:
            function.constant(start, ctran, delivered, ast.NoneLiteral(None))
        else:
            function.convert(start, ctran, delivered, node.value)
        if annotation is not None:
            following = Ctran()
            function.cast(
                ctran,
                following,
                result,
                delivered,
                annotation,
                node,
                declaration=("return", lambda_),
            )
            ctran = following
        function.jump(ctran, return_.block)

    def visit_FunctionDefinition(self, node):
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        converter.reserved(node.name, node)
        if node.type_parameters:
            self.generic_definition(node, self.generic_function(node))
            return
        scope = function.class_scopes[-1] if function.class_scopes else None
        if scope is None and function.annotation_scope:
            scope = function.defining_class
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        lambda_ = converter.function(
            node,
            scope.policy if scope is not None else function.lambda_.policy,
            owner_class=scope.native_id if scope is not None else None,
            parent=function,
            defining_class=scope,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(current, ctran, created, lambda_, node, node.name)
        ctran, created = function.apply_decorators(
            ctran,
            held,
            created,
            decorators,
            final_steps=function.final_decorator_steps(node),
        )
        function.store_function(ctran, next_, node, created)

    def visit_ClassDefinition(self, node):
        """Python's class statement: decorators and bases are evaluated, the
        body runs against a fresh namespace, and the class is built from that
        namespace (CPython's ``__build_class__``). SBCL's ``defclass`` likewise
        expands to a load-time ``%defclass`` call carrying the slot metadata;
        Mule additionally runs arbitrary body statements, inline in the
        enclosing function, with the class scope's name resolution."""
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        converter.reserved(node.name, node)
        if node.type_parameters:
            self.generic_definition(node, self.generic_class(node))
            return
        outer = function.class_scopes[-1] if function.class_scopes else None
        policy = converter.policy(
            node.decorators,
            outer.policy if outer is not None else function.lambda_.policy,
        )
        positional = [
            argument
            for argument in node.arguments
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ]
        keywords = [
            argument
            for argument in node.arguments
            if argument.kind
            in (ast.ArgumentKind.KEYWORD, ast.ArgumentKind.DOUBLE_STARRED)
        ]
        identity = converter.class_identity(node)
        native = _native_class_body(function, node, policy)
        body = node.body
        docstring = None
        if body and body[0].accept(_Docstring()):
            docstring, body = _documentation(body), body[1:]

        # Decorators, then the bases and keywords in source order, are
        # evaluated before the namespace is prepared and the body runs.
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        span = node.span
        for argument in positional:
            if argument.kind == ast.ArgumentKind.STARRED:
                function.error(
                    "starred class bases are not supported yet",
                    argument,
                    unsupported=True,
                )
        bases, class_keywords = function.temporary(), function.temporary()
        ctran, value = function.value(
            current, ast.Tuple([argument.value for argument in positional], span=span)
        )
        current = Ctran()
        function.set_var(ctran, current, bases, value, node)
        if keywords:
            current = function.keyword_dict(current, class_keywords, keywords, node)
        else:
            ctran, value = Ctran(), Lvar()
            function.constant(current, ctran, value, ast.NoneLiteral(None, span=span))
            current = Ctran()
            function.set_var(ctran, current, class_keywords, value, node)

        namespace = function.temporary()
        operands = []
        for var in (bases, class_keywords):
            ctran, value = Ctran(), Lvar()
            function.ref(current, ctran, value, var, node)
            current = ctran
            operands.append(value)
        ctran, created = Ctran(), Lvar()
        function.known(
            current,
            ctran,
            created,
            fndb.CLASS_NAMESPACE,
            (*operands, ast.TextLiteral(node.name, span=span)),
            node,
        )
        current = Ctran()
        function.set_var(ctran, current, namespace, created, node)
        bindings = _ClassBindings()
        for statement in body:
            statement.accept(bindings)
        scope = ClassScope(
            function,
            namespace,
            identity,
            bindings.bound - bindings.globals - bindings.nonlocals,
            bindings.globals,
            policy,
            identity if native is not None else None,
            function.qualified(node.name),
        )
        # Only an actual leading docstring enters the class-body namespace.
        # type.__new__ supplies None later, after checking slot conflicts.
        # CPython's codegen_class_body stores __firstlineno__ before the body:
        # the first decorator's line for a decorated class, else the class's.
        first = node.decorators[0] if node.decorators else node
        initial = (
            ("__module__", ast.Name("__name__", span=node.span)),
            ("__qualname__", ast.TextLiteral(scope.qualname, span=node.span)),
            (
                "__firstlineno__",
                ast.IntegerLiteral(first.span.start.line, span=node.span),
            ),
        )
        if docstring is not None:
            initial += (("__doc__", docstring),)
        for name, expression in initial:
            if name == "__module__":
                ctran, value = Ctran(), Lvar()
                function.ref(
                    current, ctran, value, converter.global_var("__name__"), node
                )
            else:
                ctran, value = function.value(current, expression)
            current = Ctran()
            function.class_namespace_operation(
                ctran, current, None, fndb.CLASS_STORE, scope, name, (value,), node
            )

        fields = dict.fromkeys(native.fields if native is not None else ())
        # Names a literal __slots__ declares are instance fields, even when
        # only __new__ or code outside the class assigns them.
        for statement in body:
            fields.update(dict.fromkeys(statement.accept(_SlotNames())))
        for method in _ClassMethods.of(body):
            if not method.parameters:
                continue
            receiver = method.parameters[0]
            if receiver.kind == ast.ParameterKind.VAR_POSITIONAL:
                continue
            collector = _ReceiverFields(receiver.name, fields, declaration=native)
            for statement in method.body:
                statement.accept(collector)
        if native is not None:
            converter.class_fields[identity] = native.fields

        function.class_scopes.append(scope)
        try:
            frontier = function.convert_body(current, body)
        finally:
            function.class_scopes.pop()
        if frontier is None:
            return
        declarations = tuple(
            declaration
            for statement in body
            for declaration in statement.accept(_NamedAnnotations())
        )
        if declarations and "__annotations__" not in bindings.bound:
            if native is None and "__annotate__" not in bindings.bound:
                from .annotation_runtime import AnnotationThunk

                annotate = AnnotationThunk.from_declarations(
                    converter,
                    node.span,
                    declarations,
                    _RuntimeAnnotation(converter),
                ).build()
                if annotate is not None:
                    thunk = converter.function(
                        annotate,
                        policy,
                        parent=function,
                        defining_class=scope,
                        qualname=f"{scope.qualname}.__annotate__",
                        annotation_scope=True,
                    )
                    ctran, callback = Ctran(), Lvar()
                    function.define_function(
                        frontier, ctran, callback, thunk, annotate, "__annotate__"
                    )
                    frontier = Ctran()
                    function.class_namespace_operation(
                        ctran,
                        frontier,
                        None,
                        fndb.CLASS_STORE,
                        scope,
                        "__annotate__",
                        (callback,),
                        node,
                    )
            else:
                # Class annotations are deferred metadata (PEP 649): record
                # each annotation's source text in ``__annotations__`` so
                # runtime consumers such as dataclasses and annotationlib
                # can find the declared names without evaluating anything.
                entries = [
                    ast.DictEntry(
                        ast.TextLiteral(name, span=node.span),
                        ast.TextLiteral(
                            converter.source_text(annotation), span=annotation.span
                        ),
                        span=node.span,
                    )
                    for name, annotation in declarations
                ]
                ctran, annotations = function.value(
                    frontier, ast.Dict(entries, span=node.span)
                )
                frontier = Ctran()
                function.class_namespace_operation(
                    ctran,
                    frontier,
                    None,
                    fndb.CLASS_STORE,
                    scope,
                    "__annotations__",
                    (annotations,),
                    node,
                )
        # CPython stores __static_attributes__ after the body (and its
        # deferred annotations): the sorted names assigned as ``self.NAME``.
        ctran, static_attributes = function.value(
            frontier,
            ast.Tuple(
                [
                    ast.TextLiteral(attribute, span=node.span)
                    for attribute in _StaticAttributes.of(node)
                ],
                span=node.span,
            ),
        )
        frontier = Ctran()
        function.class_namespace_operation(
            ctran,
            frontier,
            None,
            fndb.CLASS_STORE,
            scope,
            "__static_attributes__",
            (static_attributes,),
            node,
        )
        operands = []
        for var in (bases, namespace, class_keywords):
            ctran, value = Ctran(), Lvar()
            function.ref(
                frontier,
                ctran,
                value,
                var,
                node,
                final=var is not namespace or not scope.annotation_namespace_used,
            )
            frontier = ctran
            operands.append(value)
        resolved_field_annotations = []
        if native is not None:
            for fname, (contract, _) in native.fields.items():
                resolved_field_annotations.append((fname, contract))
        else:
            for fname, annotation in declarations:
                if fname in (
                    "__dict__",
                    "__slots__",
                    "__class__",
                    "__match_args__",
                    "__annotations__",
                    "__weakref__",
                ):
                    continue
                contract = converter.declared_contract(annotation, function)
                if contract is not None:
                    resolved_field_annotations.append((fname, contract))
        info = ClassInfo(
            node.name,
            len(positional),
            tuple(fields),
            tuple(resolved_field_annotations),
            policy,
            converter.location(node),
            identity,
            identity if native is not None else None,
            declarations,
            bool(keywords or decorators),
        )
        following, created = Ctran(), Lvar()
        function.known(ctran, following, created, fndb.MAKE_CLASS, operands, node, info)
        if scope.annotation_namespace_used:
            klass = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, klass, created, node)
            following, value = Ctran(), Lvar()
            function.ref(ctran, following, value, klass, node)
            ctran, live = Ctran(), Lvar()
            function.known(
                following,
                ctran,
                live,
                fndb.GETATTR,
                (value,),
                node,
                AttributeInfo("__dict__", converter.location(node)),
            )
            following = Ctran()
            function.set_var(ctran, following, namespace, live, node)
            ctran, created = Ctran(), Lvar()
            function.ref(following, ctran, created, klass, node, final=True)
            following = ctran
        if scope.cell is not None:
            # Methods using super() or __class__ see the finished class.
            klass = function.temporary()
            ctran = Ctran()
            function.set_var(following, ctran, klass, created, node)
            following, value = Ctran(), Lvar()
            function.ref(ctran, following, value, klass, node)
            ctran = Ctran()
            function.set_var(following, ctran, scope.cell, value, node)
            following, created = Ctran(), Lvar()
            function.ref(ctran, following, created, klass, node, final=True)
        ctran, created = function.apply_decorators(following, held, created, decorators)
        function.store(ctran, next_, node.name, created, node)

    def generic_definition(self, node, definition):
        """A PEP 695 generic function or class. As in CPython, a hidden
        annotation scope creates the type parameters, runs ``definition``
        (which defines the function or class and records them as
        ``__type_params__``), and returns it; decorators apply outside."""
        function = self.function
        start, next_, _ = self.frame
        converter = function.converter
        span = node.span
        statements = [
            *_annotation_scope_body(node, span),
            *definition,
            ast.Return(ast.Name(node.name, span=span), span=span),
        ]
        scope = ast.FunctionDefinition(
            f"<generic parameters of {node.name}>", [], statements, span=span
        )
        decorators = converter.runtime_decorators(node.decorators)
        current, held = function.evaluate_decorators(start, decorators)
        outer = (
            function.class_scopes[-1]
            if function.class_scopes
            else function.defining_class
        )
        lambda_ = converter.function(
            scope,
            function.lambda_.policy,
            parent=function,
            defining_class=outer,
            annotation_scope=True,
        )
        ctran, created = Ctran(), Lvar()
        function.define_function(current, ctran, created, lambda_, scope, scope.name)
        following, result = Ctran(), Lvar()
        function.full_call(
            ctran,
            following,
            result,
            created,
            (),
            node,
            CallInfo((), converter.location(node)),
        )
        ctran, result = function.apply_decorators(
            following,
            held,
            result,
            decorators,
            final_steps=function.final_decorator_steps(node),
        )
        function.store_function(ctran, next_, node, result)

    def generic_function(self, node):
        """The generic function's definition inside its annotation scope."""
        span = node.span
        return [
            replace(node, type_parameters=[], decorators=[]),
            ast.Assignment(
                [
                    ast.Attribute(
                        ast.Name(node.name, span=span), "__type_params__", span=span
                    )
                ],
                _type_parameter_tuple(node, span),
                span=span,
            ),
        ]

    def generic_class(self, node):
        """The generic class's definition inside its annotation scope; the
        class also derives from ``Generic[...]``."""
        span = node.span
        positional = [
            argument
            for argument in node.arguments
            if argument.kind in (ast.ArgumentKind.POSITIONAL, ast.ArgumentKind.STARRED)
        ]
        keywords = [
            argument for argument in node.arguments if argument not in positional
        ]
        generic = ast.CallArgument(
            ast.Subscript(
                ast.Attribute(ast.Name("$typing", span=span), "Generic", span=span),
                _type_parameter_tuple(node, span, unpack=True),
                span=span,
            ),
            span=span,
        )
        body = list(node.body)
        offset = 1 if body and body[0].accept(_Docstring()) else 0
        body.insert(
            offset,
            ast.Assignment(
                [ast.Name("__type_params__", span=span)],
                _type_parameter_tuple(node, span),
                span=span,
            ),
        )
        return [
            replace(
                node,
                type_parameters=[],
                decorators=[],
                arguments=[*positional, generic, *keywords],
                body=body,
            )
        ]


def _async_with(function, node):
    """``async with``, expanded as CPython executes it: await ``__aenter__``,
    run the body, and await ``__aexit__`` with the exception that ends the
    body (suppressing it when the result is true) or with Nones otherwise."""
    span = node.span
    item, rest = node.items[0], node.items[1:]
    body = list(node.body) if not rest else [replace(node, items=rest)]
    manager, enter_, exit_, value, error, handled = (
        function.temporary().name for _ in range(6)
    )

    def name(identifier):
        return ast.Name(identifier, span=span)

    def call(callee, *arguments):
        return ast.Call(
            callee,
            [ast.CallArgument(argument, span=span) for argument in arguments],
            span=span,
        )

    def assign(target, value):
        return ast.Assignment([target], value, span=span)

    none = ast.NoneLiteral(None, span=span)
    if item.target is not None:
        body.insert(0, assign(item.target, name(value)))
    exit_with_error = ast.Await(
        call(
            name(exit_),
            ast.Attribute(name(error), "__class__", span=span),
            name(error),
            ast.Attribute(name(error), "__traceback__", span=span),
        ),
        span=span,
    )
    return [
        assign(name(manager), item.context),
        ast.Try(
            [
                assign(
                    name(exit_), ast.Attribute(name(manager), "__aexit__", span=span)
                ),
                assign(
                    name(enter_), ast.Attribute(name(manager), "__aenter__", span=span)
                ),
            ],
            [
                ast.ExceptionHandler(
                    name("AttributeError"),
                    None,
                    [
                        ast.Raise(
                            call(
                                name("TypeError"),
                                ast.TextLiteral(
                                    "object does not support the asynchronous context manager protocol",
                                    span=span,
                                ),
                            ),
                            span=span,
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
        assign(
            name(value),
            ast.Await(call(name(enter_)), span=span),
        ),
        assign(name(handled), ast.BooleanLiteral(False, span=span)),
        ast.Try(
            body,
            [
                ast.ExceptionHandler(
                    name("BaseException"),
                    error,
                    [
                        assign(name(handled), ast.BooleanLiteral(True, span=span)),
                        ast.If(
                            ast.Unary(
                                ast.UnaryOperator.NOT, exit_with_error, span=span
                            ),
                            [ast.Raise(span=span)],
                            span=span,
                        ),
                    ],
                    span=span,
                )
            ],
            finally_body=[
                ast.If(
                    ast.Unary(ast.UnaryOperator.NOT, name(handled), span=span),
                    [
                        ast.ExpressionStatement(
                            ast.Await(call(name(exit_), none, none, none), span=span),
                            span=span,
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
    ]


def _async_for(function, node):
    """``async for``, expanded: await ``__anext__`` until it raises
    StopAsyncIteration, then run the ``else`` clause unless the body broke out."""
    span = node.span
    iterable, aiter_, iterator, anext_, value, exhausted = (
        function.temporary().name for _ in range(6)
    )

    def name(identifier):
        return ast.Name(identifier, span=span)

    def assign(target, value):
        return ast.Assignment([target], value, span=span)

    def type_error(message):
        return ast.Raise(
            ast.Call(
                name("TypeError"),
                [ast.CallArgument(ast.TextLiteral(message, span=span), span=span)],
                span=span,
            ),
            span=span,
        )

    loop = ast.While(
        ast.BooleanLiteral(True, span=span),
        [
            ast.Try(
                [
                    assign(
                        name(anext_),
                        ast.Attribute(name(iterator), "__anext__", span=span),
                    )
                ],
                [
                    ast.ExceptionHandler(
                        name("AttributeError"),
                        None,
                        [
                            type_error(
                                "'async for' received an object from __aiter__ that does not implement __anext__"
                            )
                        ],
                        span=span,
                    )
                ],
                span=span,
            ),
            ast.Try(
                [
                    assign(
                        name(value),
                        ast.Await(
                            ast.Call(name(anext_), span=span),
                            span=span,
                        ),
                    )
                ],
                [
                    ast.ExceptionHandler(
                        name("StopAsyncIteration"),
                        None,
                        [
                            assign(
                                name(exhausted), ast.BooleanLiteral(True, span=span)
                            ),
                            ast.Break(span=span),
                        ],
                        span=span,
                    )
                ],
                span=span,
            ),
            assign(node.target, name(value)),
            *node.body,
        ],
        span=span,
    )
    statements = [
        assign(name(iterable), node.iterable),
        ast.Try(
            [
                assign(
                    name(aiter_),
                    ast.Attribute(name(iterable), "__aiter__", span=span),
                )
            ],
            [
                ast.ExceptionHandler(
                    name("AttributeError"),
                    None,
                    [
                        type_error(
                            "'async for' requires an object with __aiter__ method"
                        )
                    ],
                    span=span,
                )
            ],
            span=span,
        ),
        assign(
            name(iterator),
            ast.Call(name(aiter_), span=span),
        ),
        assign(name(exhausted), ast.BooleanLiteral(False, span=span)),
        loop,
    ]
    if node.else_body:
        statements.append(ast.If(name(exhausted), list(node.else_body), span=span))
    return statements


_TYPE_PARAMETER_KINDS = {
    ast.TypeParameterKind.TYPE_VAR: "TypeVar",
    ast.TypeParameterKind.PARAM_SPEC: "ParamSpec",
    ast.TypeParameterKind.TYPE_VAR_TUPLE: "TypeVarTuple",
}


def _annotation_scope_body(node, span):
    """Statements creating ``node``'s type parameters, each lazily evaluating
    its bound, constraints and default, and the names they bind."""
    statements = [
        ast.Import([ast.ImportItem("_typing", "$typing", span=span)], span=span)
    ]

    def lazy(expression):
        if expression is None:
            return ast.NoneLiteral(None, span=span)
        from .annotation_runtime import _ANNOTATION_VALUE

        return ast.Lambda([], expression.accept(_ANNOTATION_VALUE), span=span)

    for parameter in node.type_parameters:
        constraints = (
            ast.Tuple(list(parameter.constraints), span=span)
            if parameter.constraints
            else None
        )
        create = ast.Call(
            ast.Attribute(ast.Name("$typing", span=span), "_type_parameter", span=span),
            [
                ast.CallArgument(argument, span=span)
                for argument in (
                    ast.TextLiteral(_TYPE_PARAMETER_KINDS[parameter.kind], span=span),
                    ast.TextLiteral(parameter.name, span=span),
                    lazy(parameter.bound),
                    lazy(constraints),
                    lazy(parameter.default),
                )
            ],
            span=span,
        )
        statements.append(
            ast.Assignment([ast.Name(parameter.name, span=span)], create, span=span)
        )
    return statements


def _type_parameter_tuple(node, span, *, unpack=False):
    return ast.Tuple(
        [
            ast.Unpack(ast.Name(parameter.name, span=span), span=span)
            if unpack and parameter.kind == ast.TypeParameterKind.TYPE_VAR_TUPLE
            else ast.Name(parameter.name, span=span)
            for parameter in node.type_parameters
        ],
        span=span,
    )


def _native_class_body(function, node, policy):
    """The validated body of a typed native class, or None for an ordinary
    class. A class requests the native layout with ``optimize(speed=3)`` and
    annotated declarations; its body is then restricted to a docstring,
    methods, bare ``mule_types.int64`` declarations and ``pass``."""
    requested = (
        policy.speed == 3
        and any(decorator.accept(_NativeClassOptIn()) for decorator in node.decorators)
        and any(statement.accept(_AnnotatedDeclaration()) for statement in node.body)
    )
    if not requested:
        return None
    body = _ClassBody(function)
    for index, statement in enumerate(node.body):
        if index == 0 and statement.accept(_Docstring()):
            continue
        statement.accept(body)
    if not body.fields or not body.native_eligible:
        return None
    if node.arguments:
        function.error("typed native classes cannot have bases", node, unsupported=True)
    if not any(method.name == "__init__" for method in body.methods):
        function.error("typed native classes require __init__", node, unsupported=True)
    for method in body.methods:
        if not method.parameters:
            function.error("instance methods require a receiver parameter", method)
        if function.converter.runtime_decorators(method.decorators):
            function.error(
                "typed native class methods cannot have runtime decorators",
                method,
                unsupported=True,
            )
        body.validate_method(method)
    return body


class _RangeName(ast.Visitor):
    def visit_Name(self, node):
        return node.identifier == "range"


class _RangeCandidate(ast.Visitor):
    def visit_Call(self, node):
        if (
            node.function.accept(_RangeName())
            and 1 <= len(node.arguments) <= 3
            and all(
                argument.kind == ast.ArgumentKind.POSITIONAL
                for argument in node.arguments
            )
        ):
            return node


class _IntegerConstant(ast.Visitor):
    def visit_IntegerLiteral(self, node):
        return node.value

    def visit_Unary(self, node):
        value = node.operand.accept(self)
        if value is not None:
            if node.operator == ast.UnaryOperator.NEGATIVE:
                return -value
            if node.operator == ast.UnaryOperator.POSITIVE:
                return value


def _range_interval(candidate):
    from .ctype import FIXNUM_MAX, FIXNUM_MIN

    values = [
        argument.value.accept(_IntegerConstant()) for argument in candidate.arguments
    ]
    if all(value is not None for value in values):
        if len(values) == 1:
            start, stop, step = 0, values[0], 1
        elif len(values) == 2:
            start, stop, step = values[0], values[1], 1
        else:
            start, stop, step = values
        if all(FIXNUM_MIN <= value <= FIXNUM_MAX for value in (start, stop, step)):
            if step > 0 and start < stop:
                return start, stop - 1
            if step < 0 and start > stop:
                return stop + 1, start
    return FIXNUM_MIN, FIXNUM_MAX


class _AssignmentTarget(ast.Visitor):
    def __init__(self, function, value: Lvar, start, next_):
        self.function = function
        self.value = value
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def visit_Subscript(self, node):
        function = self.function
        ctran, receiver = function.value(self.start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(
            ctran, self.next, None, "setitem", (receiver, index, self.value), node
        )

    def visit_Tuple(self, node):
        self.unpack(node)

    def visit_List(self, node):
        self.unpack(node)

    def unpack(self, node):
        function = self.function
        starred = _star_positions(node.elements)
        if len(starred) > 1:
            function.error("multiple starred expressions in assignment", node)
        if starred:
            # ``a, *b, c = value``: the protocol returns the leading items,
            # a list of the middle ones and the trailing items.
            before = starred[0]
            after = len(node.elements) - before - 1
            ctran, low = Ctran(), Lvar()
            function.constant(self.start, ctran, low, ast.IntegerLiteral(before))
            following, high = Ctran(), Lvar()
            function.constant(ctran, following, high, ast.IntegerLiteral(after))
            ctran, values = Ctran(), Lvar()
            function.protocol(
                following,
                ctran,
                values,
                "unpack_starred",
                (self.value, low, high),
                node,
            )
            following = ctran
        else:
            ctran, count = Ctran(), Lvar()
            function.constant(
                self.start, ctran, count, ast.IntegerLiteral(len(node.elements))
            )
            following, values = Ctran(), Lvar()
            function.protocol(
                ctran, following, values, "unpack", (self.value, count), node
            )
        temporary = function.temporary(expression=True)
        current = Ctran()
        function.set_var(following, current, temporary, values, node)
        for index, target in enumerate(node.elements):
            ctran, loaded = Ctran(), Lvar()
            function.ref(current, ctran, loaded, temporary, node)
            following, position = Ctran(), Lvar()
            function.constant(ctran, following, position, ast.IntegerLiteral(index))
            ctran, element = Ctran(), Lvar()
            function.known(
                following, ctran, element, fndb.UNPACK_REF, (loaded, position), node
            )
            current = Ctran()
            target.accept(_UnpackedTarget(function, element, ctran, current))
        function.discard(current, self.next, temporary.name, node)

    def visit_Name(self, node):
        self.function.store(self.start, self.next, node.identifier, self.value, node)

    def visit_Attribute(self, node):
        function = self.function
        ctran, receiver = function.value(self.start, node.value)
        value = self.value
        owner = function.lambda_.owner_class
        if owner is not None and node.value.accept(
            _ReceiverName(function.lambda_.parameters[0])
        ):
            declaration = function.converter.class_fields[owner].get(node.attribute)
            if declaration is not None:
                following, checked = Ctran(), Lvar()
                function.cast(ctran, following, checked, value, declaration[0], node)
                ctran, value = following, checked
        function.known(
            ctran,
            self.next,
            None,
            fndb.SETATTR,
            (receiver, value),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )


class _UnpackedTarget(_AssignmentTarget):
    """An element of a target list, where ``*target`` receives the list of
    the items no other element takes."""

    def visit_Unpack(self, node):
        node.value.accept(
            _AssignmentTarget(self.function, self.value, self.start, self.next)
        )


class _DeleteTarget(ast.Visitor):
    """``del name``: a read establishes the binding exists, then it is unbound."""

    def __init__(self, function, start, next_):
        self.function = function
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier, attribute and subscript deletion targets are supported",
            node,
            unsupported=True,
        )

    def visit_Subscript(self, node):
        function = self.function
        ctran, value = function.value(self.start, node.value)
        ctran, index = function.value(ctran, node.index)
        function.protocol(ctran, self.next, None, "delitem", (value, index), node)

    def visit_Attribute(self, node):
        function = self.function
        ctran, value = function.value(self.start, node.value)
        following, name = Ctran(), Lvar()
        function.constant(ctran, following, name, ast.TextLiteral(node.attribute))
        function.protocol(
            following, self.next, None, "delete_attribute", (value, name), node
        )

    def visit_Tuple(self, node):
        if not node.elements:
            self.function.flow(self.start, self.next)
            return
        current = self.start
        for index, element in enumerate(node.elements):
            following = self.next if index == len(node.elements) - 1 else Ctran()
            element.accept(_DeleteTarget(self.function, current, following))
            current = following

    visit_List = visit_Tuple

    def visit_Name(self, node):
        function = self.function
        function.converter.reserved(node.identifier, node)
        if function.class_binding(node.identifier) is not None:
            # Deleting from a class namespace checks only that namespace.
            function.unbind(self.start, self.next, node.identifier, node)
            return
        var = function.variable(node.identifier)
        if var.accept(_IS_GLOBAL_VAR):
            function.set_var(self.start, self.next, var, None, node)
            return
        ctran = Ctran()
        function.ref(self.start, ctran, None, var, node)
        function.set_var(ctran, self.next, var, None, node)


class _AnnotationTarget(ast.Visitor):
    def __init__(self, function, assignment, start, next_):
        self.function = function
        self.assignment = assignment
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        if self.assignment.value is not None:
            ctran, value = self.function.value(self.start, self.assignment.value)
            node.accept(_AssignmentTarget(self.function, value, ctran, self.next))

    def visit_Attribute(self, node):
        if self.assignment.value is not None:
            self.visit_Node(node)
        else:
            self.function.convert(self.start, self.next, None, node.value)

    def visit_Subscript(self, node):
        if self.assignment.value is not None:
            self.visit_Node(node)
        else:
            ctran = Ctran()
            self.function.convert(self.start, ctran, None, node.value)
            node.index.accept(_AnnotationIndex(self.function, ctran, self.next))

    def visit_Name(self, node):
        function = self.function
        converter = function.converter
        converter.reserved(node.identifier, node)
        if function.class_binding(node.identifier) is not None:
            # Class variable annotations are deferred metadata, not contracts.
            if self.assignment.value is not None:
                ctran, value = function.value(self.start, self.assignment.value)
                function.store(ctran, self.next, node.identifier, value, node)
            return
        function.lambda_.annotation_expressions[node.identifier] = (
            self.assignment.annotation
        )
        contract = converter.annotation(self.assignment.annotation, function)
        annotations = (
            function.annotations
            if function.local(node.identifier)
            else converter.module_annotations
        )
        if contract is not None:
            previous = annotations.get(node.identifier)
            if previous is not None and previous[0] is not contract:
                function.error(
                    f"conflicting annotation for '{node.identifier}'", self.assignment
                )
            annotations[node.identifier] = (contract, self.assignment)
            if function.local(node.identifier):
                function.lambda_.local_annotations[node.identifier] = contract
        elif function.local(node.identifier):
            declared = converter.declared_contract(self.assignment.annotation, function)
            if (
                declared is not None
                and node.identifier not in function.lambda_.local_annotations
            ):
                function.lambda_.local_annotations[node.identifier] = declared
        if self.assignment.value is not None:
            ctran, value = function.value(self.start, self.assignment.value)
            function.store(ctran, self.next, node.identifier, value, node)


class _AnnotationIndex(ast.Visitor):
    def __init__(self, function, start, next_):
        self.function = function
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.convert(self.start, self.next, None, node)

    def visit_Slice(self, node):
        current = self.start
        bounds = list(node.children())
        if not bounds:
            self.function.flow(current, self.next)
            return
        for index, bound in enumerate(bounds):
            last = index == len(bounds) - 1
            ctran = self.next if last else Ctran()
            self.function.convert(current, ctran, None, bound)
            current = ctran


class _AugmentedTarget(ast.Visitor):
    def __init__(self, function, assignment, start, next_):
        self.function = function
        self.assignment = assignment
        self.start = start
        self.next = next_

    def visit_Node(self, node):
        self.function.error(
            "only identifier and attribute assignment targets are supported yet",
            node,
            unsupported=True,
        )

    def binary(self, start, left: Lvar, reuse_local=None) -> tuple[Ctran, Lvar]:
        function = self.function
        ctran, right = function.value(start, self.assignment.value)
        following, result = Ctran(), Lvar()
        function.known(
            ctran,
            following,
            result,
            fndb.INPLACE_BINARY[self.assignment.operator],
            (left, right),
            self.assignment,
            InplaceInfo(reuse_local),
        )
        return following, result

    def visit_Subscript(self, node):
        function = self.function
        receiver, index = function.temporary(), function.temporary()
        ctran, value = function.value(self.start, node.value)
        following = Ctran()
        function.set_var(ctran, following, receiver, value, node.value)
        ctran, value = function.value(following, node.index)
        following = Ctran()
        function.set_var(ctran, following, index, value, node.index)
        ctran, loaded = Ctran(), Lvar()
        function.ref(following, ctran, loaded, receiver, node)
        following, position = Ctran(), Lvar()
        function.ref(ctran, following, position, index, node)
        ctran, left = Ctran(), Lvar()
        function.protocol(following, ctran, left, "getitem", (loaded, position), node)
        ctran, result = self.binary(ctran, left)
        following, loaded = Ctran(), Lvar()
        function.ref(ctran, following, loaded, receiver, node, final=True)
        ctran, position = Ctran(), Lvar()
        function.ref(following, ctran, position, index, node, final=True)
        function.protocol(
            ctran, self.next, None, "setitem", (loaded, position, result), node
        )

    def visit_Name(self, node):
        function = self.function
        ctran, left = Ctran(), Lvar()
        function.load(self.start, ctran, left, node.identifier, node)
        reuse = (
            node.identifier
            if function.local(node.identifier)
            and (
                function.annotation_for(node.identifier) is None
                or function.lexenv.policy.safety == 0
            )
            else None
        )
        ctran, result = self.binary(ctran, left, reuse)
        function.store(ctran, self.next, node.identifier, result, node)

    def visit_Attribute(self, node):
        function = self.function
        temporary = function.temporary()
        ctran, value = function.value(self.start, node.value)
        following = Ctran()
        function.set_var(ctran, following, temporary, value, node.value)
        ctran, receiver = Ctran(), Lvar()
        function.ref(following, ctran, receiver, temporary, node)
        following, left = Ctran(), Lvar()
        function.known(
            ctran,
            following,
            left,
            fndb.GETATTR,
            (receiver,),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )
        ctran, result = self.binary(following, left)
        owner = function.lambda_.owner_class
        if owner is not None and node.value.accept(
            _ReceiverName(function.lambda_.parameters[0])
        ):
            declaration = function.converter.class_fields[owner].get(node.attribute)
            if declaration is not None:
                following, checked = Ctran(), Lvar()
                function.cast(ctran, following, checked, result, declaration[0], node)
                ctran, result = following, checked
        following, receiver = Ctran(), Lvar()
        function.ref(ctran, following, receiver, temporary, node, final=True)
        function.known(
            following,
            self.next,
            None,
            fndb.SETATTR,
            (receiver, result),
            node,
            AttributeInfo(node.attribute, function.converter.location(node)),
        )
