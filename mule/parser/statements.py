"""Python statement grammar over Mule's lexer and expression parser."""

from mule import ast

_AUGMENTED = {operator.value + "=": operator for operator in ast.BinaryOperator}
_SIMPLE_END = {"NEWLINE", "EOF", "DEDENT"}


class StatementParserMixin:
    def statements(self, stop_kinds=("EOF",)):
        result = []
        while self.current.kind not in stop_kinds:
            if self.current.kind == "NEWLINE":
                self.advance()
                continue
            if self.current.kind in {"INDENT", "DEDENT", "EOF"}:
                self.error("unexpected indentation or end of input")
            result.extend(self.statement())
        return result

    def statement(self):
        text = self.current.text
        if text == "@":
            return [self.decorated_definition()]
        if text == "async" and self.peek().text in {"def", "for", "with"}:
            start = self.advance()
            if self.current.text == "def":
                return [self.function_definition(start=start, is_async=True)]
            if self.current.text == "for":
                return [self.for_statement(start=start, is_async=True)]
            return [self.with_statement(start=start, is_async=True)]
        compound = {
            "if": self.if_statement,
            "while": self.while_statement,
            "for": self.for_statement,
            "with": self.with_statement,
            "try": self.try_statement,
            "def": self.function_definition,
            "class": self.class_definition,
        }
        if text in compound:
            return [compound[text]()]
        if text == "match" and self._starts_match_statement():
            return [self.match_statement()]
        return self.simple_statements()

    def _starts_match_statement(self):
        # A match header requires a colon followed by a physical suite. Scanning
        # only this logical line keeps uses of the soft keyword as a name intact.
        if self.peek().text == "=":
            return False
        depth = 0
        distance = 1
        while True:
            token = self.peek(distance)
            if token.kind in {"NEWLINE", "EOF"}:
                return False
            if token.text in {"(", "[", "{"}:
                depth += 1
            elif token.text in {")", "]", "}"}:
                depth -= 1
            elif (
                not depth
                and token.text == ":"
                and self.peek(distance + 1).kind == "NEWLINE"
            ):
                return True
            distance += 1

    def simple_statements(self):
        result = [self.simple_statement()]
        while self.match(";"):
            if self.current.kind in _SIMPLE_END:
                break
            result.append(self.simple_statement())
        if self.current.kind == "NEWLINE":
            self.advance()
        elif self.current.kind not in {"EOF", "DEDENT"}:
            self.error("expected end of statement")
        return result

    def simple_statement(self):
        start = self.current
        text = start.text
        if text in {"pass", "break", "continue"}:
            self.advance()
            cls = {"pass": ast.Pass, "break": ast.Break, "continue": ast.Continue}[text]
            return self.finish(cls(), start)
        if text == "return":
            self.advance()
            value = None if self._at_simple_end() else self._statement_value()
            return self.finish(ast.Return(value), start)
        if text == "raise":
            self.advance()
            exception = cause = None
            if not self._at_simple_end():
                exception = self.expression()
                if self.match("from"):
                    cause = self.expression()
            return self.finish(ast.Raise(exception, cause), start)
        if text == "assert":
            self.advance()
            condition = self.expression()
            message = self.expression() if self.match(",") else None
            return self.finish(ast.Assert(condition, message), start)
        if text in {"global", "nonlocal"}:
            self.advance()
            names = [self.name_token().text]
            while self.match(","):
                names.append(self.name_token().text)
            cls = ast.Global if text == "global" else ast.Nonlocal
            return self.finish(cls(names), start)
        if text == "del":
            self.advance()
            target = self.expression_list(allow_star=False)
            # A top-level comma list represents several delete operands, while
            # explicitly grouped sequences remain nested target expressions.
            targets = (
                target.elements
                if isinstance(target, ast.Tuple) and not self.is_parenthesized(target)
                else [target]
            )
            for value in targets:
                self.validate_target(value, delete=True)
            return self.finish(ast.Delete(targets), start)
        if text == "import":
            return self.import_statement()
        if text == "from":
            return self.from_import_statement()
        if (
            text == "type"
            and self.peek().kind == "NAME"
            and self.peek(2).text in {"[", "="}
        ):
            return self.type_alias_statement()
        return self.assignment_or_expression()

    def _at_simple_end(self):
        return self.current.kind in _SIMPLE_END or self.current.text == ";"

    def _type_comment(self):
        if self.current.kind != "TYPE_COMMENT":
            return None
        token = self.advance()
        return token.text.removeprefix("#").lstrip().removeprefix("type:").strip()

    def _statement_value(self, *, allow_yield=False):
        value = self.expression_list(allow_yield=allow_yield)
        if isinstance(value, ast.Unpack):
            self.error("starred value requires a comma")
        return value

    def assignment_or_expression(self):
        start = self.current
        target = self.expression_list(allow_yield=True)
        if self.match(":"):
            if not isinstance(target, (ast.Name, ast.Attribute, ast.Subscript)):
                self.error(
                    "only a name, attribute or subscript can be annotated", start
                )
            annotation = self.expression()
            value = self._statement_value(allow_yield=True) if self.match("=") else None
            node = ast.AnnotatedAssignment(
                target, annotation, value, parenthesized=self.is_parenthesized(target)
            )
        elif self.current.text in _AUGMENTED:
            operator = _AUGMENTED[self.advance().text]
            if not isinstance(target, (ast.Name, ast.Attribute, ast.Subscript)):
                self.error("invalid augmented assignment target", start)
            self.validate_target(target)
            node = ast.AugmentedAssignment(
                target, operator, self._statement_value(allow_yield=True)
            )
        elif self.match("="):
            self.validate_target(target)
            targets = [target]
            value = self._statement_value(allow_yield=True)
            while self.match("="):
                self.validate_target(value)
                targets.append(value)
                value = self._statement_value(allow_yield=True)
            node = ast.Assignment(targets, value, self._type_comment())
        else:
            if isinstance(target, ast.Unpack):
                self.error("starred expression is not a statement", start)
            node = ast.ExpressionStatement(target)
        return self.finish(node, start)

    def _dotted_name(self):
        parts = [self.name_token().text]
        while self.match("."):
            parts.append(self.name_token().text)
        return ".".join(parts)

    def _import_item(self, *, dotted):
        start = self.current
        name = self._dotted_name() if dotted else self.name_token().text
        alias = self.name_token().text if self.match("as") else None
        return self.finish(ast.ImportItem(name, alias), start)

    def import_statement(self):
        start = self.expect("import")
        items = [self._import_item(dotted=True)]
        while self.match(","):
            items.append(self._import_item(dotted=True))
        return self.finish(ast.Import(items), start)

    def from_import_statement(self):
        start = self.expect("from")
        level = 0
        while self.current.text in {".", "..."}:
            level += len(self.advance().text)
        module = self._dotted_name() if self.current.text != "import" else None
        if module is None and not level:
            self.error("expected module name")
        self.expect("import")
        if self.match("*"):
            items = [self.finish(ast.ImportItem("*"), self.previous)]
        else:
            grouped = bool(self.match("("))
            items = [self._import_item(dotted=False)]
            while self.match(","):
                if grouped and self.current.text == ")":
                    break
                items.append(self._import_item(dotted=False))
            if grouped:
                self.expect(")")
        return self.finish(ast.ImportFrom(module, items, level), start)

    def suite(self, *, type_comment_target=None):
        self.expect(":")
        if self.current.kind == "TYPE_COMMENT":
            if type_comment_target is None:
                self.error("type comment is not valid on this suite")
            type_comment_target.type_comment = self._type_comment()
        if self.current.kind != "NEWLINE":
            return self.simple_statements()
        self.advance()
        # A function type signature may occupy its own comment line before the
        # suite's first statement. Such comment lines do not introduce a suite.
        if self.current.kind == "TYPE_COMMENT" and isinstance(
            type_comment_target, ast.FunctionDefinition
        ):
            if type_comment_target.type_comment is not None:
                self.error("duplicate function type comment")
            type_comment_target.type_comment = self._type_comment()
            if self.current.kind == "NEWLINE":
                self.advance()
        self.expect_kind("INDENT")
        body = self.statements(stop_kinds=("DEDENT", "EOF"))
        if not body:
            self.error("expected an indented suite")
        self.expect_kind("DEDENT")
        return body

    def if_statement(self):
        start = self.expect("if")
        node = ast.If(self.expression(allow_named=True), self.suite())
        branches = [(node, start)]
        tail = node
        while self.match("elif"):
            elif_start = self.previous
            condition = self.expression(allow_named=True)
            nested = ast.If(condition, self.suite())
            branches.append((nested, elif_start))
            tail.else_body = [nested]
            tail = nested
        if self.match("else"):
            tail.else_body = self.suite()
        end = (tail.else_body or tail.body)[-1].span.end
        for branch, branch_start in branches:
            self.finish(branch, branch_start, end=end)
        return node

    def while_statement(self):
        start = self.expect("while")
        condition = self.expression(allow_named=True)
        body = self.suite()
        else_body = self.suite() if self.match("else") else []
        return self.finish(
            ast.While(condition, body, else_body),
            start,
            end=(else_body or body)[-1].span.end,
        )

    def for_statement(self, *, start=None, is_async=False):
        for_token = self.expect("for")
        start = start or for_token
        target = self.expression_list(min_bp=31)
        self.validate_target(target)
        self.expect("in")
        iterable = self._statement_value()
        node = ast.For(target, iterable, [], is_async=is_async)
        node.body = self.suite(type_comment_target=node)
        if self.match("else"):
            node.else_body = self.suite()
        return self.finish(node, start, end=(node.else_body or node.body)[-1].span.end)

    def with_statement(self, *, start=None, is_async=False):
        with_token = self.expect("with")
        start = start or with_token
        grouped = self._with_item_parentheses()
        if grouped:
            self.expect("(")
        items = []
        while True:
            item_start = self.current
            context = self.expression()
            target = None
            if self.match("as"):
                target = self.expression(min_bp=31)
                self.validate_target(target)
            items.append(self.finish(ast.WithItem(context, target), item_start))
            if not self.match(","):
                break
            if grouped and self.current.text == ")":
                break
        if grouped:
            self.expect(")")
        node = ast.With(items, [], is_async=is_async)
        node.body = self.suite(type_comment_target=node)
        return self.finish(node, start, end=node.body[-1].span.end)

    def _with_item_parentheses(self):
        """Distinguish a manager list from an ordinary grouped expression."""
        if self.current.text != "(":
            return False
        depth = 1
        distance = 1
        expression_group = False
        while True:
            token = self.peek(distance)
            if token.kind == "EOF":
                return False
            if token.text in {"(", "[", "{"}:
                depth += 1
            elif token.text in {")", "]", "}"}:
                depth -= 1
                if not depth:
                    following = self.peek(distance + 1)
                    return not expression_group and following.text in {":", ","}
            elif depth == 1 and token.text in {"for", "async", ":="}:
                expression_group = True
            distance += 1

    def try_statement(self):
        start = self.expect("try")
        node = ast.Try(self.suite())
        seen_bare = False
        while self.match("except"):
            handler_start = self.previous
            if seen_bare:
                self.error("bare except must be the last handler", handler_start)
            groups = bool(self.match("*"))
            if node.handlers and groups != node.exception_groups:
                self.error("cannot mix except and except*", handler_start)
            node.exception_groups = groups
            exception = binding = None
            if self.current.text != ":":
                grouped = self.current.text == "("
                exception = self.expression_list(allow_star=False, stops=("as", ":"))
                if self.match("as"):
                    if isinstance(exception, ast.Tuple) and not grouped:
                        self.error(
                            "exception types must be parenthesized with an as binding"
                        )
                    binding = self.name_token().text
            elif groups:
                self.error("except* requires an exception type")
            else:
                seen_bare = True
            body = self.suite()
            node.handlers.append(
                self.finish(
                    ast.ExceptionHandler(exception, binding, body),
                    handler_start,
                    end=body[-1].span.end,
                )
            )
        if self.match("else"):
            if not node.handlers:
                self.error("try else requires an exception handler")
            node.else_body = self.suite()
        if self.match("finally"):
            node.finally_body = self.suite()
        if not node.handlers and not node.finally_body:
            self.error("try requires except or finally")
        final_suite = node.finally_body or node.else_body or node.handlers[-1].body
        return self.finish(node, start, end=final_suite[-1].span.end)

    def decorated_definition(self):
        start = self.current
        decorators = []
        while self.match("@"):
            decorators.append(self.expression())
            self.expect_kind("NEWLINE")
        if self.current.text == "class":
            return self.class_definition(start=start, decorators=decorators)
        is_async = bool(self.match("async"))
        if self.current.text != "def":
            self.error("decorators require a function or class definition")
        return self.function_definition(
            start=start, decorators=decorators, is_async=is_async
        )

    def function_definition(self, *, start=None, decorators=None, is_async=False):
        def_token = self.expect("def")
        start = start or def_token
        name = self.name_token().text
        type_parameters = self.type_parameters()
        self.expect("(")
        parameters = self.parameters()
        returns = self.expression() if self.match("->") else None
        node = ast.FunctionDefinition(
            name,
            parameters,
            [],
            decorators=decorators or [],
            return_annotation=returns,
            type_parameters=type_parameters,
            is_async=is_async,
        )
        node.body = self.suite(type_comment_target=node)
        return self.finish(node, start, end=node.body[-1].span.end)

    def class_definition(self, *, start=None, decorators=None):
        class_token = self.expect("class")
        start = start or class_token
        name = self.name_token().text
        type_parameters = self.type_parameters()
        arguments = self.call_arguments() if self.match("(") else []
        body = self.suite()
        return self.finish(
            ast.ClassDefinition(
                name,
                body,
                arguments,
                decorators=decorators or [],
                type_parameters=type_parameters,
            ),
            start,
            end=body[-1].span.end,
        )

    def parameters(self, end=")", *, lambda_mode=False):
        result = []
        slash_seen = False
        star_seen = False
        bare_star = None
        positional_default = False
        if self.match(end):
            return result
        while True:
            if self.current.text == "/":
                marker = self.advance()
                if slash_seen or star_seen or not result:
                    self.error("invalid positional-only marker", marker)
                slash_seen = True
                for parameter in result:
                    parameter.kind = ast.ParameterKind.POSITIONAL_ONLY
                if self.match(end):
                    return result
                self.expect(",")
                if self.match(end):
                    return result
                continue
            start = self.current
            if self.match("**"):
                kind = ast.ParameterKind.VAR_KEYWORD
                star_seen = True
            elif self.match("*"):
                if star_seen:
                    self.error("duplicate star parameter", start)
                star_seen = True
                if self.current.text in {",", end}:
                    bare_star = start
                    if self.current.text == end:
                        self.error("bare star requires a keyword-only parameter", start)
                    self.expect(",")
                    continue
                kind = ast.ParameterKind.VAR_POSITIONAL
            else:
                kind = (
                    ast.ParameterKind.KEYWORD_ONLY
                    if star_seen
                    else ast.ParameterKind.POSITIONAL_OR_KEYWORD
                )
            name = self.name_token().text
            annotation = default = None
            if not lambda_mode and self.match(":"):
                annotation_start = self.current
                if self.match("*"):
                    if kind != ast.ParameterKind.VAR_POSITIONAL:
                        self.error(
                            "star annotation requires a variadic parameter",
                            annotation_start,
                        )
                    annotation = self.finish(
                        ast.Unpack(self.expression()), annotation_start
                    )
                else:
                    annotation = self.expression()
            if self.match("="):
                if kind in {
                    ast.ParameterKind.VAR_POSITIONAL,
                    ast.ParameterKind.VAR_KEYWORD,
                }:
                    self.error("variadic parameters cannot have defaults", start)
                default = self.expression()
                if kind == ast.ParameterKind.POSITIONAL_OR_KEYWORD:
                    positional_default = True
            elif kind == ast.ParameterKind.POSITIONAL_OR_KEYWORD and positional_default:
                self.error("non-default parameter follows default parameter", start)
            parameter = self.finish(
                ast.Parameter(name, kind, annotation, default), start
            )
            if self.current.kind == "TYPE_COMMENT":
                parameter.type_comment = self._type_comment()
            result.append(parameter)
            if kind == ast.ParameterKind.VAR_KEYWORD:
                if bare_star is not None and not any(
                    item.kind == ast.ParameterKind.KEYWORD_ONLY for item in result
                ):
                    self.error("bare star requires a keyword-only parameter", bare_star)
                self.match(",")
                if self.current.kind == "TYPE_COMMENT":
                    parameter.type_comment = self._type_comment()
                self.expect(end)
                return result
            if self.match(end):
                if bare_star is not None and not any(
                    item.kind == ast.ParameterKind.KEYWORD_ONLY for item in result
                ):
                    self.error("bare star requires a keyword-only parameter", bare_star)
                return result
            self.expect(",")
            if self.current.kind == "TYPE_COMMENT":
                if parameter.type_comment is not None:
                    self.error("duplicate parameter type comment")
                parameter.type_comment = self._type_comment()
            if self.match(end):
                return result

    def type_parameters(self):
        if not self.match("["):
            return []
        result = []
        while True:
            start = self.current
            kind = ast.TypeParameterKind.TYPE_VAR
            if self.match("**"):
                kind = ast.TypeParameterKind.PARAM_SPEC
            elif self.match("*"):
                kind = ast.TypeParameterKind.TYPE_VAR_TUPLE
            name = self.name_token().text
            bound = default = None
            constraints = []
            if self.match(":"):
                if kind != ast.TypeParameterKind.TYPE_VAR:
                    self.error("variadic type parameters cannot have a bound", start)
                bound = self.expression()
                if isinstance(bound, ast.Tuple):
                    constraints = bound.elements
                    bound = None
            if self.match("="):
                default_start = self.current
                if self.match("*"):
                    default = self.finish(ast.Unpack(self.expression()), default_start)
                else:
                    default = self.expression()
            result.append(
                self.finish(
                    ast.TypeParameter(name, kind, bound, constraints, default), start
                )
            )
            if not self.match(","):
                break
            if self.current.text == "]":
                break
        self.expect("]")
        return result

    def type_alias_statement(self):
        start = self.expect("type")
        name = self.name_token().text
        parameters = self.type_parameters()
        self.expect("=")
        value = self.expression()
        return self.finish(ast.TypeAlias(name, value, parameters), start)
