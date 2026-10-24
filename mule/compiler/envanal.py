"""Environment analysis, following ``src/compiler/envanal.lisp``.

Two of SBCL's responsibilities apply to Python today:

- ``compute_closure`` finds variables referenced from a lambda other than
  their home and records them on each environment between the reference and
  the home. A closed-over variable that is assigned anywhere is ``indirect``:
  the closure holds a cell, not a value.
- ``find_cleanup_points`` inserts cleanup code on every control edge that
  leaves a protected region: leaving an ``except`` body unbinds the handler
  name and pops the handler, leaving ``finally`` runs the finally body, and
  leaving ``with`` calls the exit method with no exception. The converter
  only records the regions; this phase emits the code, so every exit path
  (fall-through, ``break``, ``continue``, ``return``) gets it exactly once.

Python has no lexical non-local exits (a nested function cannot ``break`` an
enclosing loop), so ``find-non-local-exits`` has no counterpart; exceptions are
the only non-local control and their dispatch blocks are reached through the
cleanup that designates them.

Cleanup code is emitted into one block per cleanup level, converted in the
environment of that cleanup's parent, and per predecessor edge rather than
shared between predecessors, because the backend bridge locates a ``return``
value by walking the single-predecessor chain back from the return block.
"""

from __future__ import annotations

from mule import ast

from . import fndb
from .dfo import find_initial_dfo
from .ir1util import block_end_cleanup, block_home_lambda, block_start_cleanup
from .node import (
    Cleanup,
    CleanupKind,
    Component,
    Ctran,
    Environment,
    Lambda,
    Lvar,
    linked,
)


def environment_analyze(compilation) -> None:
    for lambda_ in compilation.lambdas:
        lambda_.environment = Environment(lambda_)
    for lambda_ in compilation.lambdas:
        compute_closure(lambda_)
    pending = list(compilation.components)
    converter = compilation.converter
    while pending:
        for component in pending:
            find_cleanup_points(component, compilation.converters)
        # Cleanup code can define functions (a lambda or comprehension in a
        # ``finally`` suite, converted once per exit edge); give them their
        # own components, then recompute closures, which they can extend.
        pending = []
        while converter.late:
            initial, lambdas = converter.late.pop(0)
            components = find_initial_dfo(initial, lambdas)
            compilation.lambdas.extend(lambdas)
            compilation.components.extend(components)
            pending.extend(components)
            for lambda_ in lambdas:
                lambda_.environment = Environment(lambda_)
        if pending:
            for lambda_ in compilation.lambdas:
                compute_closure(lambda_)
    # Introspection and closure slots must use the same order. Finalize it
    # after cleanup conversion has added every transitive capture.
    for lambda_ in compilation.lambdas:
        environment = linked(lambda_.environment, lambda_, "environment")
        environment.closure.sort(key=lambda var: var.name)
    for function in converter.function_converters.values():
        function.finish_conversion()
    converter.function_converters.clear()


def compute_closure(lambda_: Lambda) -> bool:
    """Close over every variable of ``lambda_`` referenced or assigned from
    another lambda, and every variable ``lambda_`` declares ``nonlocal``.
    Returns whether anything was closed over."""
    environment = lambda_.environment
    closed = False
    for var in lambda_.declared_free:
        closed = True
        _close_over(var, environment, linked(var.home, var, "home").environment)
    for var in lambda_.locals:
        for ref in var.refs:
            ref_lambda = ref.home_lambda
            if ref_lambda is not lambda_:
                if any(s is not var.arg_info for s in var.sets):
                    var.indirect = True
                closed = True
                _close_over(var, ref_lambda.environment, environment)
        if not var.refs:
            continue
        for assignment in var.sets:
            set_lambda = assignment.home_lambda
            if set_lambda is not lambda_:
                closed = True
                var.indirect = True
                _close_over(var, set_lambda.environment, environment)
    return closed


def _close_over(var, ref_environment, home_environment) -> None:
    """Record ``var`` in every environment from the referencing lambda up to,
    but excluding, its home (SBCL's ``close-over``)."""
    if ref_environment is home_environment or var in ref_environment.closure:
        return
    ref_environment.closure.append(var)
    for ref in ref_environment.lambda_.refs:
        _close_over(var, ref.home_lambda.environment, home_environment)


def find_cleanup_points(component: Component, converters) -> None:
    """Emit cleanup code on edges whose successor starts in a different
    cleanup, unless the edge merely enters a nested cleanup.

    Edges are taken from a worklist rather than a single pass over the block
    order: inserting code creates edges of its own, and a ``finally`` suite may
    ``return`` or ``break`` across further cleanups, so edges leaving newly
    created blocks are examined too. The edges that connect an inserted chain
    are recorded as handled so the chain is never cleaned up again.
    """
    blocks = list(component.blocks())
    if not any(block_end_cleanup(block) is not None for block in blocks):
        return
    handled: set[tuple[int, int]] = set()
    known_blocks = {id(block) for block in blocks}
    pending = [
        (block1, block2)
        for block1 in blocks
        for block2 in block1.succ
        if block2.start is not None
    ]
    while pending:
        block1, block2 = pending.pop()
        key = id(block1), id(block2)
        if key in handled or block1.component is None or block2 not in block1.succ:
            continue
        handled.add(key)
        if block_home_lambda(block2) is not block_home_lambda(block1):
            continue
        cleanup1 = block_end_cleanup(block1)
        cleanup2 = block_start_cleanup(block2)
        if cleanup1 is cleanup2 or (
            cleanup2 is not None and cleanup2.parent is cleanup1
        ):
            continue
        chain = _emit_cleanups(block1, block2, cleanup1, cleanup2, converters)
        if not chain:
            continue
        for pred, succ in zip((block1, *chain), (*chain, block2), strict=True):
            handled.add((id(pred), id(succ)))
        for block in component.blocks():
            if id(block) in known_blocks:
                continue
            known_blocks.add(id(block))
            pending.extend(
                (block, successor)
                for successor in block.succ
                if successor.start is not None
            )


def _nested_cleanups(inner: Cleanup | None, outer: Cleanup | None):
    """Cleanups from ``inner`` outward, stopping at any cleanup still enclosing ``outer``."""
    enclosing = set()
    cursor = outer
    while cursor is not None:
        enclosing.add(cursor)
        cursor = cursor.parent
    cleanup = inner
    while cleanup is not None and cleanup not in enclosing:
        yield cleanup
        cleanup = cleanup.parent


def _emit_cleanups(block1, block2, cleanup1, cleanup2, converters) -> list:
    """Insert one block per cleanup level between ``block1`` and ``block2``;
    returns the inserted chain in control order."""
    function = converters[block_home_lambda(block1)]
    chain = []
    pred = block1
    for cleanup in _nested_cleanups(cleanup1, cleanup2):
        emit = _CLEANUP_CODE.get(cleanup.kind)
        if emit is None:
            continue
        block = function.insert_cleanup_block(pred, block2, cleanup)
        emit(function, cleanup, block)
        chain.append(block)
        pred = block
    return chain


def _leave_handler_body(function, cleanup, block) -> None:
    """Unbind the ``except ... as`` name and pop the handler."""
    source = cleanup.mess_up.source if cleanup.mess_up is not None else None
    with function.in_cleanup_block(block, cleanup) as start:
        if cleanup.binding is not None:
            following = Ctran()
            function.unbind(start, following, cleanup.binding, source, missing_ok=True)
            start = following
        if cleanup.info is not False:
            following = Ctran()
            function.known(start, following, None, fndb.LEAVE_HANDLER, (), source)
            start = following
        function.finish_cleanup_block(start, block)


def _run_finally_body(function, cleanup, block) -> None:
    """Convert the ``finally`` suite in the region's enclosing environment. A
    ``return`` or ``break`` inside it transfers control away from the edge."""
    with function.in_cleanup_block(block, cleanup) as start:
        frontier = function.convert_body(start, cleanup.info)
        if frontier is not None:
            function.finish_cleanup_block(frontier, block)


def _exit_context(function, cleanup, block) -> None:
    """Call the context manager's ``__exit__`` with no exception."""
    source = cleanup.mess_up.source if cleanup.mess_up is not None else None
    with function.in_cleanup_block(block, cleanup) as start:
        ctran, exit_method = Ctran(), Lvar()
        function.ref(start, ctran, exit_method, cleanup.info, source)
        nones = []
        for _ in range(3):
            following, none = Ctran(), Lvar()
            function.constant(ctran, following, none, ast.NoneLiteral(None))
            nones.append(none)
            ctran = following
        following = Ctran()
        function.full_call(
            ctran,
            following,
            None,
            exit_method,
            tuple(nones),
            source,
            function.call_info(source),
        )
        function.finish_cleanup_block(following, block)


_CLEANUP_CODE = {
    CleanupKind.HANDLER_BODY: _leave_handler_body,
    CleanupKind.FINALLY: _run_finally_body,
    CleanupKind.WITH: _exit_context,
}


__all__ = ["compute_closure", "environment_analyze", "find_cleanup_points"]
