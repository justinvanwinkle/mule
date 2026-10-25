"""Build deferred annotation bodies for normal IR1 lambda conversion."""

from mule import ast


class _AnnotationValue(ast.Visitor):
    """Transform ``*Ts`` in annotation value position into ``(*Ts,)[0]``."""

    def visit_Node(self, node):
        return node

    def visit_Unpack(self, node):
        return ast.Subscript(
            ast.Tuple([node], span=node.span),
            ast.IntegerLiteral(0, span=node.span),
            span=node.span,
        )


_ANNOTATION_VALUE = _AnnotationValue()


class AnnotationThunk:
    def __init__(
        self, converter, definition, runtime_supported, *, annotations=None, span=None
    ):
        self.converter = converter
        self.definition = definition
        self.runtime_supported = runtime_supported
        self.span = span if span is not None else definition.span
        if annotations is not None:
            self.annotations = list(annotations)
        else:
            self.annotations = [
                (parameter.name, parameter.annotation)
                for parameter in definition.parameters
                if parameter.annotation is not None
            ]
            if definition.return_annotation is not None:
                self.annotations.append(("return", definition.return_annotation))

    @classmethod
    def from_declarations(cls, converter, span, declarations, runtime_supported):
        return cls(
            converter,
            None,
            runtime_supported,
            annotations=declarations,
            span=span,
        )

    def build(self):
        if not self.annotations:
            return None
        span = self.span
        parameter = "$annotation_format"
        format_ = ast.Name(parameter, span=span)

        def matches(values):
            return ast.Comparison(
                format_,
                [ast.ComparisonOperator.IN],
                [
                    ast.Tuple(
                        [ast.IntegerLiteral(value, span=span) for value in values],
                        span=span,
                    )
                ],
                span=span,
            )

        strings = ast.Dict(
            [
                ast.DictEntry(
                    ast.TextLiteral(name, span=expression.span),
                    ast.TextLiteral(
                        self.converter.source_text(expression), span=expression.span
                    ),
                    span=expression.span,
                )
                for name, expression in self.annotations
            ],
            span=span,
        )
        values: list[ast.Statement]
        if self.converter.future_annotations:
            values = [ast.Return(strings, span=span)]
        elif all(
            expression.accept(self.runtime_supported)
            for _, expression in self.annotations
        ):
            values = [
                ast.Return(
                    ast.Dict(
                        [
                            ast.DictEntry(
                                ast.TextLiteral(name, span=expression.span),
                                expression.accept(_ANNOTATION_VALUE),
                                span=expression.span,
                            )
                            for name, expression in self.annotations
                        ],
                        span=span,
                    ),
                    span=span,
                )
            ]
        else:
            values = [ast.Raise(ast.Name("NotImplementedError", span=span), span=span)]
        value_formats = (1, 2, 3) if self.converter.future_annotations else (1, 2)
        return ast.FunctionDefinition(
            "__annotate__",
            [ast.Parameter(parameter, ast.ParameterKind.POSITIONAL_ONLY, span=span)],
            [
                ast.If(matches((4,)), [ast.Return(strings, span=span)], span=span),
                ast.If(matches(value_formats), values, span=span),
                ast.Raise(ast.Name("NotImplementedError", span=span), span=span),
            ],
            span=span,
        )
