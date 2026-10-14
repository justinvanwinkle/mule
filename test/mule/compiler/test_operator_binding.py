"""Operator services keep builtin calling conventions in class namespaces."""

from test.mule.compiler.test_exception_groups import compare


def test_operator_services_do_not_bind_a_receiver(tmp_path):
    compare(
        tmp_path,
        """
import operator
class Operations:
    add = operator.add
    alias = operator.__add__
    absolute = operator.abs
    index = operator.getitem
    concatenate = operator.concat
    getter = operator.itemgetter(1)
operation = Operations()
print(Operations.add(20, 22), operation.add(20, 22), operation.alias(20, 22))
print(operation.absolute(-5), operation.index([1, 2], 1))
print(operation.concatenate('directory/', 'file'), operation.getter([1, 2]))
print(operation.add is Operations.add)
""",
    )
