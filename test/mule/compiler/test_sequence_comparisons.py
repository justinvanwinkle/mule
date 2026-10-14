"""Sequence comparisons defer to foreign operands through NotImplemented."""

from test.mule.compiler.test_exception_groups import compare


def test_foreign_operand_reflection_and_unsupported_ordering(tmp_path):
    compare(
        tmp_path,
        """
import operator
import sys
class Foreign:
    def __eq__(self, other):
        return 'equal'
    def __ne__(self, other):
        return 'unequal'
    def __lt__(self, other):
        return 'less'
    def __le__(self, other):
        return 'less equal'
    def __gt__(self, other):
        return 'greater'
    def __ge__(self, other):
        return 'greater equal'
for sequence in ([], ()):
    for operation in (operator.eq, operator.ne, operator.lt, operator.le, operator.gt, operator.ge):
        print(operation(sequence, Foreign()), operation(Foreign(), sequence))
    for method in ('__eq__', '__ne__', '__lt__', '__le__', '__gt__', '__ge__'):
        print(getattr(type(sequence), method)(sequence, object()) is NotImplemented)
for operation in (operator.lt, operator.le, operator.gt, operator.ge):
    try:
        operation((), [])
    except TypeError as error:
        print(str(error))
print((3, 13) < sys.version_info, sys.version_info > (3, 13))
print((3, 15) > sys.version_info, sys.version_info < (3, 15))
print([1, 2] < [1, 3], (1,) <= (1, 2), [7] == [7], () != [])
""",
    )
