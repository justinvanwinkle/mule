"""Monitoring's inactive controls are real state; event probes remain explicit gaps."""

from test.mule.compiler.test_exception_groups import compare
from test.mule.compiler.test_modules import execute


def test_monitoring_constants_tools_and_callback_replacement_match_python(tmp_path):
    compare(
        tmp_path,
        """
import sys
m = sys.monitoring
for name in ('NO_EVENTS', 'PY_START', 'PY_RETURN', 'PY_YIELD', 'LINE', 'CALL',
             'BRANCH_LEFT', 'BRANCH_RIGHT', 'BRANCH', 'RAISE', 'RERAISE', 'C_RETURN'):
    print(name, getattr(m.events, name))
print(m.DEBUGGER_ID, m.COVERAGE_ID, m.PROFILER_ID, m.OPTIMIZER_ID)
print(m.DISABLE is m.DISABLE, m.DISABLE is m.MISSING)
for index in range(6):
    print(m.get_tool(index), m.get_events(index))
def callback(*args):
    return None
m.use_tool_id(4, 'test')
print(m.get_tool(4), m.set_events(4, 0), m.get_events(4))
print(m.get_local_events(4, callback.__code__))
print(m.set_local_events(4, callback.__code__, 0))
print(m.register_callback(4, m.events.LINE, callback))
print(m.register_callback(4, m.events.LINE, None) is callback)
print(m.register_callback(4, m.events.LINE, callback))
m.clear_tool_id(4)
print(m.get_tool(4), m.register_callback(4, m.events.LINE, None))
m.free_tool_id(4)
print(m.get_tool(4), m.get_events(4))
for method, args in ((m.use_tool_id, (7, 'x')), (m.use_tool_id, (4, 1)),
                     (m.set_events, (4, 0)), (m.register_callback, (4, 3, None))):
    try:
        method(*args)
    except ValueError as error:
        print(str(error))
""",
    )


def test_monitoring_rejects_activation_until_compiler_probes_exist(tmp_path):
    result = execute(
        tmp_path,
        {},
        """
import sys
m = sys.monitoring
m.use_tool_id(4, 'test')
def function():
    pass
for activate in (lambda: m.set_events(4, m.events.LINE),
                 lambda: m.set_local_events(4, function.__code__, m.events.LINE)):
    try:
        activate()
    except NotImplementedError:
        print('no probes')
print(m.get_events(4), m.get_local_events(4, function.__code__))
m.free_tool_id(4)
""",
    )
    assert result.returncode == 0, result.stderr
    assert result.stdout == "no probes\nno probes\n0 0\n"
