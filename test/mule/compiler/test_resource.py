"""Resource limits and usage come from real Linux process services."""

from test.mule.compiler.test_exception_groups import compare


def test_limits_usage_and_validation_match_python(tmp_path):
    compare(
        tmp_path,
        """
import resource
print(resource.error is OSError, resource.RLIM_INFINITY)
original = resource.getrlimit(resource.RLIMIT_NOFILE)
print(len(original), all(isinstance(value, int) for value in original))
try:
    soft = 256 if original[0] < 0 or original[0] > 256 else original[0]
    print(resource.setrlimit(resource.RLIMIT_NOFILE, (soft, original[1])))
    print(resource.getrlimit(resource.RLIMIT_NOFILE)[0] == soft)
    print(resource.prlimit(0, resource.RLIMIT_NOFILE) == (soft, original[1]))
    print(resource.prlimit(0, resource.RLIMIT_NOFILE, original) == (soft, original[1]))
finally:
    resource.setrlimit(resource.RLIMIT_NOFILE, original)
print(resource.getpagesize() > 0)
for who in (resource.RUSAGE_SELF, resource.RUSAGE_CHILDREN, resource.RUSAGE_THREAD):
    usage = resource.getrusage(who)
    print(isinstance(usage, tuple), len(usage), type(usage).__name__)
    print(usage.ru_utime >= 0, usage.ru_stime >= 0, usage.ru_maxrss >= 0)
    print(usage.ru_utime == usage[0], usage.ru_nivcsw == usage[15])
    print(isinstance(usage.ru_utime, float), isinstance(usage.ru_maxrss, int))
for value in (-1, 16):
    try:
        resource.getrlimit(value)
    except ValueError as error:
        print(str(error))
for limits in ((1,), (1, 2, 3), (1 << 65, 1 << 65), (-2, original[1])):
    try:
        resource.setrlimit(resource.RLIMIT_NOFILE, limits)
    except (ValueError, OverflowError) as error:
        print(type(error).__name__, str(error))
try:
    resource.getrusage(12345)
except ValueError as error:
    print(str(error))
core = resource.getrlimit(resource.RLIMIT_CORE)
if core[1] == resource.RLIM_INFINITY:
    try:
        for value in (1 << 63, (1 << 64) - 2, (1 << 64) - 1):
            resource.setrlimit(resource.RLIMIT_CORE, (value, core[1]))
            print(resource.getrlimit(resource.RLIMIT_CORE)[0] ==
                  (-1 if value == (1 << 64) - 1 else value))
    finally:
        resource.setrlimit(resource.RLIMIT_CORE, core)
""",
    )
