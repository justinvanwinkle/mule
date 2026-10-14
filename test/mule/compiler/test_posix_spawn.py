"""Native child setup and wait status match Python's POSIX subprocess boundary."""

from test.mule.compiler.test_exception_groups import compare


def test_fork_exec_pipes_environment_candidates_and_child_errors(tmp_path):
    compare(
        tmp_path,
        """
import os
from _posixsubprocess import fork_exec
def launch(argv, candidates=None, cwd=None, env=None, preserved=()):
    output_read, output_write = os.pipe()
    error_read, error_write = os.pipe()
    print(os.get_inheritable(output_read), os.get_inheritable(output_write))
    keep = tuple(sorted((*preserved, error_write)))
    candidates = (os.fsencode(argv[0]),) if candidates is None else candidates
    try:
        pid = fork_exec(argv, candidates, True, keep, cwd, env,
                        -1, -1, output_read, output_write, -1, -1,
                        error_read, error_write, True, False,
                        -1, None, None, None, -1, None)
    finally:
        os.close(output_write)
        os.close(error_write)
    error = os.read(error_read, 8192)
    output = os.read(output_read, 8192)
    os.close(error_read)
    os.close(output_read)
    returned, status = os.waitpid(pid, 0)
    print(error, output, returned == pid, os.waitstatus_to_exitcode(status))
launch(['/usr/bin/printf', '%s\\n', 'one value'])
launch(['/usr/bin/env'], env=[b'MULE_CHILD_VALUE=hello'])
launch(['/usr/bin/printf', '%s', 'candidate fallback'],
       candidates=(b'/no/such/mule-child', b'/usr/bin/printf'))
launch(['/no/such/mule-child'])
launch(['/usr/bin/printf', 'unreached'], cwd='/no/such/mule-directory')
read, write = os.pipe()
try:
    os.set_inheritable(read, True)
    launch(['/usr/bin/test', '-e', '/proc/self/fd/' + str(read)])
    launch(['/usr/bin/test', '-e', '/proc/self/fd/' + str(read)], preserved=(read,))
finally:
    os.close(read)
    os.close(write)
""",
    )


def test_wait_status_decoding_matches_python(tmp_path):
    compare(
        tmp_path,
        """
import os
for status in (0, 1, 9, 13, 128, 137, 256, 65280, 4991, 65535, -1):
    print(status, os.WIFEXITED(status), os.WIFSIGNALED(status), os.WIFSTOPPED(status),
          os.WIFCONTINUED(status), os.WEXITSTATUS(status), os.WTERMSIG(status),
          os.WSTOPSIG(status), os.WCOREDUMP(status))
    try:
        print(os.waitstatus_to_exitcode(status))
    except ValueError as error:
        print(str(error))
""",
    )
