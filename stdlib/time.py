"""Time access and conversions, as CPython's builtin ``time`` module.

Clock and calendar services come from the ``_time`` native module; the
``struct_time`` type and formatting are Python here.
"""

from _time import gmtime_parts as _gmtime_parts
from _time import localtime_parts as _localtime_parts
from _time import monotonic as _monotonic
from _time import process as _process
from _time import sleep_for as _sleep_for
from _time import thread as _thread
from _time import wall as _wall
from _time import zoneinfo as _zoneinfo

CLOCK_MONOTONIC = 1
CLOCK_MONOTONIC_RAW = 4
CLOCK_PROCESS_CPUTIME_ID = 2
CLOCK_REALTIME = 0
CLOCK_THREAD_CPUTIME_ID = 3
CLOCK_BOOTTIME = 7
CLOCK_REALTIME_ALARM = 8
CLOCK_BOOTTIME_ALARM = 9
CLOCK_TAI = 11

_zone = _zoneinfo()
timezone = _zone[0]
daylight = _zone[1]
tzname = _zone[2]
altzone = timezone - 3600 if daylight else timezone


class struct_time:
    """A time value as a tuple of nine fields with named access."""

    n_fields = 11
    n_unnamed_fields = 0
    n_sequence_fields = 9

    def __init__(self, values):
        self._values = tuple(values)

    def __getitem__(self, index):
        return self._values[index]

    def __len__(self):
        return 9

    def __iter__(self):
        return iter(self._values)

    def __eq__(self, other):
        if isinstance(other, struct_time):
            return self._values == other._values
        if isinstance(other, tuple):
            return self._values == other
        return NotImplemented

    def __ne__(self, other):
        result = self.__eq__(other)
        return result if result is NotImplemented else not result

    def __repr__(self):
        names = (
            "tm_year", "tm_mon", "tm_mday", "tm_hour", "tm_min", "tm_sec",
            "tm_wday", "tm_yday", "tm_isdst",
        )
        parts = []
        for index in range(9):
            parts.append(names[index] + "=" + str(self._values[index]))
        return "time.struct_time(" + ", ".join(parts) + ")"

    @property
    def tm_year(self):
        return self._values[0]

    @property
    def tm_mon(self):
        return self._values[1]

    @property
    def tm_mday(self):
        return self._values[2]

    @property
    def tm_hour(self):
        return self._values[3]

    @property
    def tm_min(self):
        return self._values[4]

    @property
    def tm_sec(self):
        return self._values[5]

    @property
    def tm_wday(self):
        return self._values[6]

    @property
    def tm_yday(self):
        return self._values[7]

    @property
    def tm_isdst(self):
        return self._values[8]


def time():
    return _wall()


def monotonic():
    return _monotonic()


def perf_counter():
    return _monotonic()


def process_time():
    return _process()


def thread_time():
    return _thread()


def time_ns():
    return int(_wall() * 1000000000)


def monotonic_ns():
    return int(_monotonic() * 1000000000)


def perf_counter_ns():
    return int(_monotonic() * 1000000000)


def process_time_ns():
    return int(_process() * 1000000000)


def thread_time_ns():
    return int(_thread() * 1000000000)


def sleep(seconds):
    return _sleep_for(seconds)


def clock_gettime(clock_id):
    if clock_id == CLOCK_REALTIME:
        return _wall()
    if clock_id == CLOCK_PROCESS_CPUTIME_ID:
        return _process()
    if clock_id == CLOCK_THREAD_CPUTIME_ID:
        return _thread()
    return _monotonic()


def clock_gettime_ns(clock_id):
    return int(clock_gettime(clock_id) * 1000000000)


def clock_getres(clock_id):
    return 1e-09


def gmtime(seconds=None):
    return struct_time(_gmtime_parts(_wall() if seconds is None else seconds))


def localtime(seconds=None):
    return struct_time(_localtime_parts(_wall() if seconds is None else seconds))


def asctime(parts=None):
    if parts is None:
        parts = localtime()
    days = ("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
    months = ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
    return "%s %s %2d %02d:%02d:%02d %d" % (
        days[parts[6]], months[parts[1] - 1], parts[2], parts[3], parts[4], parts[5], parts[0])


def ctime(seconds=None):
    return asctime(localtime(seconds))


def mktime(parts):
    year, month = parts[0], parts[1]
    # Days since the epoch, from the civil calendar (Howard Hinnant's algorithm).
    shifted = year - (1 if month <= 2 else 0)
    era = shifted // 400
    yoe = shifted - era * 400
    mp = (month + 9) % 12
    doy = (153 * mp + 2) // 5 + parts[2] - 1
    doe = yoe * 365 + yoe // 4 - yoe // 100 + doy
    days = era * 146097 + doe - 719468
    return float(days * 86400 + parts[3] * 3600 + parts[4] * 60 + parts[5] + timezone)


_DAYS = ("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
_DAYS_SHORT = ("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
_MONTHS = ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
_MONTHS_SHORT = ("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")


def strftime(format, parts=None):
    if parts is None:
        parts = localtime()
    result = []
    index = 0
    while index < len(format):
        char = format[index]
        index += 1
        if char != "%":
            result.append(char)
            continue
        if index >= len(format):
            result.append("%")
            break
        code = format[index]
        index += 1
        if code == "%":
            result.append("%")
        elif code == "a":
            result.append(_DAYS_SHORT[parts[6]])
        elif code == "A":
            result.append(_DAYS[parts[6]])
        elif code == "b" or code == "h":
            result.append(_MONTHS_SHORT[parts[1] - 1])
        elif code == "B":
            result.append(_MONTHS[parts[1] - 1])
        elif code == "c":
            result.append(asctime(parts))
        elif code == "d":
            result.append("%02d" % parts[2])
        elif code == "H":
            result.append("%02d" % parts[3])
        elif code == "I":
            hour = parts[3] % 12
            result.append("%02d" % (12 if hour == 0 else hour))
        elif code == "j":
            result.append("%03d" % parts[7])
        elif code == "m":
            result.append("%02d" % parts[1])
        elif code == "M":
            result.append("%02d" % parts[4])
        elif code == "p":
            result.append("AM" if parts[3] < 12 else "PM")
        elif code == "S":
            result.append("%02d" % parts[5])
        elif code == "U" or code == "W":
            result.append("%02d" % ((parts[7] - 1) // 7))
        elif code == "w":
            result.append(str((parts[6] + 1) % 7))
        elif code == "x":
            result.append("%02d/%02d/%02d" % (parts[1], parts[2], parts[0] % 100))
        elif code == "X":
            result.append("%02d:%02d:%02d" % (parts[3], parts[4], parts[5]))
        elif code == "y":
            result.append("%02d" % (parts[0] % 100))
        elif code == "Y":
            result.append(str(parts[0]))
        elif code == "Z":
            result.append(tzname[1] if parts[8] > 0 else tzname[0])
        else:
            result.append("%" + code)
    return "".join(result)


def get_clock_info(name):
    if name not in ("time", "monotonic", "perf_counter", "process_time", "thread_time"):
        raise ValueError("unknown clock")

    class _Info:
        adjustable = name == "time"
        implementation = "clock_gettime()"
        monotonic = name != "time"
        resolution = 1e-09

    return _Info()
