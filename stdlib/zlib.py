"""Pure-Python zlib implementation (CRC-32, Adler-32, and RFC 1950/1951/1952)."""

MAX_WBITS = 15
DEF_MEM_LEVEL = 8
DEF_BUF_SIZE = 16384
DEFLATED = 8
Z_NO_COMPRESSION = 0
Z_BEST_SPEED = 1
Z_BEST_COMPRESSION = 9
Z_DEFAULT_COMPRESSION = -1
Z_DEFAULT_STRATEGY = 0
Z_FILTERED = 1
Z_HUFFMAN_ONLY = 2
Z_RLE = 3
Z_FIXED = 4
Z_NO_FLUSH = 0
Z_PARTIAL_FLUSH = 1
Z_SYNC_FLUSH = 2
Z_FULL_FLUSH = 3
Z_FINISH = 4
Z_BLOCK = 5
Z_TREES = 6
ZLIB_VERSION = "1.3.1"
ZLIB_RUNTIME_VERSION = "1.3.1"


class error(Exception):
    pass


_ZlibError = error


def _bytes_of(data):
    if isinstance(data, bytes):
        return data
    if isinstance(data, bytearray):
        return bytes(data)
    return memoryview(data).tobytes()


_CRC32_TABLE = []
for _i in range(256):
    _c = _i
    for _ in range(8):
        if _c & 1:
            _c = 0xEDB88320 ^ (_c >> 1)
        else:
            _c >>= 1
    _CRC32_TABLE.append(_c)


def crc32(data, value=0):
    raw = _bytes_of(data)
    crc = (value & 0xFFFFFFFF) ^ 0xFFFFFFFF
    for byte in raw:
        crc = _CRC32_TABLE[(crc ^ byte) & 0xFF] ^ (crc >> 8)
    return (crc ^ 0xFFFFFFFF) & 0xFFFFFFFF


def adler32(data, value=1):
    raw = _bytes_of(data)
    s1 = value & 0xFFFF
    s2 = (value >> 16) & 0xFFFF
    for byte in raw:
        s1 = (s1 + byte) % 65521
        s2 = (s2 + s1) % 65521
    return ((s2 << 16) | s1) & 0xFFFFFFFF


def _deflate_stored(raw, finish=True):
    out = bytearray()
    n = len(raw)
    pos = 0
    while n - pos > 65535:
        chunk = raw[pos : pos + 65535]
        out.append(0x00)
        out.extend((0xFFFF, 0xFF, 0x00, 0x00))
        out.extend(chunk)
        pos += 65535
    if finish:
        chunk = raw[pos:]
        size = len(chunk)
        nlen = size ^ 0xFFFF
        out.append(0x01)
        out.extend((size & 0xFF, (size >> 8) & 0xFF, nlen & 0xFF, (nlen >> 8) & 0xFF))
        out.extend(chunk)
    elif pos < n:
        chunk = raw[pos:]
        size = len(chunk)
        nlen = size ^ 0xFFFF
        out.append(0x00)
        out.extend((size & 0xFF, (size >> 8) & 0xFF, nlen & 0xFF, (nlen >> 8) & 0xFF))
        out.extend(chunk)
    return bytes(out)


class _Compress:
    def __init__(self, level=Z_DEFAULT_COMPRESSION, method=DEFLATED, wbits=MAX_WBITS, memLevel=DEF_MEM_LEVEL, strategy=Z_DEFAULT_STRATEGY, zdict=None):
        self._wbits = wbits
        self._header_sent = False
        self._crc = 0
        self._adler = 1
        self._size = 0
        self._buffer = bytearray()

    def compress(self, data):
        raw = _bytes_of(data)
        self._crc = crc32(raw, self._crc)
        self._adler = adler32(raw, self._adler)
        self._size = (self._size + len(raw)) & 0xFFFFFFFF
        self._buffer.extend(raw)
        out = bytearray()
        if not self._header_sent:
            self._header_sent = True
            if self._wbits > 15:
                out.extend(b"\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\xff")
            elif self._wbits > 0:
                out.extend(b"\x78\x9c")
        return bytes(out)

    def flush(self, mode=Z_FINISH):
        out = bytearray()
        if not self._header_sent:
            self._header_sent = True
            if self._wbits > 15:
                out.extend(b"\x1f\x8b\x08\x00\x00\x00\x00\x00\x00\xff")
            elif self._wbits > 0:
                out.extend(b"\x78\x9c")
        raw = bytes(self._buffer)
        self._buffer.clear()
        finish = mode == Z_FINISH
        out.extend(_deflate_stored(raw, finish=finish))
        if finish:
            if self._wbits > 15:
                for shift in (0, 8, 16, 24):
                    out.append((self._crc >> shift) & 0xFF)
                for shift in (0, 8, 16, 24):
                    out.append((self._size >> shift) & 0xFF)
            elif self._wbits > 0:
                for shift in (24, 16, 8, 0):
                    out.append((self._adler >> shift) & 0xFF)
        return bytes(out)


def compressobj(level=Z_DEFAULT_COMPRESSION, method=DEFLATED, wbits=MAX_WBITS, memLevel=DEF_MEM_LEVEL, strategy=Z_DEFAULT_STRATEGY, zdict=None):
    return _Compress(level, method, wbits, memLevel, strategy, zdict)


def compress(data, /, level=Z_DEFAULT_COMPRESSION, wbits=MAX_WBITS):
    c = compressobj(level=level, wbits=wbits)
    return c.compress(data) + c.flush()


class _BitReader:
    def __init__(self, data, pos=0):
        self.data = data
        self.pos = pos
        self.bitbuf = 0
        self.bitcnt = 0

    def bits(self, n):
        while self.bitcnt < n:
            if self.pos >= len(self.data):
                raise error("Error -5 while decompressing data: incomplete or truncated stream")
            self.bitbuf |= self.data[self.pos] << self.bitcnt
            self.pos += 1
            self.bitcnt += 8
        val = self.bitbuf & ((1 << n) - 1)
        self.bitbuf >>= n
        self.bitcnt -= n
        return val

    def align(self):
        self.bitbuf = 0
        self.bitcnt = 0

    def read_bytes(self, n):
        self.align()
        if self.pos + n > len(self.data):
            raise error("Error -5 while decompressing data: incomplete or truncated stream")
        res = self.data[self.pos : self.pos + n]
        self.pos += n
        return res


def _build_huffman(lengths):
    table = {}
    counts = {}
    max_bits = 0
    for length in lengths:
        if length > 0:
            counts[length] = counts.get(length, 0) + 1
            if length > max_bits:
                max_bits = length
    code = 0
    next_code = {}
    for bits in range(1, max_bits + 1):
        code = (code + counts.get(bits - 1, 0)) << 1
        next_code[bits] = code
    for sym, length in enumerate(lengths):
        if length > 0:
            c = next_code[length]
            next_code[length] = c + 1
            rev = 0
            for i in range(length):
                if (c >> (length - 1 - i)) & 1:
                    rev |= 1 << i
            table[(length, rev)] = sym
    return table, max_bits


def _decode_symbol(reader, table, max_bits):
    code = 0
    for length in range(1, max_bits + 1):
        code |= reader.bits(1) << (length - 1)
        key = (length, code)
        if key in table:
            return table[key]
    raise error("Error -3 while decompressing data: invalid code")


_FIXED_LIT_TABLE, _FIXED_LIT_MAX = _build_huffman(
    [8] * 144 + [9] * 112 + [7] * 24 + [8] * 8
)
_FIXED_DIST_TABLE, _FIXED_DIST_MAX = _build_huffman([5] * 32)
_LEN_BASE = (
    3, 4, 5, 6, 7, 8, 9, 10, 11, 13, 15, 17, 19, 23, 27, 31,
    35, 43, 51, 59, 67, 83, 99, 115, 131, 163, 195, 227, 258,
)
_LEN_EXTRA = (
    0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2,
    3, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 0,
)
_DIST_BASE = (
    1, 2, 3, 4, 5, 7, 9, 13, 17, 25, 33, 49, 65, 97, 129, 193,
    257, 385, 513, 769, 1025, 1537, 2049, 3073, 4097, 6145, 8193, 12289, 16385, 24577,
)
_DIST_EXTRA = (
    0, 0, 0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6,
    7, 7, 8, 8, 9, 9, 10, 10, 11, 11, 12, 12, 13, 13,
)
_CL_ORDER = (16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15)


def _inflate_raw(data, pos=0):
    reader = _BitReader(data, pos)
    out = bytearray()
    bfinal = 0
    while not bfinal:
        bfinal = reader.bits(1)
        btype = reader.bits(2)
        if btype == 0:
            header = reader.read_bytes(4)
            length = header[0] | (header[1] << 8)
            nlen = header[2] | (header[3] << 8)
            if (length ^ 0xFFFF) != nlen:
                raise error("Error -3 while decompressing data: invalid stored block lengths")
            out.extend(reader.read_bytes(length))
        elif btype == 1 or btype == 2:
            if btype == 1:
                lit_table, lit_max = _FIXED_LIT_TABLE, _FIXED_LIT_MAX
                dist_table, dist_max = _FIXED_DIST_TABLE, _FIXED_DIST_MAX
            else:
                hlit = reader.bits(5) + 257
                hdist = reader.bits(5) + 1
                hclen = reader.bits(4) + 4
                cl_lengths = [0] * 19
                for i in range(hclen):
                    cl_lengths[_CL_ORDER[i]] = reader.bits(3)
                cl_table, cl_max = _build_huffman(cl_lengths)
                lengths = []
                while len(lengths) < hlit + hdist:
                    sym = _decode_symbol(reader, cl_table, cl_max)
                    if sym < 16:
                        lengths.append(sym)
                    elif sym == 16:
                        repeat = reader.bits(2) + 3
                        prev = lengths[-1]
                        lengths.extend([prev] * repeat)
                    elif sym == 17:
                        repeat = reader.bits(3) + 3
                        lengths.extend([0] * repeat)
                    else:
                        repeat = reader.bits(7) + 11
                        lengths.extend([0] * repeat)
                lit_table, lit_max = _build_huffman(lengths[:hlit])
                dist_table, dist_max = _build_huffman(lengths[hlit : hlit + hdist])
            while True:
                sym = _decode_symbol(reader, lit_table, lit_max)
                if sym < 256:
                    out.append(sym)
                elif sym == 256:
                    break
                else:
                    idx = sym - 257
                    length = _LEN_BASE[idx] + reader.bits(_LEN_EXTRA[idx])
                    d_sym = _decode_symbol(reader, dist_table, dist_max)
                    dist = _DIST_BASE[d_sym] + reader.bits(_DIST_EXTRA[d_sym])
                    for _ in range(length):
                        out.append(out[-dist])
        else:
            raise error("Error -3 while decompressing data: invalid block type")
    reader.align()
    return bytes(out), reader.pos


class _Decompress:
    def __init__(self, wbits=MAX_WBITS, zdict=b""):
        self._wbits = wbits
        self.unused_data = b""
        self.unconsumed_tail = b""
        self.eof = False
        self._buf = bytearray()

    def decompress(self, data, max_length=0):
        self._buf.extend(_bytes_of(data))
        raw = bytes(self._buf)
        if not raw:
            return b""
        pos = 0
        wbits = self._wbits
        if wbits > 31 and raw[:2] == b"\x1f\x8b":
            wbits = 31
        if wbits > 15:
            if len(raw) < 18 or raw[:2] != b"\x1f\x8b":
                raise error("Error -3 while decompressing data: incorrect header check")
            flg = raw[3]
            pos = 10
            if flg & 4:
                xlen = raw[pos] | (raw[pos + 1] << 8)
                pos += 2 + xlen
            if flg & 8:
                pos = raw.index(0, pos) + 1
            if flg & 16:
                pos = raw.index(0, pos) + 1
            if flg & 2:
                pos += 2
            result, end_pos = _inflate_raw(raw, pos)
            self.eof = True
            self.unused_data = raw[end_pos + 8 :]
            self._buf.clear()
            return result
        if wbits > 0:
            if len(raw) < 6:
                raise error("Error -5 while decompressing data: incomplete or truncated stream")
            pos = 2
            result, end_pos = _inflate_raw(raw, pos)
            self.eof = True
            self.unused_data = raw[end_pos + 4 :]
            self._buf.clear()
            return result
        result, end_pos = _inflate_raw(raw, 0)
        self.eof = True
        self.unused_data = raw[end_pos:]
        self._buf.clear()
        return result

    def flush(self, length=DEF_BUF_SIZE):
        return b""


def decompressobj(wbits=MAX_WBITS, zdict=b""):
    return _Decompress(wbits, zdict)


def decompress(data, /, wbits=MAX_WBITS, bufsize=DEF_BUF_SIZE):
    return decompressobj(wbits=wbits).decompress(data)
