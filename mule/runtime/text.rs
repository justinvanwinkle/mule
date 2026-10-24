//! Physical `str` storage following CPython's PEP 393 compact layout.
//!
//! Each string stores every codepoint in the narrowest width that holds its
//! largest codepoint: one byte through U+00FF, two through U+FFFF, otherwise
//! four. The width is canonical, so equal strings always share a variant and
//! codepoint access is constant time. Lone surrogates are valid codepoints.
//! Text algorithms are compiled Mule Python; this module only allocates,
//! copies, and reads storage.
use std::cmp::Ordering;
use std::sync::atomic::{AtomicI64, Ordering as AtomicOrdering};

const INLINE_LATIN1_CAP: usize = 24;

#[repr(u8)]
#[derive(Clone, Debug, PartialEq, Eq)]
enum Kind {
    InlineLatin1 {
        len: u8,
        words: [u64; 3],
    },
    Latin1(u8, Vec<u8>),
    Ucs2(u8, Vec<u16>),
    Ucs4(u8, Vec<u32>),
}

#[repr(C)]
#[derive(Debug)]
pub struct Text {
    kind: Kind,
    cached_hash: AtomicI64,
}

impl Clone for Text {
    fn clone(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            cached_hash: AtomicI64::new(self.cached_hash.load(AtomicOrdering::Relaxed)),
        }
    }
}

impl PartialEq for Text {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        match (&self.kind, &other.kind) {
            (
                Kind::InlineLatin1 {
                    len: len_a,
                    words: words_a,
                },
                Kind::InlineLatin1 {
                    len: len_b,
                    words: words_b,
                },
            ) => {
                len_a == len_b
                    && words_a[0] == words_b[0]
                    && (*len_a <= 8 || (words_a[1] == words_b[1] && words_a[2] == words_b[2]))
            }
            (Kind::Latin1(_, a), Kind::Latin1(_, b)) => a == b,
            (Kind::Ucs2(_, a), Kind::Ucs2(_, b)) => a == b,
            (Kind::Ucs4(_, a), Kind::Ucs4(_, b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for Text {}

/// A storage request exceeded the address space or the allocator failed.
#[derive(Debug)]
pub struct AllocationError;

fn reserve<T>(length: usize) -> Result<Vec<T>, AllocationError> {
    let mut values = Vec::new();
    values
        .try_reserve_exact(length)
        .map_err(|_| AllocationError)?;
    Ok(values)
}

#[inline]
fn pack_inline_latin1(slice: &[u8]) -> Kind {
    let len = slice.len();
    debug_assert!(len <= INLINE_LATIN1_CAP);
    let ptr = slice.as_ptr();
    let mut words = [0u64; 3];
    unsafe {
        if len >= 4 {
            if len <= 8 {
                let lo = u64::from(ptr.cast::<u32>().read_unaligned());
                let hi = u64::from(ptr.add(len - 4).cast::<u32>().read_unaligned());
                words[0] = lo | (hi << ((len - 4) * 8));
            } else if len <= 16 {
                words[0] = ptr.cast::<u64>().read_unaligned();
                let tail = ptr.add(len - 8).cast::<u64>().read_unaligned();
                words[1] = tail >> ((16 - len) * 8);
            } else {
                words[0] = ptr.cast::<u64>().read_unaligned();
                words[1] = ptr.add(8).cast::<u64>().read_unaligned();
                let tail = ptr.add(len - 8).cast::<u64>().read_unaligned();
                words[2] = tail >> ((24 - len) * 8);
            }
        } else if len >= 2 {
            let lo = u64::from(ptr.cast::<u16>().read_unaligned());
            let hi = u64::from(*ptr.add(len - 1));
            words[0] = lo | (hi << ((len - 1) * 8));
        } else if len == 1 {
            words[0] = u64::from(*ptr);
        }
    }
    Kind::InlineLatin1 {
        len: len as u8,
        words,
    }
}

#[inline]
fn latin1_kind_from_slice(slice: &[u8]) -> Result<Kind, AllocationError> {
    if slice.len() <= INLINE_LATIN1_CAP {
        return Ok(pack_inline_latin1(slice));
    }
    let mut values = reserve(slice.len())?;
    values.extend_from_slice(slice);
    Ok(Kind::Latin1(0, values))
}

impl Text {
    const fn from_kind(kind: Kind) -> Self {
        Self {
            kind,
            cached_hash: AtomicI64::new(-1),
        }
    }

    pub const fn empty() -> Self {
        Self::from_kind(Kind::InlineLatin1 {
            len: 0,
            words: [0u64; 3],
        })
    }

    #[inline]
    pub const fn is_inline(&self) -> bool {
        matches!(self.kind, Kind::InlineLatin1 { .. })
    }

    /// Store validated codepoints (each at most U+10FFFF) at their canonical width.
    pub fn from_codepoints(
        codes: impl ExactSizeIterator<Item = u32> + Clone,
    ) -> Result<Self, AllocationError> {
        let length = codes.len();
        let widest = codes.clone().max().unwrap_or(0);
        let kind = if widest <= 0xff {
            if length <= INLINE_LATIN1_CAP {
                let mut bytes = [0u8; INLINE_LATIN1_CAP];
                for (slot, code) in bytes.iter_mut().zip(codes) {
                    *slot = code as u8;
                }
                pack_inline_latin1(&bytes[..length])
            } else {
                let mut values = reserve(length)?;
                values.extend(codes.map(|code| code as u8));
                Kind::Latin1(0, values)
            }
        } else if widest <= 0xffff {
            let mut values = reserve(length)?;
            values.extend(codes.map(|code| code as u16));
            Kind::Ucs2(0, values)
        } else {
            let mut values = reserve(length)?;
            values.extend(codes);
            Kind::Ucs4(0, values)
        };
        Ok(Self::from_kind(kind))
    }

    pub fn from_rust_str(text: &str) -> Self {
        if text.is_ascii() {
            let kind =
                latin1_kind_from_slice(text.as_bytes()).expect("decoded text already fits in memory");
            return Self::from_kind(kind);
        }
        let codes: Vec<u32> = text.chars().map(u32::from).collect();
        Self::from_codepoints(codes.into_iter()).expect("decoded text already fits in memory")
    }

    /// Decode generalized UTF-8, which also encodes lone surrogates. Compiler
    /// literals use this form so that `"\ud800"` survives code generation.
    pub fn from_wtf8(data: &[u8]) -> Result<Self, &'static str> {
        if data.is_ascii() {
            let kind =
                latin1_kind_from_slice(data).map_err(|_| "text storage allocation failed")?;
            return Ok(Self::from_kind(kind));
        }
        let mut codes = Vec::with_capacity(data.len());
        let mut offset = 0;
        while offset < data.len() {
            let first = data[offset];
            offset += 1;
            if first < 0x80 {
                codes.push(u32::from(first));
                continue;
            }
            let (count, mut value, minimum) = match first {
                0xc2..=0xdf => (1, u32::from(first & 0x1f), 0x80),
                0xe0..=0xef => (2, u32::from(first & 0x0f), 0x800),
                0xf0..=0xf4 => (3, u32::from(first & 0x07), 0x10000),
                _ => return Err("invalid UTF-8 text"),
            };
            if data.len() - offset < count {
                return Err("incomplete UTF-8 text");
            }
            for _ in 0..count {
                let byte = data[offset];
                offset += 1;
                if byte & 0xc0 != 0x80 {
                    return Err("invalid UTF-8 text");
                }
                value = (value << 6) | u32::from(byte & 0x3f);
            }
            if value < minimum || value > 0x10ffff {
                return Err("invalid UTF-8 text");
            }
            codes.push(value);
        }
        Self::from_codepoints(codes.into_iter()).map_err(|_| "text storage allocation failed")
    }

    #[inline]
    pub fn from_latin1_slice(bytes: &[u8]) -> Result<Self, AllocationError> {
        Ok(Self::from_kind(latin1_kind_from_slice(bytes)?))
    }

    pub fn from_latin1(units: Vec<u8>) -> Self {
        if units.len() <= INLINE_LATIN1_CAP {
            return Self::from_kind(pack_inline_latin1(&units));
        }
        Self::from_kind(Kind::Latin1(0, units))
    }

    pub fn from_ucs2(units: Vec<u16>) -> Self {
        Self::from_kind(Kind::Ucs2(0, units))
    }

    pub fn from_ucs4(units: Vec<u32>) -> Self {
        Self::from_kind(Kind::Ucs4(0, units))
    }

    #[inline]
    pub fn len(&self) -> usize {
        match &self.kind {
            Kind::InlineLatin1 { len, .. } => usize::from(*len),
            Kind::Latin1(_, values) => values.len(),
            Kind::Ucs2(_, values) => values.len(),
            Kind::Ucs4(_, values) => values.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Bytes per codepoint: 1, 2, or 4, as `PyUnicode_KIND` reports.
    pub fn kind(&self) -> usize {
        match &self.kind {
            Kind::InlineLatin1 { .. } | Kind::Latin1(..) => 1,
            Kind::Ucs2(..) => 2,
            Kind::Ucs4(..) => 4,
        }
    }

    #[inline]
    pub fn as_latin1(&self) -> Option<&[u8]> {
        match &self.kind {
            Kind::InlineLatin1 { len, words } => unsafe {
                let n = usize::from(*len);
                std::hint::assert_unchecked(n <= INLINE_LATIN1_CAP);
                Some(std::slice::from_raw_parts(words.as_ptr().cast::<u8>(), n))
            },
            Kind::Latin1(_, values) => Some(values),
            _ => None,
        }
    }

    #[inline]
    pub fn get(&self, index: usize) -> Option<u32> {
        match &self.kind {
            Kind::InlineLatin1 { len, words } => {
                if index < usize::from(*len) {
                    Some(u32::from(unsafe { *words.as_ptr().cast::<u8>().add(index) }))
                } else {
                    None
                }
            }
            Kind::Latin1(_, values) => values.get(index).map(|code| u32::from(*code)),
            Kind::Ucs2(_, values) => values.get(index).map(|code| u32::from(*code)),
            Kind::Ucs4(_, values) => values.get(index).copied(),
        }
    }

    pub fn codepoints(&self) -> Codepoints<'_> {
        Codepoints {
            text: self,
            index: 0,
        }
    }

    /// Copy the codepoints in `start..stop`, narrowing to the canonical width.
    #[inline]
    pub fn slice(&self, start: usize, stop: usize) -> Result<Self, AllocationError> {
        if let Some(values) = self.as_latin1() {
            let stop = stop.min(values.len());
            let start = start.min(stop);
            return Self::from_latin1_slice(&values[start..stop]);
        }
        let stop = stop.min(self.len());
        let start = start.min(stop);
        let codes = Codepoints {
            text: self,
            index: start,
        }
        .take(stop - start);
        Self::from_codepoints(codes)
    }

    /// Compare `self[start..stop]` with `other` in-place without allocating.
    #[inline]
    pub fn slice_eq(&self, start: usize, stop: usize, other: &Self) -> bool {
        self.slice_range_eq(start, stop, other, 0, other.len())
    }

    /// Compare `self[start1..stop1]` with `other[start2..stop2]` in-place
    /// without allocating.
    #[inline]
    pub fn slice_range_eq(
        &self,
        start1: usize,
        stop1: usize,
        other: &Self,
        start2: usize,
        stop2: usize,
    ) -> bool {
        if let (Some(left_bytes), Some(right_bytes)) = (self.as_latin1(), other.as_latin1()) {
            let stop1 = stop1.min(left_bytes.len());
            let start1 = start1.min(stop1);
            let stop2 = stop2.min(right_bytes.len());
            let start2 = start2.min(stop2);
            return left_bytes[start1..stop1] == right_bytes[start2..stop2];
        }
        let stop1 = stop1.min(self.len());
        let start1 = start1.min(stop1);
        let stop2 = stop2.min(other.len());
        let start2 = start2.min(stop2);
        let len1 = stop1 - start1;
        let len2 = stop2 - start2;
        if len1 != len2 {
            return false;
        }
        Codepoints {
            text: self,
            index: start1,
        }
        .take(len1)
        .eq(Codepoints {
            text: other,
            index: start2,
        }
        .take(len2))
    }

    /// First codepoint index of `sub` within `start..end`, or `None`.
    #[inline]
    pub fn find(&self, sub: &Self, start: usize, end: usize) -> Option<usize> {
        if let (Some(haystack), Some(needle)) = (self.as_latin1(), sub.as_latin1()) {
            let end = end.min(haystack.len());
            if start > end {
                return None;
            }
            let sub_len = needle.len();
            if sub_len == 0 {
                return Some(start);
            }
            let slice = &haystack[start..end];
            if sub_len > slice.len() {
                return None;
            }
            let first = needle[0];
            if sub_len == 1 {
                return slice.iter().position(|&b| b == first).map(|pos| start + pos);
            }
            let rest = &needle[1..];
            let search_len = slice.len() - sub_len + 1;
            let mut offset = 0;
            while let Some(rel) = slice[offset..search_len].iter().position(|&b| b == first) {
                let pos = offset + rel;
                if &slice[pos + 1..pos + sub_len] == rest {
                    return Some(start + pos);
                }
                offset = pos + 1;
            }
            return None;
        }
        let end = end.min(self.len());
        if start > end {
            return None;
        }
        let sub_len = sub.len();
        if sub_len == 0 {
            return Some(start);
        }
        if sub_len > end - start || sub.kind() > self.kind() {
            return None;
        }
        let first = sub.get(0)?;
        for pos in start..=(end - sub_len) {
            if self.get(pos) == Some(first)
                && (1..sub_len).all(|i| self.get(pos + i) == sub.get(i))
            {
                return Some(pos);
            }
        }
        None
    }

    /// Last codepoint index of `sub` within `start..end`, or `None`.
    #[inline]
    pub fn rfind(&self, sub: &Self, start: usize, end: usize) -> Option<usize> {
        if let (Some(haystack), Some(needle)) = (self.as_latin1(), sub.as_latin1()) {
            let end = end.min(haystack.len());
            if start > end {
                return None;
            }
            let sub_len = needle.len();
            if sub_len == 0 {
                return Some(end);
            }
            let slice = &haystack[start..end];
            if sub_len > slice.len() {
                return None;
            }
            let first = needle[0];
            if sub_len == 1 {
                return slice.iter().rposition(|&b| b == first).map(|pos| start + pos);
            }
            let rest = &needle[1..];
            let mut limit = slice.len() - sub_len + 1;
            while let Some(pos) = slice[..limit].iter().rposition(|&b| b == first) {
                if &slice[pos + 1..pos + sub_len] == rest {
                    return Some(start + pos);
                }
                limit = pos;
            }
            return None;
        }
        let end = end.min(self.len());
        if start > end {
            return None;
        }
        let sub_len = sub.len();
        if sub_len == 0 {
            return Some(end);
        }
        if sub_len > end - start || sub.kind() > self.kind() {
            return None;
        }
        let first = sub.get(0)?;
        for pos in (start..=(end - sub_len)).rev() {
            if self.get(pos) == Some(first)
                && (1..sub_len).all(|i| self.get(pos + i) == sub.get(i))
            {
                return Some(pos);
            }
        }
        None
    }

    /// Concatenate pieces in order, allocating the result once.
    pub fn join<'a>(pieces: impl Iterator<Item = &'a Text> + Clone) -> Result<Self, AllocationError> {
        let mut length = 0usize;
        let mut kind = 1;
        for piece in pieces.clone() {
            length = length.checked_add(piece.len()).ok_or(AllocationError)?;
            kind = kind.max(piece.kind());
        }
        let storage = match kind {
            1 => {
                if length <= INLINE_LATIN1_CAP {
                    let mut bytes = [0u8; INLINE_LATIN1_CAP];
                    let mut cursor = 0;
                    for piece in pieces {
                        if let Some(slice) = piece.as_latin1() {
                            bytes[cursor..cursor + slice.len()].copy_from_slice(slice);
                            cursor += slice.len();
                        }
                    }
                    pack_inline_latin1(&bytes[..length])
                } else {
                    let mut values = reserve(length)?;
                    for piece in pieces {
                        if let Some(slice) = piece.as_latin1() {
                            values.extend_from_slice(slice);
                        }
                    }
                    Kind::Latin1(0, values)
                }
            }
            2 => {
                let mut values = reserve(length)?;
                for piece in pieces {
                    values.extend(piece.codepoints().map(|code| code as u16));
                }
                Kind::Ucs2(0, values)
            }
            _ => {
                let mut values = reserve(length)?;
                for piece in pieces {
                    values.extend(piece.codepoints());
                }
                Kind::Ucs4(0, values)
            }
        };
        Ok(Self::from_kind(storage))
    }

    /// Encode as UTF-8, failing at the first lone surrogate.
    pub fn to_utf8(&self) -> Result<Vec<u8>, usize> {
        if let Some(values) = self.as_latin1() {
            if values.is_ascii() {
                let mut bytes = Vec::with_capacity(values.len());
                bytes.extend_from_slice(values);
                return Ok(bytes);
            }
        }
        let mut bytes = Vec::with_capacity(self.len());
        for (index, code) in self.codepoints().enumerate() {
            let character = char::from_u32(code).ok_or(index)?;
            let mut buffer = [0; 4];
            bytes.extend_from_slice(character.encode_utf8(&mut buffer).as_bytes());
        }
        Ok(bytes)
    }

    /// Encode as generalized UTF-8, preserving lone surrogates.
    pub fn to_wtf8(&self) -> Vec<u8> {
        if let Some(values) = self.as_latin1() {
            if values.is_ascii() {
                let mut bytes = Vec::with_capacity(values.len());
                bytes.extend_from_slice(values);
                return bytes;
            }
        }
        let mut bytes = Vec::with_capacity(self.len());
        for code in self.codepoints() {
            append_wtf8(&mut bytes, code);
        }
        bytes
    }

    /// Compute or return the cached 63-bit fixnum hash without allocating a temporary buffer.
    pub fn hash(&self) -> i64 {
        let cached = self.cached_hash.load(AtomicOrdering::Relaxed);
        if cached != -1 {
            return cached;
        }
        let computed = if self.is_empty() {
            0
        } else {
            let mut state: u64 = 0xcbf2_9ce4_8422_2325;
            let mut feed = |byte: u8| {
                state ^= u64::from(byte);
                state = state.wrapping_mul(0x0000_0100_0000_01b3);
            };
            if let Some(values) = self.as_latin1() {
                for &code in values {
                    if code < 0x80 {
                        feed(code);
                    } else {
                        feed(0xc0 | (code >> 6));
                        feed(0x80 | (code & 0x3f));
                    }
                }
            } else {
                for code in self.codepoints() {
                    if code < 0x80 {
                        feed(code as u8);
                    } else if code < 0x800 {
                        feed((0xc0 | (code >> 6)) as u8);
                        feed((0x80 | (code & 0x3f)) as u8);
                    } else if code < 0x10000 {
                        feed((0xe0 | (code >> 12)) as u8);
                        feed((0x80 | ((code >> 6) & 0x3f)) as u8);
                        feed((0x80 | (code & 0x3f)) as u8);
                    } else {
                        feed((0xf0 | (code >> 18)) as u8);
                        feed((0x80 | ((code >> 12) & 0x3f)) as u8);
                        feed((0x80 | ((code >> 6) & 0x3f)) as u8);
                        feed((0x80 | (code & 0x3f)) as u8);
                    }
                }
            }
            let folded = (((state ^ (state >> 62)) as i64) << 1) >> 1;
            if folded == -1 { -2 } else { folded }
        };
        self.cached_hash.store(computed, AtomicOrdering::Relaxed);
        computed
    }

    /// A Rust string for identifiers and messages; `None` with surrogates.
    pub fn to_rust_string(&self) -> Option<String> {
        self.to_utf8()
            .ok()
            .map(|bytes| String::from_utf8(bytes).expect("encoded from scalar values"))
    }
}

impl Ord for Text {
    /// Lexicographic codepoint order, as Python compares `str`.
    #[inline]
    fn cmp(&self, other: &Self) -> Ordering {
        if let (Some(left), Some(right)) = (self.as_latin1(), other.as_latin1()) {
            if left.len() == 1 && right.len() == 1 {
                return left[0].cmp(&right[0]);
            }
            return left.cmp(right);
        }
        self.codepoints().cmp(other.codepoints())
    }
}

impl PartialOrd for Text {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Clone)]
pub struct Codepoints<'a> {
    text: &'a Text,
    index: usize,
}

impl Iterator for Codepoints<'_> {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        let code = self.text.get(self.index)?;
        self.index += 1;
        Some(code)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining = self.text.len().saturating_sub(self.index);
        (remaining, Some(remaining))
    }
}

impl ExactSizeIterator for Codepoints<'_> {}

/// Append one codepoint, including a lone surrogate, as generalized UTF-8.
pub fn append_wtf8(bytes: &mut Vec<u8>, code: u32) {
    debug_assert!(code <= 0x10ffff);
    if code < 0x80 {
        bytes.push(code as u8);
    } else if code < 0x800 {
        bytes.extend_from_slice(&[(0xc0 | (code >> 6)) as u8, (0x80 | (code & 0x3f)) as u8]);
    } else if code < 0x10000 {
        bytes.extend_from_slice(&[
            (0xe0 | (code >> 12)) as u8,
            (0x80 | ((code >> 6) & 0x3f)) as u8,
            (0x80 | (code & 0x3f)) as u8,
        ]);
    } else {
        bytes.extend_from_slice(&[
            (0xf0 | (code >> 18)) as u8,
            (0x80 | ((code >> 12) & 0x3f)) as u8,
            (0x80 | ((code >> 6) & 0x3f)) as u8,
            (0x80 | (code & 0x3f)) as u8,
        ]);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn width_is_the_narrowest_for_the_largest_codepoint() {
        for (codes, kind) in [
            (vec![], 1),
            (vec![0x41, 0xff], 1),
            (vec![0x41, 0x100], 2),
            (vec![0xd800], 2),
            (vec![0x41, 0x10000], 4),
        ] {
            let text = Text::from_codepoints(codes.clone().into_iter()).unwrap();
            assert_eq!(text.kind(), kind, "{codes:x?}");
            assert_eq!(text.codepoints().collect::<Vec<_>>(), codes);
        }
    }

    #[test]
    fn slices_and_joins_narrow_to_canonical_width() {
        let wide = Text::from_codepoints([0x61, 0x1f600, 0x62].into_iter()).unwrap();
        let narrow = wide.slice(2, 3).unwrap();
        assert_eq!(narrow, Text::from_rust_str("b"));
        let joined = Text::join([&Text::from_rust_str("a"), &narrow].into_iter()).unwrap();
        assert_eq!(joined, Text::from_rust_str("ab"));
        let mixed = Text::join([&Text::from_rust_str("é"), &wide].into_iter()).unwrap();
        assert_eq!(mixed.kind(), 4);
        assert_eq!(mixed.len(), 4);
    }

    #[test]
    fn order_compares_codepoints_across_widths() {
        let latin = Text::from_rust_str("\u{ff}");
        let bmp = Text::from_rust_str("\u{100}");
        let astral = Text::from_rust_str("\u{10000}");
        assert!(latin < bmp && bmp < astral);
        assert!(Text::from_rust_str("ab") < Text::from_rust_str("abc"));
    }

    #[test]
    fn wtf8_round_trips_surrogates_and_utf8_rejects_them() {
        let bytes = b"a\xed\xa0\x80\xf4\x8f\xbf\xbf";
        let text = Text::from_wtf8(bytes).unwrap();
        assert_eq!(text.codepoints().collect::<Vec<_>>(), [0x61, 0xd800, 0x10ffff]);
        assert_eq!(text.to_wtf8(), bytes);
        assert_eq!(text.to_utf8(), Err(1));
        assert!(Text::from_wtf8(b"\xc0\x80").is_err());
        assert!(Text::from_wtf8(b"\xe0\xa0").is_err());
    }
}
