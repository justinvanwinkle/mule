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

#[derive(Clone, Debug, PartialEq, Eq)]
enum Kind {
    Latin1(Vec<u8>),
    Ucs2(Vec<u16>),
    Ucs4(Vec<u32>),
}

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
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
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

impl Text {
    const fn from_kind(kind: Kind) -> Self {
        Self {
            kind,
            cached_hash: AtomicI64::new(-1),
        }
    }

    pub const fn empty() -> Self {
        Self::from_kind(Kind::Latin1(Vec::new()))
    }

    /// Store validated codepoints (each at most U+10FFFF) at their canonical width.
    pub fn from_codepoints(
        codes: impl ExactSizeIterator<Item = u32> + Clone,
    ) -> Result<Self, AllocationError> {
        let length = codes.len();
        let widest = codes.clone().max().unwrap_or(0);
        let kind = if widest <= 0xff {
            let mut values = reserve(length)?;
            values.extend(codes.map(|code| code as u8));
            Kind::Latin1(values)
        } else if widest <= 0xffff {
            let mut values = reserve(length)?;
            values.extend(codes.map(|code| code as u16));
            Kind::Ucs2(values)
        } else {
            let mut values = reserve(length)?;
            values.extend(codes);
            Kind::Ucs4(values)
        };
        Ok(Self::from_kind(kind))
    }

    pub fn from_rust_str(text: &str) -> Self {
        if text.is_ascii() {
            let mut values = reserve(text.len()).expect("decoded text already fits in memory");
            values.extend_from_slice(text.as_bytes());
            return Self::from_kind(Kind::Latin1(values));
        }
        let codes: Vec<u32> = text.chars().map(u32::from).collect();
        Self::from_codepoints(codes.into_iter()).expect("decoded text already fits in memory")
    }

    /// Decode generalized UTF-8, which also encodes lone surrogates. Compiler
    /// literals use this form so that `"\ud800"` survives code generation.
    pub fn from_wtf8(data: &[u8]) -> Result<Self, &'static str> {
        if data.is_ascii() {
            let mut values = reserve(data.len()).map_err(|_| "text storage allocation failed")?;
            values.extend_from_slice(data);
            return Ok(Self::from_kind(Kind::Latin1(values)));
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

    pub fn len(&self) -> usize {
        match &self.kind {
            Kind::Latin1(values) => values.len(),
            Kind::Ucs2(values) => values.len(),
            Kind::Ucs4(values) => values.len(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Bytes per codepoint: 1, 2, or 4, as `PyUnicode_KIND` reports.
    pub fn kind(&self) -> usize {
        match &self.kind {
            Kind::Latin1(_) => 1,
            Kind::Ucs2(_) => 2,
            Kind::Ucs4(_) => 4,
        }
    }

    pub fn get(&self, index: usize) -> Option<u32> {
        match &self.kind {
            Kind::Latin1(values) => values.get(index).map(|code| u32::from(*code)),
            Kind::Ucs2(values) => values.get(index).map(|code| u32::from(*code)),
            Kind::Ucs4(values) => values.get(index).copied(),
        }
    }

    pub fn codepoints(&self) -> Codepoints<'_> {
        Codepoints {
            text: self,
            index: 0,
        }
    }

    /// Copy the codepoints in `start..stop`, narrowing to the canonical width.
    pub fn slice(&self, start: usize, stop: usize) -> Result<Self, AllocationError> {
        let stop = stop.min(self.len());
        let start = start.min(stop);
        if let Kind::Latin1(values) = &self.kind {
            let mut result = reserve(stop - start)?;
            result.extend_from_slice(&values[start..stop]);
            return Ok(Self::from_kind(Kind::Latin1(result)));
        }
        let codes = Codepoints {
            text: self,
            index: start,
        }
        .take(stop - start);
        Self::from_codepoints(codes)
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
                let mut values = reserve(length)?;
                for piece in pieces {
                    if let Kind::Latin1(piece) = &piece.kind {
                        values.extend_from_slice(piece);
                    }
                }
                Kind::Latin1(values)
            }
            2 => {
                let mut values = reserve(length)?;
                for piece in pieces {
                    values.extend(piece.codepoints().map(|code| code as u16));
                }
                Kind::Ucs2(values)
            }
            _ => {
                let mut values = reserve(length)?;
                for piece in pieces {
                    values.extend(piece.codepoints());
                }
                Kind::Ucs4(values)
            }
        };
        Ok(Self::from_kind(storage))
    }

    /// Encode as UTF-8, failing at the first lone surrogate.
    pub fn to_utf8(&self) -> Result<Vec<u8>, usize> {
        if let Kind::Latin1(values) = &self.kind {
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
        if let Kind::Latin1(values) = &self.kind {
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
            match &self.kind {
                Kind::Latin1(values) => {
                    for &code in values {
                        if code < 0x80 {
                            feed(code);
                        } else {
                            feed(0xc0 | (code >> 6));
                            feed(0x80 | (code & 0x3f));
                        }
                    }
                }
                _ => {
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
    fn cmp(&self, other: &Self) -> Ordering {
        match (&self.kind, &other.kind) {
            (Kind::Latin1(left), Kind::Latin1(right)) => left.cmp(right),
            _ => self.codepoints().cmp(other.codepoints()),
        }
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
