"""Bounded text content constraints, separate from identity and representation."""

from dataclasses import dataclass
from typing import TypedDict

AFFIX_LIMIT = 5


class TextContentData(TypedDict):
    min_length: int
    max_length: int | None
    prefix: str
    suffix: str


def _common_prefix(left: str, right: str) -> str:
    end = 0
    while end < min(len(left), len(right)) and left[end] == right[end]:
        end += 1
    return left[:end]


def _feasible_lengths(lo: int, hi: int | None, prefix: str, suffix: str) -> list[int]:
    # Beyond twice the affix limit, neither end can constrain the other end.
    # One representative covers every such length, even for enormous bounds.
    separate = 2 * AFFIX_LIMIT
    stop = separate if hi is None else min(separate, hi + 1)
    lengths = []
    for length in range(lo, stop):
        overlap = len(prefix) + len(suffix) - length
        if overlap <= 0 or prefix[-overlap:] == suffix[:overlap]:
            lengths.append(length)
    representative = max(lo, separate)
    if hi is None or representative <= hi:
        lengths.append(representative)
    return lengths


def _character_at(length: int, index: int, prefix: str, suffix: str) -> str | None:
    if index < len(prefix):
        return prefix[index]
    if index >= length - len(suffix):
        return suffix[index - length + len(suffix)]
    return None


def _guaranteed_end(
    lengths: list[int], prefix: str, suffix: str, *, from_end: bool
) -> str:
    result = []
    for offset in range(min(AFFIX_LIMIT, lengths[0])):
        characters = [
            _character_at(
                length, length - 1 - offset if from_end else offset, prefix, suffix
            )
            for length in lengths
        ]
        character = characters[0]
        if character is None or any(other != character for other in characters[1:]):
            break
        result.append(character)
    return "".join(reversed(result) if from_end else result)


@dataclass(frozen=True, slots=True)
class TextContent:
    """A nonempty set of strings constrained by lengths and guaranteed ends.

    ``meet`` returns ``None`` for the empty set. Affixes contain Unicode
    characters, not bytes. No field makes an object identity or storage claim.
    """

    min_length: int = 0
    max_length: int | None = None
    prefix: str = ""
    suffix: str = ""

    def __post_init__(self):
        if self.min_length < 0:
            raise ValueError("minimum text length must be non-negative")
        if len(self.prefix) > AFFIX_LIMIT or len(self.suffix) > AFFIX_LIMIT:
            raise ValueError(
                f"text affixes must contain at most {AFFIX_LIMIT} characters"
            )
        lo = max(self.min_length, len(self.prefix), len(self.suffix))
        hi = self.max_length
        if hi is not None and hi < lo:
            raise ValueError("text length bounds and affixes have no common value")
        lengths = _feasible_lengths(lo, hi, self.prefix, self.suffix)
        if not lengths:
            raise ValueError(
                "text prefix and suffix conflict at every permitted length"
            )
        prefix = _guaranteed_end(lengths, self.prefix, self.suffix, from_end=False)
        suffix = _guaranteed_end(lengths, self.prefix, self.suffix, from_end=True)
        object.__setattr__(self, "min_length", lengths[0])
        if hi is not None and hi < 2 * AFFIX_LIMIT:
            object.__setattr__(self, "max_length", lengths[-1])
        object.__setattr__(self, "prefix", prefix)
        object.__setattr__(self, "suffix", suffix)

    @classmethod
    def from_literal(cls, value: str) -> TextContent:
        return cls(len(value), len(value), value[:AFFIX_LIMIT], value[-AFFIX_LIMIT:])

    def matches(self, value: str) -> bool:
        return (
            len(value) >= self.min_length
            and (self.max_length is None or len(value) <= self.max_length)
            and value.startswith(self.prefix)
            and value.endswith(self.suffix)
        )

    def fits(self, other: TextContent) -> bool:
        return (
            self.min_length >= other.min_length
            and (
                other.max_length is None
                or (self.max_length is not None and self.max_length <= other.max_length)
            )
            and self.prefix.startswith(other.prefix)
            and self.suffix.endswith(other.suffix)
        )

    def join(self, other: TextContent) -> TextContent:
        if self.fits(other):
            return other
        if other.fits(self):
            return self
        hi = (
            None
            if self.max_length is None or other.max_length is None
            else max(self.max_length, other.max_length)
        )
        return TextContent(
            min(self.min_length, other.min_length),
            hi,
            _common_prefix(self.prefix, other.prefix),
            _common_prefix(self.suffix[::-1], other.suffix[::-1])[::-1],
        )

    def meet(self, other: TextContent) -> TextContent | None:
        if self.fits(other):
            return self
        if other.fits(self):
            return other
        prefix, other_prefix = sorted((self.prefix, other.prefix), key=len)
        suffix, other_suffix = sorted((self.suffix, other.suffix), key=len)
        if not other_prefix.startswith(prefix) or not other_suffix.endswith(suffix):
            return None
        hi = self.max_length
        if hi is None:
            hi = other.max_length
        elif other.max_length is not None:
            hi = min(hi, other.max_length)
        try:
            return TextContent(
                max(self.min_length, other.min_length), hi, other_prefix, other_suffix
            )
        except ValueError:
            # Both operands are valid. Only an empty intersection can fail.
            return None

    def widen(self, new: TextContent) -> TextContent:
        joined = self.join(new)
        lo = 0 if joined.min_length < self.min_length else self.min_length
        hi = self.max_length
        if hi is not None and (joined.max_length is None or joined.max_length > hi):
            hi = None
        return TextContent(lo, hi, joined.prefix, joined.suffix)

    def to_dict(self) -> TextContentData:
        return {
            "min_length": self.min_length,
            "max_length": self.max_length,
            "prefix": self.prefix,
            "suffix": self.suffix,
        }

    @classmethod
    def from_dict(cls, data: TextContentData) -> TextContent:
        return cls(
            data["min_length"], data["max_length"], data["prefix"], data["suffix"]
        )
