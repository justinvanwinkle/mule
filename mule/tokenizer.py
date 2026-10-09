
_sentinal = object()


class PeekableIt:
    def __init__(self, it):
        self.it = it
        self.peeked = _sentinal

    def __iter__(self):
        return self

    def __next__(self):
        peeked = self.peeked
        if peeked is not _sentinal:
            self.peeked = _sentinal
            return peeked
        return next(self.it)

    @property
    def peek(self):
        self.peeked = next(self)
        return self.peeked


class NameReader:
    def match(self, it):
        if it.peek.isalpha():
            return True
        return False

    def read(self, it):
        s = ''
        while it.peek.isalnum():
            s += next(it)
        return s


class StringReader:
    def match(self, it):
        return it.peek in ('"', "'")

    def read(self, it):
        start_tok = next(it)
        if it.peek == start_tok:
            pass
        for _ in range(2):
            if


class Tokenizer:
    def __init__(self):
        pass

    def tokenize(self, f):
        it = PeekableIt(f)
