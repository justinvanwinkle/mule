"""Builtin len() vs bound __len__() vs base-descriptor __len__() on builtin subclasses."""


class SizedList(list):
    def __len__(self):
        return list.__len__(self) + 100


class SizedTuple(tuple):
    def __len__(self):
        return tuple.__len__(self) + 200


class SizedDict(dict):
    def __len__(self):
        return dict.__len__(self) + 300


class SizedSet(set):
    def __len__(self):
        return set.__len__(self) + 400


class SizedStr(str):
    def __len__(self):
        return str.__len__(self) + 500


sl = SizedList([1, 2, 3])
st = SizedTuple((1, 2))
sd = SizedDict({"a": 1})
ss = SizedSet({1, 2, 3, 4})
sstr = SizedStr("abcde")

print("list_len:", len(sl), sl.__len__(), list.__len__(sl))
print("tuple_len:", len(st), st.__len__(), tuple.__len__(st))
print("dict_len:", len(sd), sd.__len__(), dict.__len__(sd))
print("set_len:", len(ss), ss.__len__(), set.__len__(ss))
print("str_len:", len(sstr), sstr.__len__(), str.__len__(sstr))
