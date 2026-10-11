"""json: dumps options (sort_keys, indent, separators, ensure_ascii), loads of nested data, round-tripping, custom default/object_hook/parse hooks, errors."""
import json
from decimal import Decimal

data = {"name": "Widget", "tags": ["a", "b"], "price": 9.5, "stock": 0, "active": True, "meta": None,
        "dims": {"w": 1, "h": 2.25}, "empty": {}, "list": []}
print("dumps", json.dumps(data))
print("sort_keys", json.dumps(data, sort_keys=True))
print("indent")
print(json.dumps({"b": [1, {"c": []}], "a": {}}, indent=2, sort_keys=True))
print("indent str")
print(json.dumps([1, [2]], indent="\t"))
print("compact", json.dumps(data["dims"], separators=(",", ":")))
print("custom seps", json.dumps([1, 2, {"k": 3}], separators=(" | ", " => ")))
print("ensure_ascii", json.dumps("café ☃"), json.dumps("café ☃", ensure_ascii=False))
print("escapes", json.dumps('quote " back \\ nl \n tab \t ctl \x01'))
print("non-str keys", json.dumps({1: "a", 2.5: "b", True: "c", None: "d"}))
print("tuple becomes list", json.dumps((1, (2, 3))))
print("floats", json.dumps([1.0, 1e100, 0.1, -0.0, 1e-7]))
print("nan inf", json.dumps([float("nan"), float("inf"), -float("inf")]))
try:
    json.dumps(float("nan"), allow_nan=False)
except ValueError as e:
    print("allow_nan False", type(e).__name__)
try:
    json.dumps({"s": {1, 2}})
except TypeError as e:
    print("unserializable", type(e).__name__)
try:
    json.dumps({(1, 2): 3})
except TypeError as e:
    print("tuple key", type(e).__name__)
print("skipkeys", json.dumps({(1, 2): 3, "ok": 4}, skipkeys=True))

text = '{"users": [{"id": 1, "roles": ["admin", "dev"], "score": 3.5}, {"id": 2, "roles": [], "score": null}], "count": 2, "ok": true}'
obj = json.loads(text)
print("loads", obj)
print("nested access", obj["users"][0]["roles"][1], obj["users"][1]["score"])
print("types", type(obj["count"]).__name__, type(obj["users"][0]["score"]).__name__, type(obj["ok"]).__name__)
print("big int", json.loads("123456789012345678901234567890"))
print("unicode escape", json.loads('"\\u00e9\\ud83d\\ude00"') == "é\U0001F600")
print("duplicate keys last wins", json.loads('{"a": 1, "a": 2}'))
print("whitespace", json.loads("  [ 1 ,\n 2 ]  "))
print("scalars", json.loads("null"), json.loads("true"), json.loads('"s"'), json.loads("-0.0"))
print("NaN literal", json.loads("[NaN, Infinity]"))
print("round trip", json.loads(json.dumps(data)) == data)
print("round trip tuple lossy", json.loads(json.dumps({"t": (1, 2)})))
print("round trip int keys", json.loads(json.dumps({1: "x"})))
for bad in ["{'a': 1}", "[1, 2,]", "", "{\"a\" 1}", "[1] x"]:
    try:
        json.loads(bad)
    except json.JSONDecodeError as e:
        print("decode error", repr(bad), type(e).__name__, isinstance(e, ValueError), e.pos)


class Point:
    def __init__(self, x, y):
        self.x, self.y = x, y

    def __repr__(self):
        return f"Point({self.x}, {self.y})"


def encode(o):
    if isinstance(o, Point):
        return {"__point__": [o.x, o.y]}
    if isinstance(o, Decimal):
        return str(o)
    if isinstance(o, set):
        return sorted(o)
    raise TypeError(f"cannot encode {type(o).__name__}")


def decode(d):
    if "__point__" in d:
        return Point(*d["__point__"])
    return d


payload = {"p": Point(1, 2), "price": Decimal("1.10"), "ids": {3, 1, 2}}
s = json.dumps(payload, default=encode, sort_keys=True)
print("default", s)
print("object_hook", json.loads(s, object_hook=decode))
try:
    json.dumps(object.__new__(type("Opaque", (), {})), default=encode)
except TypeError as e:
    print("default raises", e)


class PointEncoder(json.JSONEncoder):
    def default(self, o):
        if isinstance(o, Point):
            return [o.x, o.y]
        return super().default(o)


print("encoder subclass", json.dumps([Point(3, 4)], cls=PointEncoder))
print("parse_float", json.loads("[1.10, 2]", parse_float=Decimal))
print("parse_int", json.loads("[1, 2.5]", parse_int=lambda s: int(s) * 100))
print("parse_constant", json.loads("[NaN]", parse_constant=lambda c: c.lower()))
print("object_pairs_hook", json.loads('{"b": 1, "a": 2, "b": 3}', object_pairs_hook=list))
print("iterencode", "".join(json.JSONEncoder(sort_keys=True).iterencode({"z": 1, "a": [True]})))
print("raw_decode", json.JSONDecoder().raw_decode('{"a": 1} trailing'))
