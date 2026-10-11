"""collections.OrderedDict (move_to_end, popitem last=False, equality semantics) and ChainMap (lookups, writes, new_child, parents)."""
from collections import OrderedDict, ChainMap

od = OrderedDict.fromkeys("abcde")
od.move_to_end("b")
print("move_to_end", list(od))
od.move_to_end("d", last=False)
print("move_to_end first", list(od))
print("popitem last", od.popitem())
print("popitem first", od.popitem(last=False))
print("remaining", list(od))
try:
    od.move_to_end("zz")
except KeyError as e:
    print("move missing", type(e).__name__)
try:
    OrderedDict().popitem()
except KeyError as e:
    print("popitem empty", type(e).__name__)

a = OrderedDict([("x", 1), ("y", 2)])
b = OrderedDict([("y", 2), ("x", 1)])
print("od == od order-sensitive", a == b)
print("od == dict order-insensitive", a == dict(b), dict(a) == b)
print("dict == dict", dict(a) == dict(b))
print("reversed", list(reversed(a)))
a["x"] = 100
print("update keeps position", list(a.items()))
del a["x"]
a["x"] = 1
print("reinsert goes to end", list(a.items()))
print("repr", a)
print("or", a | {"z": 0})


class LRU(OrderedDict):
    def __init__(self, cap):
        super().__init__()
        self.cap = cap

    def get_(self, k):
        self.move_to_end(k)
        return self[k]

    def put(self, k, v):
        self[k] = v
        self.move_to_end(k)
        if len(self) > self.cap:
            self.popitem(last=False)


lru = LRU(2)
lru.put("a", 1)
lru.put("b", 2)
lru.get_("a")
lru.put("c", 3)
print("lru", list(lru.items()))

defaults = {"color": "red", "size": "M", "user": "anon"}
env = {"size": "L"}
cli = {"user": "joe"}
cm = ChainMap(cli, env, defaults)
print("lookup", cm["color"], cm["size"], cm["user"])
print("len/keys", len(cm), sorted(cm))
print("get missing", cm.get("nope", "dflt"), "nope" in cm)
cm["color"] = "blue"
print("write goes to first map", cli, defaults["color"])
del cm["user"]
print("delete from first", cli, cm["user"])
try:
    del cm["size"]
except KeyError as e:
    print("delete only in first", type(e).__name__)
child = cm.new_child({"size": "XL"})
print("new_child", child["size"], cm["size"], len(child.maps))
child["extra"] = 1
print("child write isolated", "extra" in cm, child.maps[0])
print("parents", child.parents["size"], child.parents.maps[0] is cli)
empty_child = cm.new_child()
print("new_child empty", empty_child.maps[0])
print("as dict", sorted(dict(cm).items()))
print("iteration order", list(ChainMap({"b": 1}, {"a": 2, "b": 3})))
cm.maps[1]["size"] = "S"
print("live view", cm["size"])
print("chainmap eq", ChainMap({"a": 1}) == {"a": 1})
try:
    ChainMap({}, {"k": 1}).pop("k")
except KeyError as e:
    print("pop only first", type(e).__name__)
