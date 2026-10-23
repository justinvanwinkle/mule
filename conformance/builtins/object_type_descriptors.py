"""Object and type constructors, base descriptors, MRO, and receiver checks."""

o1 = object()
o2 = object()
print("object_type:", type(o1) is object, isinstance(o1, object), issubclass(type, object))
print("type_meta:", type(object) is type, type(type) is type, isinstance(object, type))
print("object_eq:", o1 == o1, o1 == o2, o1 != o1, o1 != o2)
print(
    "object_eq_descriptor:",
    object.__eq__(o1, o1),
    object.__eq__(o1, o2) is NotImplemented,
    object.__ne__(o1, o1),
    object.__ne__(o1, o2) is NotImplemented,
)

for args, kwargs in (((1,), {}), ((), {"x": 1})):
    try:
        object(*args, **kwargs)
    except TypeError as e:
        print("object_excess_args:", type(e).__name__)

try:
    object.__setattr__(o1, "attr", 1)
except AttributeError as e:
    print("plain_object_setattr:", type(e).__name__)


class Box:
    label = "box"

    def __init__(self, value):
        self.value = value


b = Box(42)
print(
    "object_getattribute:",
    object.__getattribute__(b, "value"),
    object.__getattribute__(b, "label"),
)
object.__setattr__(b, "extra", 99)
print("after_setattr:", b.extra)
object.__delattr__(b, "extra")
try:
    object.__getattribute__(b, "extra")
except AttributeError as e:
    print("after_delattr:", type(e).__name__)

try:
    object.__getattribute__(b, 123)
except TypeError as e:
    print("non_str_attr:", type(e).__name__)

Dynamic = type(
    "Dynamic",
    (Box,),
    {"factor": 3, "scale": lambda self: self.value * self.factor},
)
dyn = Dynamic(7)
print("dynamic_type:", Dynamic.__name__, dyn.scale(), isinstance(dyn, Box))
print("mro_names:", [cls.__name__ for cls in type.mro(Dynamic)])
print("mro_tuple:", [cls.__name__ for cls in Dynamic.__mro__])
print("subclasses:", [cls.__name__ for cls in Box.__subclasses__()])

type.__setattr__(Dynamic, "factor", 5)
print("type_setattr:", type.__getattribute__(Dynamic, "factor"), dyn.scale())
type.__delattr__(Dynamic, "factor")
try:
    type.__getattribute__(Dynamic, "factor")
except AttributeError as e:
    print("type_delattr:", type(e).__name__)

for fn in (type.mro, type.__subclasses__):
    try:
        fn(123)
    except TypeError as e:
        print("invalid_type_receiver:", type(e).__name__)
