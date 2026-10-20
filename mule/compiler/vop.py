"""IR2 structures, following ``src/compiler/vop.lisp``.

IR2 describes the same flow graph as IR1 in terms of virtual operations
(``VOP``) over temporary names (``TN``). A VOP is an instance of a
``Template``: the backend's description of one operation, with restrictions
on where its operands may live and a generator that emits the LLVM text. A TN
is a value container with a primitive type (what values it may hold) and,
after representation selection, a storage class (how it is held).

Python and LLVM adaptations, recorded in ``spec/python-changes.md``:

- Storage classes describe representations, not registers. LLVM allocates
  registers; Mule only decides whether a value is a tagged object pointer
  that owns a reference, an immortal tagged pointer, or a raw machine value.
- A ``TN`` of descriptor storage owns one runtime reference and is backed by
  an entry-block slot so the shared error path can release it.
- ``IR2Lvar.locs`` holds one TN per value an lvar delivers. Lvars carry a
  single Python object, except that a resolved method carries its receiver
  as a second location, the way SBCL delivers multiple values.
"""

from __future__ import annotations

import itertools
from collections import defaultdict
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import TYPE_CHECKING

from .boxing import IntegerPayload, ScalarPayload

if TYPE_CHECKING:
    from .ltn import EntryContext
    from .node import Combination, Lambda
    from .policy import OptimizationPolicy

_numbers = itertools.count(1)


def _number() -> int:
    return next(_numbers)


@dataclass(frozen=True, slots=True)
class StorageClass:
    """One way to hold a value: its LLVM type, zero value, and whether a
    container of this class owns a runtime reference (``descriptor``)."""

    name: str
    llvm_type: str
    zero: str
    owned: bool = False
    # A machine representation rather than an object pointer.
    raw: bool = False

    def __repr__(self):
        return f"<SC {self.name}>"


# Object pointers that own a reference and must be released.
DESCRIPTOR = StorageClass("descriptor", "ptr", "null", owned=True)
# Tagged fixnums, ``True``/``False``/``None``: pointers nothing ever releases.
IMMEDIATE = StorageClass("immediate", "ptr", "null")
# Raw machine values.
SIGNED = StorageClass("signed", "i64", "0", raw=True)
BOOLEAN = StorageClass("boolean", "i1", "false", raw=True)
DOUBLE = StorageClass("double", "double", "0.0", raw=True)
# A borrowed pointer to the original native instance object.
INSTANCE = StorageClass("instance", "ptr", "null", raw=True)
# The value ``None`` held as nothing at all.
NULL = StorageClass("null", "i64", "0", raw=True)
# A borrowed pointer to a native class object whose layout was guarded at entry.
CLASS = StorageClass("class", "ptr", "null", raw=True)
# Counted-loop iteration state: current value, stop, and step words.
RANGE = StorageClass("range", "[3 x i64]", "zeroinitializer")


@dataclass(frozen=True, slots=True)
class BoxLayout:
    """The active fields and payload lifetime of a fixed-layout box.

    Payload codecs initialize, extract, and release any owned storage. Private
    ownership must still exclude weak references and escaping aliases.
    """

    name: str
    tag: int
    storage: StorageClass
    payload_offset: int
    null_pointer_offsets: tuple[int, ...]
    codec: ScalarPayload | IntegerPayload = field(default_factory=ScalarPayload)

    def symbol(self, operation: str = "") -> str:
        return f"mule.box.{self.name}" + (f".{operation}" if operation else "")


FLOAT_BOX_LAYOUT = BoxLayout("f64", 3, DOUBLE, 24, (32, 40))
INT64_BOX_LAYOUT = BoxLayout("i64", 2, SIGNED, 24, (40, 48), IntegerPayload())
BOX_LAYOUTS = (FLOAT_BOX_LAYOUT, INT64_BOX_LAYOUT)


@dataclass(frozen=True, slots=True)
class PrimitiveType:
    """The values a TN may hold and the storage classes that can hold them,
    cheapest first (``primitive-type-scs``)."""

    name: str
    scs: tuple[StorageClass, ...]
    # Representation-selection data: the native class layout for instances.
    layout: object = field(default=None, compare=False)
    # Object entries without coercion VOPs must respect these implicit boundaries.
    object_boundary_scs: tuple[StorageClass, ...] | None = None
    boxed_layout: BoxLayout | None = None

    def __repr__(self):
        return f"<PrimitiveType {self.name}>"

    def admits(self, sc: StorageClass) -> bool:
        return sc in self.scs

    def operand_restriction(self, restriction, *, raw):
        if restriction is None and not raw:
            return self.object_boundary_scs
        return restriction


# Any Python object.
T = PrimitiveType("t", (DESCRIPTOR,))
# A tagged fixnum: immortal as a pointer, or a raw machine word.
FIXNUM = PrimitiveType("fixnum", (IMMEDIATE, SIGNED))
# ``True`` or ``False``.
BOOLEAN_T = PrimitiveType("boolean", (IMMEDIATE, BOOLEAN))
# ``None``.
NONE_T = PrimitiveType("none", (IMMEDIATE, NULL))
# Any immortal object: fixnum, boolean, or ``None``.
IMMORTAL_T = PrimitiveType("immortal", (IMMEDIATE,))
# A machine integer that may exceed the fixnum range (``int64`` contracts).
SIGNED64 = PrimitiveType(
    "signed64", (SIGNED, DESCRIPTOR), boxed_layout=INT64_BOX_LAYOUT
)
BOXED_I64 = PrimitiveType(
    "boxed-i64",
    (DESCRIPTOR, SIGNED),
    object_boundary_scs=(DESCRIPTOR,),
    boxed_layout=INT64_BOX_LAYOUT,
)
# A binary64 float (``float``/``float64`` contracts): raw, or a boxed object.
FLOAT64 = PrimitiveType("float64", (DOUBLE, DESCRIPTOR), boxed_layout=FLOAT_BOX_LAYOUT)
# A proven Python float whose object identity and ownership are preserved.
BOXED_FLOAT = PrimitiveType(
    "boxed-float",
    (DOUBLE, DESCRIPTOR),
    object_boundary_scs=(DESCRIPTOR,),
    boxed_layout=FLOAT_BOX_LAYOUT,
)
# Loop state.
RANGE_T = PrimitiveType("range-state", (RANGE,))
# A native class object: owned, or borrowed once its layout is guarded.
CLASS_T = PrimitiveType("class", (DESCRIPTOR, CLASS))


def instance_type(layout) -> PrimitiveType:
    """An instance of a native class with ``layout``: owned descriptor or a
    borrowed payload pointer."""
    return PrimitiveType(f"instance:{layout.index}", (DESCRIPTOR, INSTANCE), layout)


class TNKind(Enum):
    NORMAL = auto()
    CONSTANT = auto()


class TN:
    """A temporary name: one value container of the IR2 program."""

    __slots__ = (
        "kind",
        "leaf",
        "name",
        "number",
        "primitive_type",
        "private_box",
        "sc",
        "slot",
        "type",
    )

    def __init__(
        self, primitive_type: PrimitiveType, *, leaf=None, name=None, kind=None
    ):
        self.primitive_type = primitive_type
        self.sc: StorageClass | None = None
        # A whole-copy-group proof permits non-atomic local ownership operations.
        self.private_box: BoxLayout | None = None
        self.leaf = leaf
        self.name = name
        self.kind = kind or TNKind.NORMAL
        # The derived value type, when flow analysis established one.
        self.type = None
        # Assigned by code generation: the entry-block slot holding the value.
        self.slot: str | None = None
        self.number = _number()

    def __repr__(self):
        label = self.name or self.number
        sc = f" {self.sc.name}" if self.sc is not None else ""
        return f"<TN {label} {self.primitive_type.name}{sc}>"


class TNRef:
    """A VOP operand: a read or write of a TN."""

    __slots__ = ("tn", "write_p")

    def __init__(self, tn: TN, write_p: bool):
        self.tn = tn
        self.write_p = write_p

    def __repr__(self):
        return f"<TNRef {'write' if self.write_p else 'read'} {self.tn!r}>"


class VOP:
    """One instance of a template in an IR2 block."""

    __slots__ = ("args", "codegen_info", "info", "node", "number", "results")

    def __init__(self, info: Template, node, args: list[TNRef], results: list[TNRef]):
        self.info = info
        self.node = node
        self.args = args
        self.results = results
        self.codegen_info = None
        self.number = _number()

    def __repr__(self):
        return f"<VOP {self.info.name} {self.number}>"


@dataclass(slots=True, eq=False)
class Template:
    """A backend operation description (SBCL's ``template``/``vop-info``).

    ``args`` and ``results`` list an operand restriction each: the storage
    classes the operand may occupy, or ``None`` for any. ``arg_types`` and
    ``result_types`` give the primitive types the translation requires from
    the IR1 derived types, which local template selection checks. ``policy``
    is a predicate on the optimization policy, and ``guard`` an extra
    predicate on the combination (such as a required constant operand).
    ``generator`` emits the code for one VOP.
    """

    name: str
    generator: object
    args: tuple = ()
    results: tuple = ()
    arg_types: tuple = ()
    result_types: tuple = ()
    cost: int = 0
    policy: Callable[[OptimizationPolicy], bool] | None = None
    guard: Callable[[Combination], bool] | None = None
    translate: object = None
    # Which entry contexts may select the template (SBCL's ``ltn-policy``):
    # ``None`` for bodies over object representations, ``"raw"`` for native
    # bodies whose plan assigns machine representations.
    ltn_policy: str | None = None
    # A result with a new object identity, independent of operand identities.
    fresh_result: bool = False
    # Unspecified operands may retain or publish their references.
    arg_escapes: tuple[bool, ...] | None = None
    # Such generators require codegen_info.reuse_local when info is present.
    reuses_local: bool = False

    def captures_argument(self, index: int, *, vop: VOP | None = None) -> bool:
        # Reuse can forward the left operand through the result. Without the
        # operation's reuse facts, preserve the capturing answer.
        if (
            self.reuses_local
            and index == 0
            and (vop is None or not self.produces_fresh(vop))
        ):
            return True
        return (
            self.arg_escapes is None
            or index >= len(self.arg_escapes)
            or self.arg_escapes[index]
        )

    def produces_fresh(self, vop: VOP) -> bool:
        return self.fresh_result and not (
            self.reuses_local
            and vop.codegen_info is not None
            and vop.codegen_info.reuse_local is not None
        )


class IR2Block:
    """The VOP sequence for one IR1 block."""

    __slots__ = ("block", "label", "number", "vops")

    def __init__(self, block, number: int):
        self.block = block
        self.vops: list[VOP] = []
        self.number = number
        self.label = f"block.{number}"

    def append(self, vop: VOP) -> VOP:
        self.vops.append(vop)
        return vop

    def __repr__(self):
        return f"<IR2Block {self.number}>"


class IR2LvarKind(Enum):
    FIXED = auto()
    DELAYED = auto()


@dataclass(slots=True, eq=False)
class IR2Lvar:
    """Delivery information for one lvar: the TNs holding its value(s)."""

    kind: IR2LvarKind
    primitive_type: PrimitiveType
    locs: list[TN] = field(default_factory=list)


@dataclass(slots=True, eq=False)
class IR2Environment:
    """Code generation data for one lambda body in one entry context:
    the TN of every local, the hidden locals holding captured cells and
    copies, and the loop states."""

    lambda_: Lambda
    context: EntryContext
    vars: dict[object, TN] = field(default_factory=dict)
    cells: dict[object, TN] = field(default_factory=dict)
    copies: dict[object, TN] = field(default_factory=dict)
    captured: list = field(default_factory=list)
    home_cells: list = field(default_factory=list)
    ranges: dict[int, TN] = field(default_factory=dict)
    return_tn: TN | None = None


@dataclass(slots=True, eq=False)
class IR2Component:
    """The IR2 of one lambda in one entry context: its blocks in emission
    order, every TN, and the environment."""

    lambda_: Lambda
    context: EntryContext
    environment: IR2Environment
    blocks: list[IR2Block] = field(default_factory=list)
    tns: list[TN] = field(default_factory=list)
    ir2_blocks: dict[object, IR2Block] = field(default_factory=dict)
    reads: dict[TN, list[tuple[VOP, TNRef]]] = field(
        default_factory=lambda: defaultdict(list)
    )
    writes: dict[TN, list[tuple[VOP, TNRef]]] = field(
        default_factory=lambda: defaultdict(list)
    )

    def tn(self, primitive_type, **kwargs) -> TN:
        tn = TN(primitive_type, **kwargs)
        self.tns.append(tn)
        return tn


__all__ = [
    "BOOLEAN",
    "BOOLEAN_T",
    "CLASS",
    "CLASS_T",
    "DESCRIPTOR",
    "DOUBLE",
    "FIXNUM",
    "FLOAT64",
    "FLOAT_BOX_LAYOUT",
    "IMMEDIATE",
    "IMMORTAL_T",
    "INSTANCE",
    "NONE_T",
    "NULL",
    "RANGE",
    "RANGE_T",
    "SIGNED",
    "SIGNED64",
    "TN",
    "VOP",
    "BoxLayout",
    "IR2Block",
    "IR2Component",
    "IR2Environment",
    "IR2Lvar",
    "IR2LvarKind",
    "PrimitiveType",
    "StorageClass",
    "T",
    "TNKind",
    "TNRef",
    "Template",
    "instance_type",
]
