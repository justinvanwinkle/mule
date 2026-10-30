"""ORM model hydration, custom data descriptors, and immutable record cloning workloads.

Covers descriptor protocol (__get__, __set__, __set_name__), lazy foreign key
resolution with an identity map, dirty-field diff generation, frozen dataclasses
with dataclasses.replace, slotted classes, @property, and namedtuple records.
"""

from collections import namedtuple
from dataclasses import dataclass, replace
import sys
import time
from typing import Any

DEFAULT_SIZE = 600
DEFAULT_LOOPS = 1

_USERNAMES = (
    "alice_v",
    "bob_builder",
    "carol_ops",
    "dave_sec",
    "erin_data",
    "frank_ml",
    "grace_compiler",
    "heidi_net",
)
_ROLES = ("admin", "maintainer", "developer", "auditor", "viewer", "billing")
_REGIONS = ("us-east-1", "us-west-2", "eu-central-1", "ap-northeast-1")
_STATUSES = ("pending", "paid", "shipped", "settled", "refunded")
_OPCODES = ("LOAD_CONST", "CALL_METHOD", "BINARY_ADD", "JUMP_IF_FALSE", "RETURN_VALUE")


class FieldDescriptor:
    def __init__(self, default: Any = None) -> None:
        self.default = default
        self.name = ""
        self.storage_name = ""

    def __set_name__(self, owner: type, name: str) -> None:
        self.name = name
        self.storage_name = "_field_" + name

    def validate(self, value: Any) -> Any:
        return value

    def __get__(self, instance: Any, owner: type | None = None) -> Any:
        if instance is None:
            return self
        return instance.__dict__.get(self.storage_name, self.default)

    def __set__(self, instance: Any, value: Any) -> None:
        validated = self.validate(value)
        old = instance.__dict__.get(self.storage_name, _MISSING)
        instance.__dict__[self.storage_name] = validated
        if old is not _MISSING and old != validated:
            instance._dirty_fields.add(self.name)


_MISSING = object()


class StringField(FieldDescriptor):
    def __init__(self, max_length: int = 128, strip: bool = True, default: str = "") -> None:
        super().__init__(default)
        self.max_length = max_length
        self.strip = strip

    def validate(self, value: Any) -> str:
        if not isinstance(value, str):
            value = str(value)
        if self.strip:
            value = value.strip()
        if len(value) > self.max_length:
            value = value[: self.max_length]
        return value


class IntField(FieldDescriptor):
    def __init__(self, min_value: int = 0, max_value: int = 1 << 30, default: int = 0) -> None:
        super().__init__(default)
        self.min_value = min_value
        self.max_value = max_value

    def validate(self, value: Any) -> int:
        ival = int(value)
        if ival < self.min_value:
            return self.min_value
        if ival > self.max_value:
            return self.max_value
        return ival


class FloatField(FieldDescriptor):
    def __init__(self, min_value: float = 0.0, precision: int = 4, default: float = 0.0) -> None:
        super().__init__(default)
        self.min_value = min_value
        self.precision = precision

    def validate(self, value: Any) -> float:
        fval = float(value)
        if fval < self.min_value:
            fval = self.min_value
        return round(fval, self.precision)


class NullableField(FieldDescriptor):
    def __init__(self, inner: FieldDescriptor) -> None:
        super().__init__(None)
        self.inner = inner

    def validate(self, value: Any) -> Any:
        if value is None:
            return None
        return self.inner.validate(value)


class ForeignKeyField:
    def __init__(self, target_table: str) -> None:
        self.target_table = target_table
        self.name = ""
        self.id_attr = ""
        self.cache_attr = ""

    def __set_name__(self, owner: type, name: str) -> None:
        self.name = name
        self.id_attr = name + "_id"
        self.cache_attr = "_fk_cache_" + name

    def __get__(self, instance: Any, owner: type | None = None) -> Any:
        if instance is None:
            return self
        cached = instance.__dict__.get(self.cache_attr)
        if cached is not None:
            return cached
        fk_id = instance.__dict__.get(self.id_attr)
        if fk_id is None or instance._identity_map is None:
            return None
        resolved = instance._identity_map.get((self.target_table, fk_id))
        if resolved is not None:
            instance.__dict__[self.cache_attr] = resolved
        return resolved

    def __set__(self, instance: Any, value: Any) -> None:
        if isinstance(value, Model):
            new_id = value.pk
            instance.__dict__[self.cache_attr] = value
        else:
            new_id = int(value) if value is not None else None
            instance.__dict__.pop(self.cache_attr, None)
        old_id = instance.__dict__.get(self.id_attr, _MISSING)
        instance.__dict__[self.id_attr] = new_id
        if old_id is not _MISSING and old_id != new_id:
            instance._dirty_fields.add(self.id_attr)


class Model:
    table_name = "model"

    def __init__(self, pk: int, identity_map: dict[tuple[str, int], "Model"] | None = None) -> None:
        object.__setattr__(self, "pk", pk)
        object.__setattr__(self, "_dirty_fields", set())
        object.__setattr__(self, "_write_count", 0)
        object.__setattr__(self, "_identity_map", identity_map)
        if identity_map is not None:
            identity_map[(self.table_name, pk)] = self

    def __setattr__(self, name: str, value: Any) -> None:
        object.__setattr__(self, "_write_count", self._write_count + 1)
        super().__setattr__(name, value)

    def dirty_diff(self) -> dict[str, Any]:
        diff: dict[str, Any] = {}
        for field_name in self._dirty_fields:
            if field_name.endswith("_id") and field_name in self.__dict__:
                diff[field_name] = self.__dict__[field_name]
            else:
                diff[field_name] = getattr(self, field_name)
        return diff

    def clear_dirty(self) -> None:
        self._dirty_fields.clear()


class UserAccount(Model):
    table_name = "users"
    username = StringField(max_length=32, strip=True)
    role = StringField(max_length=24, strip=True)
    region = StringField(max_length=24, strip=True)
    login_count = IntField(min_value=0, max_value=1_000_000)
    credit_limit = FloatField(min_value=0.0, precision=2)
    notes = NullableField(StringField(max_length=64, strip=True))

    def __init__(
        self,
        pk: int,
        username: str,
        role: str,
        region: str,
        login_count: int,
        credit_limit: float,
        notes: str | None,
        identity_map: dict[tuple[str, int], Model] | None = None,
    ) -> None:
        super().__init__(pk, identity_map)
        self.username = username
        self.role = role
        self.region = region
        self.login_count = login_count
        self.credit_limit = credit_limit
        self.notes = notes
        self.clear_dirty()


class OrderRecord(Model):
    table_name = "orders"
    status = StringField(max_length=16, strip=True)
    quantity = IntField(min_value=1, max_value=10_000)
    unit_price = FloatField(min_value=0.01, precision=2)
    discount_rate = NullableField(FloatField(min_value=0.0, precision=3))
    user = ForeignKeyField("users")

    def __init__(
        self,
        pk: int,
        user_id: int,
        status: str,
        quantity: int,
        unit_price: float,
        discount_rate: float | None,
        identity_map: dict[tuple[str, int], Model] | None = None,
    ) -> None:
        super().__init__(pk, identity_map)
        self.user = user_id
        self.status = status
        self.quantity = quantity
        self.unit_price = unit_price
        self.discount_rate = discount_rate
        self.clear_dirty()

    @property
    def net_total(self) -> float:
        gross = self.quantity * self.unit_price
        disc = self.discount_rate
        if disc is not None:
            gross *= 1.0 - disc
        return round(gross, 2)


SourceSpan = namedtuple("SourceSpan", ["file_id", "line", "col", "length"])


class RegisterSlot:
    __slots__ = ("reg_id", "width_bits", "is_spilled", "spill_offset")

    def __init__(self, reg_id: int, width_bits: int, is_spilled: bool, spill_offset: int) -> None:
        self.reg_id = reg_id
        self.width_bits = width_bits
        self.is_spilled = is_spilled
        self.spill_offset = spill_offset

    def clone_shifted(self, reg_delta: int, offset_delta: int) -> "RegisterSlot":
        return RegisterSlot(
            self.reg_id + reg_delta,
            self.width_bits,
            self.is_spilled or (offset_delta > 0),
            self.spill_offset + offset_delta,
        )

    @property
    def encoding_cost(self) -> int:
        base = self.width_bits >> 3
        if self.is_spilled:
            return base + 4 + (self.spill_offset & 0x1F)
        return base + (self.reg_id & 0x7)


@dataclass(frozen=True)
class IRInstructionNode:
    opcode: str
    dest: RegisterSlot
    src_regs: tuple[int, int]
    immediate: int
    span: SourceSpan

    @property
    def weight(self) -> int:
        return (
            len(self.opcode)
            + self.dest.encoding_cost
            + self.src_regs[0]
            + self.src_regs[1]
            + (self.immediate & 0xFF)
            + self.span.length
        )


@dataclass(frozen=True)
class IRBlockConfig:
    block_id: int
    loop_depth: int
    hotness: int
    instructions: tuple[IRInstructionNode, ...]
    succ_blocks: tuple[int, int]

    @property
    def total_weight(self) -> int:
        acc = self.loop_depth * 16 + self.hotness
        for inst in self.instructions:
            acc += inst.weight
        return acc


UserRow = tuple[int, str, str, str, int, float, str | None]
OrderRow = tuple[int, int, str, int, float, float | None]


def prepare(size: int) -> tuple[list[UserRow], list[OrderRow], list[IRBlockConfig]]:
    user_rows: list[UserRow] = []
    order_rows: list[OrderRow] = []
    ir_blocks: list[IRBlockConfig] = []

    state = 981273645
    num_users = max(4, size // 3)
    for uid in range(1, num_users + 1):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        uname = "  " + _USERNAMES[state % len(_USERNAMES)] + f"_{uid}  "
        role = _ROLES[(state >> 4) % len(_ROLES)]
        region = _REGIONS[(state >> 8) % len(_REGIONS)]
        logins = (state >> 12) % 500
        credit = float(((state >> 16) % 20000) + 500) / 10.0
        notes = f"tier_{(state >> 20) & 7}" if (state & 3) != 0 else None
        user_rows.append((uid, uname, role, region, logins, credit, notes))

    for oid in range(1, size + 1):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        uid = (state % num_users) + 1
        status = _STATUSES[(state >> 5) % len(_STATUSES)]
        qty = ((state >> 9) % 25) + 1
        price = float(((state >> 14) % 5000) + 100) / 100.0
        disc = round(((state >> 19) % 30) / 100.0, 3) if (state & 1) == 0 else None
        order_rows.append((oid, uid, status, qty, price, disc))

    for bid in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        insts: list[IRInstructionNode] = []
        for idx in range(4):
            state = (state * 1103515245 + 12345) & 0x7FFFFFFF
            op = _OPCODES[state % len(_OPCODES)]
            slot = RegisterSlot(
                reg_id=(state >> 3) & 15,
                width_bits=64 if (state & 4) else 32,
                is_spilled=((state >> 7) & 3) == 0,
                spill_offset=((state >> 9) & 31) * 8,
            )
            span = SourceSpan(
                file_id=bid & 7,
                line=10 + idx * 3 + (bid & 63),
                col=(state >> 14) & 31,
                length=((state >> 19) & 15) + 2,
            )
            insts.append(
                IRInstructionNode(
                    opcode=op,
                    dest=slot,
                    src_regs=((state >> 5) & 15, (state >> 11) & 15),
                    immediate=(state >> 15) & 0x3FF,
                    span=span,
                )
            )
        ir_blocks.append(
            IRBlockConfig(
                block_id=bid,
                loop_depth=(bid & 3) + 1,
                hotness=(state >> 8) & 0xFF,
                instructions=tuple(insts),
                succ_blocks=((bid + 1) % max(1, size), (bid + 2) % max(1, size)),
            )
        )

    return user_rows, order_rows, ir_blocks


def scenario_model_hydration_and_validation(
    user_rows: list[UserRow], order_rows: list[OrderRow]
) -> tuple[int, int, int, int]:
    """Hydrate ORM models into an identity map, resolve lazy FKs, mutate, and diff."""
    hydrated_count = 0
    fk_resolutions = 0
    dirty_fields_total = 0
    revenue_cents = 0

    for cycle in range(2410):
        identity_map: dict[tuple[str, int], Model] = {}
        orders: list[OrderRecord] = []

        for uid, uname, role, region, logins, credit, notes in user_rows:
            UserAccount(uid, uname, role, region, logins, credit, notes, identity_map)
            hydrated_count += 1

        for oid, uid, status, qty, price, disc in order_rows:
            order = OrderRecord(oid, uid, status, qty, price, disc, identity_map)
            orders.append(order)
            hydrated_count += 1

        num_users = len(user_rows)
        for idx, order in enumerate(orders):
            owner = order.user
            if owner is not None:
                fk_resolutions += 1
                owner.login_count = owner.login_count + 1
                if (idx + cycle) % 3 == 0:
                    order.quantity = order.quantity + 2
                    order.status = "settled"
                if (idx + cycle) % 5 == 0:
                    order.discount_rate = 0.125
                    owner.credit_limit = owner.credit_limit + 25.5
                if (idx + cycle) % 7 == 0:
                    next_uid = ((owner.pk + cycle) % num_users) + 1
                    order.user = identity_map[("users", next_uid)]
                    fk_resolutions += 1

            revenue_cents += int(round(order.net_total * 100.0))
            diff = order.dirty_diff()
            dirty_fields_total += len(diff) + (order._write_count & 0xF)
            order.clear_dirty()

        for key, model in identity_map.items():
            if key[0] == "users":
                udiff = model.dirty_diff()
                dirty_fields_total += len(udiff)

    return hydrated_count, fk_resolutions, dirty_fields_total, revenue_cents


def scenario_immutable_record_cloning(
    ir_blocks: list[IRBlockConfig],
) -> tuple[int, int, int, int]:
    """Clone and transform frozen dataclass IR trees, slotted registers, and namedtuples."""
    total_weight = 0
    cloned_nodes = 0
    span_checksum = 0
    hot_blocks = 0

    for pass_idx in range(1905):
        reg_shift = (pass_idx & 3) + 1
        imm_delta = pass_idx * 7
        for block in ir_blocks:
            new_insts: list[IRInstructionNode] = []
            for inst in block.instructions:
                new_slot = inst.dest.clone_shifted(reg_shift, pass_idx & 4)
                new_span = inst.span._replace(
                    line=inst.span.line + pass_idx,
                    col=(inst.span.col + reg_shift) & 63,
                )
                span_checksum += new_span.line + new_span.col + new_span.length
                cloned_inst = replace(
                    inst,
                    dest=new_slot,
                    src_regs=(inst.src_regs[0] + reg_shift, inst.src_regs[1] + 1),
                    immediate=inst.immediate + imm_delta,
                    span=new_span,
                )
                new_insts.append(cloned_inst)
                cloned_nodes += 1

            cloned_block = replace(
                block,
                loop_depth=block.loop_depth + (pass_idx & 1),
                hotness=block.hotness + reg_shift * 3,
                instructions=tuple(new_insts),
            )
            w = cloned_block.total_weight
            total_weight += w
            if cloned_block.hotness >= 120:
                hot_blocks += 1

    return total_weight, cloned_nodes, span_checksum, hot_blocks


def main() -> None:
    size = DEFAULT_SIZE
    loops = DEFAULT_LOOPS
    scenario = "all"
    args = sys.argv[1:]
    i = 0
    while i < len(args):
        if args[i] == "--size" and i + 1 < len(args):
            size = int(args[i + 1])
            i += 2
        elif args[i] == "--loops" and i + 1 < len(args):
            loops = int(args[i + 1])
            i += 2
        elif args[i] == "--scenario" and i + 1 < len(args):
            scenario = args[i + 1]
            i += 2
        else:
            raise ValueError(f"Unknown argument: {args[i]}")

    t0 = time.perf_counter()
    user_rows, order_rows, ir_blocks = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    orm_s = 0.0
    clone_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "model_hydration_and_validation"):
            s0 = time.perf_counter()
            r1 = scenario_model_hydration_and_validation(user_rows, order_rows)
            orm_s += time.perf_counter() - s0
        if scenario in ("all", "immutable_record_cloning"):
            s1 = time.perf_counter()
            r2 = scenario_immutable_record_cloning(ir_blocks)
            clone_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (orm_s + clone_s) / loops
    print("topic\tobjects/orm_and_descriptors")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "model_hydration_and_validation"):
        print(f"model_hydration_and_validation\t{orm_s / loops:.6f}")
    if scenario in ("all", "immutable_record_cloning"):
        print(f"immutable_record_cloning\t{clone_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
