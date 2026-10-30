"""Dynamic telemetry monkey-patching, context managers, and MRO middleware workloads.

Covers wrapper closures inspecting *args/**kwargs, bounded ring-buffer span recording,
10-layer cooperative multiple inheritance (super()), @property descriptors, __getattr__
fallbacks, dynamic getattr hook dispatch, and 4-frame custom exception handling.
"""

import sys
import time

DEFAULT_SIZE = 2000
DEFAULT_LOOPS = 1


class SpanRecord:
    def __init__(
        self,
        service_name: str,
        method_name: str,
        depth: int,
        start_tick: int,
        end_tick: int,
        arg_sig: int,
        status_code: int,
    ) -> None:
        self.service_name = service_name
        self.method_name = method_name
        self.depth = depth
        self.start_tick = start_tick
        self.end_tick = end_tick
        self.arg_sig = arg_sig
        self.status_code = status_code


class TelemetryRingBuffer:
    def __init__(self, capacity: int) -> None:
        self.capacity = capacity
        self.buffer: list[SpanRecord | None] = [None] * capacity
        self.write_pos = 0
        self.total_recorded = 0
        self.clock = 1000
        self.active_depth = 0
        self.exported_batches = 0
        self.exported_latency_sum = 0

    def next_tick(self, delta: int) -> int:
        self.clock += delta
        return self.clock

    def record(
        self,
        service_name: str,
        method_name: str,
        depth: int,
        start_tick: int,
        end_tick: int,
        arg_sig: int,
        status_code: int,
    ) -> None:
        rec = SpanRecord(
            service_name,
            method_name,
            depth,
            start_tick,
            end_tick,
            arg_sig,
            status_code,
        )
        self.buffer[self.write_pos] = rec
        self.write_pos = (self.write_pos + 1) % self.capacity
        self.total_recorded += 1
        if (self.total_recorded & 63) == 0:
            self.export_sampled(sample_mod=4)

    def export_sampled(self, sample_mod: int) -> int:
        self.exported_batches += 1
        acc = 0
        for idx, rec in enumerate(self.buffer):
            if rec is not None and (idx % sample_mod) == 0:
                dur = rec.end_tick - rec.start_tick
                acc += dur * (rec.depth + 1) + (rec.arg_sig & 0xFF) + rec.status_code
        self.exported_latency_sum = (self.exported_latency_sum + acc) & 0x7FFFFFFF
        return acc


class _SpanTrackerContext:
    def __init__(
        self,
        ring: TelemetryRingBuffer,
        service_name: str,
        method_name: str,
        arg_sig: int,
    ) -> None:
        self.ring = ring
        self.service_name = service_name
        self.method_name = method_name
        self.arg_sig = arg_sig
        self.depth = 0
        self.start_tick = 0
        self.status_code = 200

    def __enter__(self) -> "_SpanTrackerContext":
        self.ring.active_depth += 1
        self.depth = self.ring.active_depth
        self.start_tick = self.ring.next_tick(3)
        return self

    def __exit__(self, exc_type: object, exc_val: object, exc_tb: object) -> bool:
        if exc_type is not None:
            self.status_code = 500
        end_tick = self.ring.next_tick(5 + self.depth)
        self.ring.record(
            self.service_name,
            self.method_name,
            self.depth,
            self.start_tick,
            end_tick,
            self.arg_sig,
            self.status_code,
        )
        self.ring.active_depth -= 1
        return False


def span_tracker(
    ring: TelemetryRingBuffer, service_name: str, method_name: str, arg_sig: int
) -> _SpanTrackerContext:
    return _SpanTrackerContext(ring, service_name, method_name, arg_sig)


def _inspect_call_signature(args: tuple[object, ...], kwargs: dict[str, object]) -> int:
    sig = len(args) * 19 + len(kwargs) * 37
    for a in args:
        tname = type(a).__name__
        sig = (sig * 31 + len(tname) * 7 + ord(tname[0])) & 0x7FFFFF
    for k, v in kwargs.items():
        vname = type(v).__name__
        sig = (sig * 29 + len(k) * 11 + len(vname) * 5) & 0x7FFFFF
    return sig


def make_telemetry_wrapper(
    ring: TelemetryRingBuffer, service_name: str, method_name: str, orig_fn: object
) -> object:
    def _wrapped(*args: object, **kwargs: object) -> object:
        arg_sig = _inspect_call_signature(args, kwargs)
        with span_tracker(ring, service_name, method_name, arg_sig) as span:
            res = orig_fn(*args, **kwargs)  # type: ignore[operator]
            if res is None:
                span.status_code = 204
            return res

    return _wrapped


class AuthService:
    def __init__(self, secret_salt: int) -> None:
        self.secret_salt = secret_salt

    def verify_token(self, user_id: int, token: str, *, strict: bool = True) -> int:
        base = (user_id * 31 + len(token) * 17 + self.secret_salt) & 0xFFFF
        if strict and (base & 3) == 0:
            return base ^ 0x5A5A
        return base

    def resolve_scopes(self, role: str, tenant_id: int = 1) -> tuple[str, int]:
        mask = (len(role) * 13 + tenant_id * 7 + self.secret_salt) & 0xFF
        return (role + ":read", mask)


class QuotaService:
    def __init__(self, base_limit: int) -> None:
        self.base_limit = base_limit
        self.counters: dict[int, int] = {}

    def check_and_consume(
        self, tenant_id: int, cost: int, *, burst_multiplier: int = 2
    ) -> int:
        used = self.counters.get(tenant_id, 0) + cost
        self.counters[tenant_id] = used
        cap = self.base_limit * burst_multiplier
        return cap - (used % cap)


class PricingService:
    def __init__(self, tax_bps: int) -> None:
        self.tax_bps = tax_bps

    def compute_quote(
        self,
        unit_cents: int,
        quantity: int,
        *,
        discount_code: str = "",
        tier_bonus: int = 0,
    ) -> int:
        subtotal = unit_cents * quantity
        if discount_code:
            subtotal -= (len(discount_code) * 15) % (subtotal + 1)
        tax = (subtotal * self.tax_bps) // 10000
        return subtotal + tax - tier_bonus


class OrderProcessor:
    def __init__(
        self,
        auth: AuthService,
        quota: QuotaService,
        pricing: PricingService,
    ) -> None:
        self.auth = auth
        self.quota = quota
        self.pricing = pricing

    def execute_order(
        self,
        user_id: int,
        tenant_id: int,
        token: str,
        role: str,
        unit_cents: int,
        quantity: int,
        *,
        discount: str = "PROMO",
    ) -> int:
        tok_hash = self.auth.verify_token(user_id, token, strict=(quantity > 2))
        _scope_label, scope_mask = self.auth.resolve_scopes(role, tenant_id=tenant_id)
        rem_quota = self.quota.check_and_consume(
            tenant_id, quantity + (scope_mask & 7), burst_multiplier=3
        )
        total_cents = self.pricing.compute_quote(
            unit_cents,
            quantity,
            discount_code=discount,
            tier_bonus=(tok_hash & 0x1F),
        )
        return (total_cents + rem_quota + tok_hash) & 0x7FFFFFFF


def _install_telemetry_patches(
    ring: TelemetryRingBuffer,
    auth: AuthService,
    quota: QuotaService,
    pricing: PricingService,
    processor: OrderProcessor,
) -> dict[tuple[object, str], object]:
    targets: list[tuple[object, str, str]] = [
        (auth, "AuthService", "verify_token"),
        (auth, "AuthService", "resolve_scopes"),
        (quota, "QuotaService", "check_and_consume"),
        (pricing, "PricingService", "compute_quote"),
        (processor, "OrderProcessor", "execute_order"),
    ]
    originals: dict[tuple[object, str], object] = {}
    for instance, svc_name, attr_name in targets:
        orig_bound = getattr(instance, attr_name)
        originals[(instance, attr_name)] = orig_bound
        patched = make_telemetry_wrapper(ring, svc_name, attr_name, orig_bound)
        setattr(instance, attr_name, patched)
    return originals


def _restore_telemetry_patches(originals: dict[tuple[object, str], object]) -> None:
    for (instance, attr_name), orig_bound in originals.items():
        setattr(instance, attr_name, orig_bound)


# ==============================================================================
# Scenario 2: 10-Layer Cooperative MRO Middleware & 4-Frame Domain Exceptions
# ==============================================================================


class DomainError(Exception):
    def __init__(self, code: int, detail: str, retry_after: int = 0) -> None:
        super().__init__(detail)
        self.code = code
        self.detail = detail
        self.retry_after = retry_after


class ValidationError(DomainError):
    def __init__(self, field: str, reason: str) -> None:
        super().__init__(422, field + ":" + reason, retry_after=0)
        self.field = field


class RateLimitError(DomainError):
    def __init__(self, tenant: int, window_sec: int) -> None:
        super().__init__(429, "rate_limit:" + str(tenant), retry_after=window_sec)
        self.tenant = tenant


class AuthExpiredError(DomainError):
    def __init__(self, session_epoch: int) -> None:
        super().__init__(401, "expired:" + str(session_epoch), retry_after=1)
        self.session_epoch = session_epoch


class RequestEnvelope:
    def __init__(
        self,
        req_id: int,
        kind: str,
        tenant_id: int,
        payload_size: int,
        auth_epoch: int,
        flags: int,
    ) -> None:
        self._req_id = req_id
        self._kind = kind
        self._tenant_id = tenant_id
        self._payload_size = payload_size
        self._auth_epoch = auth_epoch
        self._flags = flags
        self._headers: dict[str, int] = {
            "x_trace": (req_id * 17) & 0xFFFF,
            "x_priority": (flags & 7) + 1,
        }

    @property
    def kind(self) -> str:
        return self._kind

    @property
    def tenant_id(self) -> int:
        return self._tenant_id

    @property
    def effective_weight(self) -> int:
        return self._payload_size * (self._headers["x_priority"]) + (self._flags & 0xF)

    @property
    def auth_epoch(self) -> int:
        return self._auth_epoch


class MiddlewareContext:
    """Provides dynamic attribute fallback via __getattr__ and property descriptors."""

    def __init__(self, base_epoch: int, quota_cap: int) -> None:
        self._base_epoch = base_epoch
        self._quota_cap = quota_cap
        self.hops = 0
        self.audit_trail = 0
        self.recovered_errors = 0
        self._dynamic_attrs: dict[str, int] = {
            "default_timeout_ms": 250,
            "circuit_threshold": 12,
            "canary_weight": 5,
            "compression_level": 4,
            "sanitize_mask": 0x3F,
        }

    @property
    def current_epoch(self) -> int:
        return self._base_epoch + (self.hops // 4)

    @property
    def remaining_budget(self) -> int:
        return self._quota_cap - (self.hops * 3)

    def record_hop(self, layer_tag: int) -> None:
        self.hops += 1
        self.audit_trail = (self.audit_trail * 31 + layer_tag) & 0x7FFFFFFF

    def __getattr__(self, name: str) -> int:
        val = self._dynamic_attrs.get(name)
        if val is not None:
            return val
        if name.startswith("policy_"):
            computed = (len(name) * 19 + self._base_epoch) & 0xFF
            self._dynamic_attrs[name] = computed
            return computed
        raise AttributeError(name)


class BaseHandlerLayer:
    """Root of the 10-layer cooperative MRO hierarchy; dispatches dynamic hooks."""

    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(1)
        hook_name = "handle_" + req.kind
        hook = getattr(self, hook_name, self.handle_fallback)
        return hook(req, ctx)

    def _validate_frame_4(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        if (req._flags & 0x1F) == 7:
            raise ValidationError("payload_size", "unaligned_frame")
        if (req._flags & 0x1F) == 13:
            raise RateLimitError(req.tenant_id, window_sec=15)
        if (req._flags & 0x1F) == 21:
            raise AuthExpiredError(req.auth_epoch)
        return req.effective_weight + ctx.default_timeout_ms

    def _validate_frame_3(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_4(req, ctx) + ctx.compression_level

    def _validate_frame_2(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_3(req, ctx) ^ ctx.sanitize_mask

    def _validate_frame_1(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_2(req, ctx) + ctx.policy_routing

    def handle_OrderCreate(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_1(req, ctx) + 101

    def handle_PaymentCapture(
        self, req: RequestEnvelope, ctx: MiddlewareContext
    ) -> int:
        return self._validate_frame_1(req, ctx) + 202

    def handle_InventorySync(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_1(req, ctx) + 303

    def handle_ProfileAudit(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_1(req, ctx) + 404

    def handle_fallback(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        return self._validate_frame_1(req, ctx) + 505


class SerializationLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(2)
        res = super().process(req, ctx)
        return res + (ctx.compression_level * 3)


class SanitizationLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(3)
        res = super().process(req, ctx)
        return res ^ (ctx.sanitize_mask & 0x1F)


class AuditLogLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(4)
        res = super().process(req, ctx)
        return res + (ctx.policy_audit & 0x3F)


class RoutingPolicyLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(5)
        res = super().process(req, ctx)
        return res + ctx.canary_weight


class QuotaEnforcementLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(6)
        if ctx.remaining_budget < -50000:
            raise RateLimitError(req.tenant_id, window_sec=30)
        return super().process(req, ctx) + (req.tenant_id & 0xF)


class SessionAuthLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(7)
        res = super().process(req, ctx)
        return res + (ctx.current_epoch & 0xFF)


class CircuitBreakerLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(8)
        res = super().process(req, ctx)
        return res + ctx.circuit_threshold


class MetricsTaggingLayer(BaseHandlerLayer):
    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(9)
        res = super().process(req, ctx)
        return res + req._headers["x_trace"]


class EdgeGatewayPipeline(
    MetricsTaggingLayer,
    CircuitBreakerLayer,
    SessionAuthLayer,
    QuotaEnforcementLayer,
    RoutingPolicyLayer,
    AuditLogLayer,
    SanitizationLayer,
    SerializationLayer,
):
    """10th class in the cooperative MRO diamond; catches domain exceptions."""

    def process(self, req: RequestEnvelope, ctx: MiddlewareContext) -> int:
        ctx.record_hop(10)
        try:
            return super().process(req, ctx)
        except ValidationError as exc:
            ctx.recovered_errors += 1
            return exc.code + len(exc.field) * 7
        except RateLimitError as exc:
            ctx.recovered_errors += 1
            return exc.code + exc.retry_after * 11 + (exc.tenant & 0xFF)
        except AuthExpiredError as exc:
            ctx.recovered_errors += 1
            return exc.code + (exc.session_epoch & 0xFF) * 5


_REQUEST_KINDS = (
    "OrderCreate",
    "PaymentCapture",
    "InventorySync",
    "ProfileAudit",
    "WebhookDispatch",
)


def prepare(
    size: int,
) -> tuple[list[tuple[int, int, str, str, int, int]], list[RequestEnvelope]]:
    orders: list[tuple[int, int, str, str, int, int]] = []
    envelopes: list[RequestEnvelope] = []
    roles = ("admin", "editor", "viewer", "billing", "service")
    tokens = ("tok_alpha_991", "tok_beta_4420", "tok_gamma_18", "tok_delta_77321")

    state = 44192837
    for i in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        user_id = 1000 + (state & 0xFFF)
        tenant_id = 1 + ((state >> 8) & 0x3F)
        tok = tokens[(state >> 12) % len(tokens)]
        role = roles[(state >> 15) % len(roles)]
        unit_cents = 150 + ((state >> 18) & 0x3FF)
        qty = 1 + ((state >> 24) & 7)
        orders.append((user_id, tenant_id, tok, role, unit_cents, qty))

        kind = _REQUEST_KINDS[i % len(_REQUEST_KINDS)]
        payload_sz = 64 + ((state >> 10) & 0x1FF)
        auth_epoch = 500 + (i & 0x7F)
        if i % 5 == 1:
            flags = 7
        elif i % 5 == 3:
            flags = 13
        elif i % 5 == 4:
            flags = 21
        else:
            flags = ((state >> 5) & 0x1F) | 1
        envelopes.append(
            RequestEnvelope(i, kind, tenant_id, payload_sz, auth_epoch, flags)
        )

    return orders, envelopes


def scenario_telemetry_monkey_patching(
    orders: list[tuple[int, int, str, str, int, int]],
) -> tuple[int, int, int, int]:
    """Execute service pipeline with dynamic monkey-patching, ring buffer, and span_tracker."""
    ring = TelemetryRingBuffer(capacity=128)
    auth = AuthService(secret_salt=901)
    quota = QuotaService(base_limit=5000)
    pricing = PricingService(tax_bps=825)
    processor = OrderProcessor(auth, quota, pricing)

    order_checksum = 0
    patch_toggle_count = 0

    for cycle in range(730):
        originals = _install_telemetry_patches(ring, auth, quota, pricing, processor)
        patch_toggle_count += len(originals)
        disc = "VIP25" if (cycle & 1) == 0 else ""
        for user_id, tenant_id, tok, role, unit_cents, qty in orders:
            res: int = processor.execute_order(
                user_id,
                tenant_id,
                tok,
                role,
                unit_cents,
                qty,
                discount=disc,
            )
            order_checksum = (order_checksum + res) & 0x7FFFFFFF
        _restore_telemetry_patches(originals)

    return (
        order_checksum,
        ring.total_recorded,
        ring.exported_batches,
        ring.exported_latency_sum + patch_toggle_count,
    )


def scenario_middleware_and_exceptions(
    envelopes: list[RequestEnvelope],
) -> tuple[int, int, int, int]:
    """Run 10-layer cooperative MRO middleware with descriptors, __getattr__, and exceptions."""
    pipeline = EdgeGatewayPipeline()
    ctx = MiddlewareContext(base_epoch=1200, quota_cap=500000)
    response_sum = 0

    for _ in range(1380):
        for req in envelopes:
            val = pipeline.process(req, ctx)
            response_sum = (response_sum + val) & 0x7FFFFFFF

    return response_sum, ctx.hops, ctx.recovered_errors, ctx.audit_trail


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
    orders, envelopes = prepare(size)
    prep_s = time.perf_counter() - t0

    expected = None
    tel_s = 0.0
    mid_s = 0.0
    for _ in range(loops):
        r1: tuple[int, ...] = ()
        r2: tuple[int, ...] = ()
        if scenario in ("all", "telemetry_monkey_patching"):
            s0 = time.perf_counter()
            r1 = scenario_telemetry_monkey_patching(orders)
            tel_s += time.perf_counter() - s0
        if scenario in ("all", "middleware_and_exceptions"):
            s1 = time.perf_counter()
            r2 = scenario_middleware_and_exceptions(envelopes)
            mid_s += time.perf_counter() - s1
        combined = r1 + r2
        if expected is not None and combined != expected:
            raise AssertionError("Result mismatch across loops")
        expected = combined

    total_s = (tel_s + mid_s) / loops
    print("topic\tdispatch/telemetry_and_middleware")
    print(f"size\t{size}")
    print(f"loops\t{loops}")
    print(f"result\t{expected}")
    print(f"prepare\t{prep_s:.6f}")
    if scenario in ("all", "telemetry_monkey_patching"):
        print(f"telemetry_monkey_patching\t{tel_s / loops:.6f}")
    if scenario in ("all", "middleware_and_exceptions"):
        print(f"middleware_and_exceptions\t{mid_s / loops:.6f}")
    print(f"total\t{total_s:.6f}")


if __name__ == "__main__":
    main()
