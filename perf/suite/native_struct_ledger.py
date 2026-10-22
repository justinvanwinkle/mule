"""Simulate the account ledger using a Mule native struct class with int64 fields and speed=3, safety=0.

Computes the exact same transaction stream and ledger totals as account_ledger,
using a @optimize(speed=3, safety=0) native struct class with unboxed int64 fields.
"""

from perfutils import run_case

from mule import int64, optimize

__speed__ = 3
__safety__ = 0

NAME = "native_struct_ledger"
DESCRIPTION = "Native int64 struct account ledger with speed=3, safety=0"
DEFAULT_SIZE = 100_000
DEFAULT_SEED = 42


@optimize(speed=3, safety=0)
class LedgerAccount:
    ident: int64
    balance: int64
    tx_count: int64
    rate: int64
    points: int64
    floor: int64
    penalties: int64
    kind: int64

    @optimize(speed=3, safety=0)
    def __init__(
        self,
        ident: int64,
        balance: int64,
        rate: int64,
        floor: int64,
        kind: int64,
    ):
        self.ident = ident
        self.balance = balance
        self.tx_count = 0
        self.rate = rate
        self.points = 0
        self.floor = floor
        self.penalties = 0
        self.kind = kind

    @optimize(speed=3, safety=0)
    def deposit(self, amount: int64) -> int64:
        bal: int64 = self.balance + amount
        self.balance = bal
        self.tx_count = self.tx_count + 1
        if self.kind == 0:
            return bal
        pts: int64 = self.points + (amount * self.rate) // 10
        self.points = pts
        return bal + pts

    @optimize(speed=3, safety=0)
    def withdraw(self, amount: int64) -> int64:
        eff_amount: int64 = amount
        if self.kind == 2 and self.balance - amount < self.floor:
            self.penalties = self.penalties + 1
            eff_amount = 1
        if eff_amount <= self.balance:
            self.balance = self.balance - eff_amount
        else:
            self.balance = self.balance - 1
        self.tx_count = self.tx_count + 1
        return self.balance

    @optimize(speed=3, safety=0)
    def score(self) -> int64:
        base: int64 = self.balance * 3 + self.tx_count * 7 + self.ident
        if self.kind == 0:
            return base
        rewards: int64 = base + self.points * 5
        if self.kind == 1:
            return rewards
        return rewards - self.penalties * 11


def prepare(size: int, seed: int) -> list[tuple[int, int, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state = seed & 0x7FFFFFFF
    ops: list[tuple[int, int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        idx = (state >> 4) & 63
        amount = ((state >> 10) % 97) + 1
        kind = state & 3
        ops.append((idx, amount, kind))
    return ops


@optimize(speed=3, safety=0)
def run(data: list[tuple[int, int, int]]) -> tuple[int, int, int]:
    accounts: list[LedgerAccount] = []
    i: int64
    for i in range(64):
        acct_kind: int64 = i % 3
        initial: int64 = 500 + i * 10
        if acct_kind == 0:
            accounts.append(LedgerAccount(i, initial, 0, 0, 0))
        elif acct_kind == 1:
            accounts.append(LedgerAccount(i, initial, (i % 5) + 1, 0, 1))
        else:
            accounts.append(LedgerAccount(i, initial, (i % 4) + 1, 200, 2))

    n: int64 = len(data)
    rounds: int64 = max(1, n // 425)
    rolling: int64 = 0
    round_idx: int64
    for round_idx in range(rounds):
        bonus: int64 = round_idx & 7
        for idx, amount, kind in data:
            idx64: int64 = idx
            amount64: int64 = amount
            kind64: int64 = kind
            acc = accounts[(idx64 + round_idx) & 63]
            adj: int64 = amount64 + bonus
            if kind64 == 0:
                rolling = (rolling + acc.deposit(adj)) & 0x3FFFFFFF
            elif kind64 == 1:
                rolling = (rolling + acc.withdraw(adj)) & 0x3FFFFFFF
            else:
                rolling = (rolling + acc.score()) & 0x3FFFFFFF

    final_score: int64 = 0
    tx_total: int64 = 0
    for acc in accounts:
        final_score += acc.score()
        tx_total += acc.tx_count
    return rolling, final_score, tx_total


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
