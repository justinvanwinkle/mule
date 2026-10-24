"""Simulate a polymorphic account ledger with Mule int64 annotations and speed=3, safety=0.

Transactions are generated from the runtime seed and applied across a rotating
pool of Account, RewardsAccount, and VaultAccount instances.
"""

from perfutils import run_case

from mule_types import int64, optimize

__speed__ = 3
__safety__ = 0
__debug_level__ = 0

NAME = "account_ledger_typed"
DESCRIPTION = "Polymorphic class hierarchy with int64 annotations and speed=3, safety=0"
DEFAULT_SIZE = 100_000
DEFAULT_SEED = 42


@optimize(speed=3, safety=0, debug=0)
class Account:
    @optimize(speed=3, safety=0, debug=0)
    def __init__(self, ident: int64, balance: int64):
        self.ident = ident
        self.balance = balance
        self.tx_count = 0

    @optimize(speed=3, safety=0, debug=0)
    def deposit(self, amount: int64) -> int64:
        self.balance = self.balance + amount
        self.tx_count = self.tx_count + 1
        return self.balance

    @optimize(speed=3, safety=0, debug=0)
    def withdraw(self, amount: int64) -> int64:
        if amount <= self.balance:
            self.balance = self.balance - amount
        else:
            self.balance = self.balance - 1
        self.tx_count = self.tx_count + 1
        return self.balance

    @optimize(speed=3, safety=0, debug=0)
    def score(self) -> int64:
        return self.balance * 3 + self.tx_count * 7 + self.ident


@optimize(speed=3, safety=0, debug=0)
class RewardsAccount(Account):
    @optimize(speed=3, safety=0, debug=0)
    def __init__(self, ident: int64, balance: int64, rate: int64):
        super().__init__(ident, balance)
        self.rate = rate
        self.points = 0

    @optimize(speed=3, safety=0, debug=0)
    def deposit(self, amount: int64) -> int64:
        bal: int64 = super().deposit(amount)
        self.points = self.points + (amount * self.rate) // 10
        return bal + self.points

    @optimize(speed=3, safety=0, debug=0)
    def score(self) -> int64:
        return super().score() + self.points * 5


@optimize(speed=3, safety=0, debug=0)
class VaultAccount(RewardsAccount):
    @optimize(speed=3, safety=0, debug=0)
    def __init__(self, ident: int64, balance: int64, rate: int64, floor: int64):
        super().__init__(ident, balance, rate)
        self.floor = floor
        self.penalties = 0

    @optimize(speed=3, safety=0, debug=0)
    def withdraw(self, amount: int64) -> int64:
        if self.balance - amount < self.floor:
            self.penalties = self.penalties + 1
            return super().withdraw(1)
        return super().withdraw(amount)

    @optimize(speed=3, safety=0, debug=0)
    def score(self) -> int64:
        return super().score() - self.penalties * 11


@optimize(speed=3, safety=0, debug=0)
def prepare(size: int64, seed: int64) -> list[tuple[int, int, int]]:
    if size < 0:
        raise ValueError("size must be nonnegative")
    state: int64 = seed & 0x7FFFFFFF
    ops: list[tuple[int, int, int]] = []
    for _ in range(size):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        idx: int64 = (state >> 4) & 63
        amount: int64 = ((state >> 10) % 97) + 1
        kind: int64 = state & 3
        ops.append((idx, amount, kind))
    return ops


@optimize(speed=3, safety=0, debug=0)
def run(data: list[tuple[int, int, int]]) -> tuple[int, int, int]:
    accounts: list[Account] = []
    i: int64
    for i in range(64):
        kind: int64 = i % 3
        if kind == 0:
            accounts.append(Account(i, 500 + i * 10))
        elif kind == 1:
            accounts.append(RewardsAccount(i, 500 + i * 10, (i % 5) + 1))
        else:
            accounts.append(VaultAccount(i, 500 + i * 10, (i % 4) + 1, 200))

    rounds: int64 = max(1, len(data) // 425)
    rolling: int64 = 0
    round_idx: int64
    idx: int64
    amount: int64
    for round_idx in range(rounds):
        bonus: int64 = round_idx & 7
        for idx, amount, kind in data:
            acc = accounts[(idx + round_idx) & 63]
            adj: int64 = amount + bonus
            if kind == 0:
                rolling = (rolling + acc.deposit(adj)) & 0x3FFFFFFF
            elif kind == 1:
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
