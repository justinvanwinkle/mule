"""Simulate a polymorphic account ledger with inheritance and super() dispatch.

Transactions are generated from the runtime seed and applied across a rotating
pool of Account, RewardsAccount, and VaultAccount instances.
"""

from perfutils import run_case

NAME = "account_ledger"
DESCRIPTION = "Polymorphic class hierarchy with super() calls and slot updates"
DEFAULT_SIZE = 100_000
DEFAULT_SEED = 42


class Account:
    def __init__(self, ident: int, balance: int):
        self.ident = ident
        self.balance = balance
        self.tx_count = 0

    def deposit(self, amount: int) -> int:
        self.balance = self.balance + amount
        self.tx_count = self.tx_count + 1
        return self.balance

    def withdraw(self, amount: int) -> int:
        if amount <= self.balance:
            self.balance = self.balance - amount
        else:
            self.balance = self.balance - 1
        self.tx_count = self.tx_count + 1
        return self.balance

    def score(self) -> int:
        return self.balance * 3 + self.tx_count * 7 + self.ident


class RewardsAccount(Account):
    def __init__(self, ident: int, balance: int, rate: int):
        super().__init__(ident, balance)
        self.rate = rate
        self.points = 0

    def deposit(self, amount: int) -> int:
        bal = super().deposit(amount)
        self.points = self.points + (amount * self.rate) // 10
        return bal + self.points

    def score(self) -> int:
        return super().score() + self.points * 5


class VaultAccount(RewardsAccount):
    def __init__(self, ident: int, balance: int, rate: int, floor: int):
        super().__init__(ident, balance, rate)
        self.floor = floor
        self.penalties = 0

    def withdraw(self, amount: int) -> int:
        if self.balance - amount < self.floor:
            self.penalties = self.penalties + 1
            return super().withdraw(1)
        return super().withdraw(amount)

    def score(self) -> int:
        return super().score() - self.penalties * 11


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


def run(data: list[tuple[int, int, int]]) -> tuple[int, int, int]:
    accounts: list[Account] = []
    for i in range(64):
        kind = i % 3
        if kind == 0:
            accounts.append(Account(i, 500 + i * 10))
        elif kind == 1:
            accounts.append(RewardsAccount(i, 500 + i * 10, (i % 5) + 1))
        else:
            accounts.append(VaultAccount(i, 500 + i * 10, (i % 4) + 1, 200))

    rounds = max(1, len(data) // 425)
    rolling = 0
    for round_idx in range(rounds):
        bonus = round_idx & 7
        for idx, amount, kind in data:
            acc = accounts[(idx + round_idx) & 63]
            adj = amount + bonus
            if kind == 0:
                rolling = (rolling + acc.deposit(adj)) & 0x3FFFFFFF
            elif kind == 1:
                rolling = (rolling + acc.withdraw(adj)) & 0x3FFFFFFF
            else:
                rolling = (rolling + acc.score()) & 0x3FFFFFFF

    final_score = 0
    tx_total = 0
    for acc in accounts:
        final_score += acc.score()
        tx_total += acc.tx_count
    return rolling, final_score, tx_total


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
