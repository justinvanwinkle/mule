"""Simulate a 5-body gravitational system with Mule float64/int64 annotations and speed=3, safety=0.

Exercises unboxed float64/int64 specialization, math.sqrt, nested pairwise
loops, and high-speed/zero-safety optimization policy.
"""

import math

from perfutils import run_case

from mule import float64, int64, optimize

__speed__ = 3
__safety__ = 0

NAME = "float_nbody_typed"
DESCRIPTION = "Floating-point N-body simulation with float64/int64 annotations and speed=3, safety=0"
DEFAULT_SIZE = 150_000
DEFAULT_SEED = 42


def prepare(
    size: int, seed: int
) -> tuple[int, list[tuple[float, float, float, float, float, float, float]]]:
    if size < 1:
        raise ValueError("size must be positive")
    state = seed & 0x7FFFFFFF
    scale = 0.0009765625
    bodies: list[tuple[float, float, float, float, float, float, float]] = []
    for i in range(5):
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        x = float(((state >> 8) % 2048) - 1024) * scale + float(i * 2)
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        y = float(((state >> 8) % 2048) - 1024) * scale
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        z = float(((state >> 8) % 2048) - 1024) * scale
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        vx = float(((state >> 8) % 512) - 256) * scale
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        vy = float(((state >> 8) % 512) - 256) * scale
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        vz = float(((state >> 8) % 512) - 256) * scale
        state = (state * 1103515245 + 12345) & 0x7FFFFFFF
        mass = float(((state >> 8) % 512) + 256) * scale
        bodies.append((x, y, z, vx, vy, vz, mass))
    return size, bodies


@optimize(speed=3, safety=0)
def pair_mag(dx: float64, dy: float64, dz: float64, dt: float64) -> float64:
    d2: float64 = dx * dx + dy * dy + dz * dz + 0.015625
    dist: float64 = math.sqrt(d2)
    return dt / (d2 * dist)


@optimize(speed=3, safety=0)
def run(
    data: tuple[int, list[tuple[float, float, float, float, float, float, float]]],
) -> tuple[int, int]:
    steps, bodies = data
    n: int64 = len(bodies)
    px = [b[0] for b in bodies]
    py = [b[1] for b in bodies]
    pz = [b[2] for b in bodies]
    vx = [b[3] for b in bodies]
    vy = [b[4] for b in bodies]
    vz = [b[5] for b in bodies]
    mass = [b[6] for b in bodies]
    dt: float64 = 0.0078125
    step: int64
    i: int64
    j: int64

    for step in range(steps):
        for i in range(n):
            xi: float64 = px[i]
            yi: float64 = py[i]
            zi: float64 = pz[i]
            vxi: float64 = vx[i]
            vyi: float64 = vy[i]
            vzi: float64 = vz[i]
            mi: float64 = mass[i]
            for j in range(i + 1, n):
                dx: float64 = xi - px[j]
                dy: float64 = yi - py[j]
                dz: float64 = zi - pz[j]
                mag: float64 = pair_mag(dx, dy, dz, dt)
                mj: float64 = mass[j]
                vxi = vxi - dx * (mj * mag)
                vyi = vyi - dy * (mj * mag)
                vzi = vzi - dz * (mj * mag)
                vx[j] = vx[j] + dx * (mi * mag)
                vy[j] = vy[j] + dy * (mi * mag)
                vz[j] = vz[j] + dz * (mi * mag)
            vx[i] = vxi
            vy[i] = vyi
            vz[i] = vzi
        for i in range(n):
            px[i] = px[i] + dt * vx[i]
            py[i] = py[i] + dt * vy[i]
            pz[i] = pz[i] + dt * vz[i]

    energy: float64 = 0.0
    spread: float64 = 0.0
    for i in range(n):
        vxi2: float64 = vx[i]
        vyi2: float64 = vy[i]
        vzi2: float64 = vz[i]
        pxi2: float64 = px[i]
        pyi2: float64 = py[i]
        pzi2: float64 = pz[i]
        mi2: float64 = mass[i]
        v2: float64 = vxi2 * vxi2 + vyi2 * vyi2 + vzi2 * vzi2
        energy = energy + 0.5 * mi2 * v2
        spread = spread + (pxi2 * pxi2 + pyi2 * pyi2 + pzi2 * pzi2) * mi2
        for j in range(i + 1, n):
            dx2: float64 = pxi2 - px[j]
            dy2: float64 = pyi2 - py[j]
            dz2: float64 = pzi2 - pz[j]
            mj2: float64 = mass[j]
            dist: float64 = math.sqrt(dx2 * dx2 + dy2 * dy2 + dz2 * dz2 + 0.015625)
            energy = energy - (mi2 * mj2) / dist

    return int(energy * 1048576.0), int(spread * 65536.0)


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
