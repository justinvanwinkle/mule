"""Simulate a 5-body gravitational system using symplectic Euler integration.

Exercises floating-point arithmetic, math.sqrt, nested pairwise loops, and
mutable coordinate/velocity state arrays.
"""

import math

from perfutils import run_case

NAME = "float_nbody"
DESCRIPTION = "Floating-point N-body gravitational orbital simulation"
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


def pair_mag(dx: float, dy: float, dz: float, dt: float) -> float:
    d2 = dx * dx + dy * dy + dz * dz + 0.015625
    dist = math.sqrt(d2)
    return dt / (d2 * dist)


def run(
    data: tuple[int, list[tuple[float, float, float, float, float, float, float]]],
) -> tuple[int, int]:
    steps, bodies = data
    n = len(bodies)
    px = [b[0] for b in bodies]
    py = [b[1] for b in bodies]
    pz = [b[2] for b in bodies]
    vx = [b[3] for b in bodies]
    vy = [b[4] for b in bodies]
    vz = [b[5] for b in bodies]
    mass = [b[6] for b in bodies]
    dt = 0.0078125

    for _ in range(steps):
        for i in range(n):
            xi = px[i]
            yi = py[i]
            zi = pz[i]
            vxi = vx[i]
            vyi = vy[i]
            vzi = vz[i]
            mi = mass[i]
            for j in range(i + 1, n):
                dx = xi - px[j]
                dy = yi - py[j]
                dz = zi - pz[j]
                mag = pair_mag(dx, dy, dz, dt)
                mj = mass[j]
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

    energy = 0.0
    spread = 0.0
    for i in range(n):
        v2 = vx[i] * vx[i] + vy[i] * vy[i] + vz[i] * vz[i]
        energy = energy + 0.5 * mass[i] * v2
        spread = spread + (px[i] * px[i] + py[i] * py[i] + pz[i] * pz[i]) * mass[i]
        for j in range(i + 1, n):
            dx = px[i] - px[j]
            dy = py[i] - py[j]
            dz = pz[i] - pz[j]
            dist = math.sqrt(dx * dx + dy * dy + dz * dz + 0.015625)
            energy = energy - (mass[i] * mass[j]) / dist

    return int(energy * 1048576.0), int(spread * 65536.0)


def main():
    run_case(NAME, DESCRIPTION, DEFAULT_SIZE, DEFAULT_SEED, prepare, run)


if __name__ == "__main__":
    main()
