"""Constant LLVM IR procedures used by generated modules.

Each `.ll` file under `mule/procedures/` defines one internal LLVM procedure
(or core intrinsic declarations in `core/intrinsics.ll`). `codegen.py` parses
these procedures once per compiler process and includes in each generated `.ll`
module only the `@mule.*` procedures that module transitively calls.
"""

from __future__ import annotations

from functools import cache
from pathlib import Path

PROCEDURES_DIR = Path(__file__).resolve().parent
MULE_ROOT = PROCEDURES_DIR.parent.parent


@cache
def load_procedure(relative_path: str) -> str:
    """Return the contents of `mule/procedures/<relative_path>`."""
    return (PROCEDURES_DIR / relative_path).read_text(encoding="utf-8")


@cache
def load_category(category: str) -> str:
    """Concatenate all `.ll` files in `mule/procedures/<category>/`."""
    directory = PROCEDURES_DIR / category
    return "\n\n".join(
        path.read_text(encoding="utf-8").strip()
        for path in sorted(directory.glob("*.ll"))
    )


@cache
def all_procedures() -> tuple[tuple[str, str], ...]:
    """Return `(repo_relative_path, content)` for every `.ll` file in `mule/procedures/`."""
    return tuple(
        (
            str(path.relative_to(MULE_ROOT)),
            path.read_text(encoding="utf-8"),
        )
        for path in sorted(PROCEDURES_DIR.rglob("*.ll"))
    )
