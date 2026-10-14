"""Standard-library-only support for standalone performance scenarios.

Keep runner/reporting imports out of this module: scenarios compile only the
small measurement helper, not the host-side orchestration code.
"""

from .case import run_case

__all__ = ["run_case"]
