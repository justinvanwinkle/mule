---
name: mule-sbcl-investigation
description: Use when diagnosing slow Mule programs or builds, comparing Mule with SBCL or CPython, implementing SBCL-corresponding compiler optimizations, or reviewing whether an optimization actually works. Trace source facts through IR1, entry contexts, IR2, LLVM, runtime units, and linked execution. Use SBCL, the LLVM skill, other local language implementations, and historical case studies to explain discrepancies and verify general improvements.
---

# Mule: SBCL-guided investigation and implementation

Apply the general `engineering-cycle` skill to Mule's compiler and runtime.
Keep its model, prediction, experiment, discrepancy, and durable-correction cycle.
This skill supplies project-specific execution paths, references, and cases.
It does not replace repository instructions or authorize architectural departures.

## Start with the requested program and outcome

Read `AGENTS.md`, `spec/index.md`, and the relevant implemented contract.
Use the active worktree's files, not an assumed checkout or an old report.
Distinguish a runtime slowdown, cold build delay, semantic failure, and requested
code-generation comparison. Do not replace one with another.

Preserve the original source, input, output, settings, and objective.
Do not substitute numeric kernels, annotations, source rewrites, or a later
compatibility blocker for ordinary-program efficiency.

Read [the procedure](references/procedure.md) before an investigation.
Use [the stage trace](references/stage-trace.md) for concrete inspection APIs.
Consult [the cases](references/case-studies.md) for relevant failure patterns.
Paths beginning `mule/`, `spec/`, `test/`, or `perf/` refer to the repository root.

## Use references to answer concrete questions

1. Mule's actual source, artifacts, and execution establish the problem.
2. **STOP and inspect the corresponding source in `~/checkout/sbcl` BEFORE
   proposing or implementing any compiler, loader, build, or runtime design.**
   Follow its producer, consumer, stage boundaries, FASL compilation/loading
   model (`src/compiler/main.lisp`, `src/code/module.lisp`, `src/code/load.lisp`,
   `src/code/target-load.lisp`), and optimization policies. Never invent bespoke
   IPC servers, multi-module dynamic bundles, or CLI self-spawning on cache hits
   instead of following SBCL's architecture.
3. Load the `llvm` skill for lowering, optimizer, ABI, lifetime, and verification
   questions. Check its advice against the LLVM version used by this build.
4. Inspect other language implementations under `~/checkout/` when their
   mechanisms clarify an adaptation. Locate actual directories and revisions.

Rust and Swift studies can inform captures, ownership, and cleanup.
Julia, GHC, and Kotlin/Native can inform managed roots and safepoints.
Clang and other frontends can inform ABI and exception lowering.
These are subjects to investigate, not claims that each checkout is available.
Do not clone, install, reset, or switch an external checkout without permission.

Trace an example from analysis through lowering to the runtime effect.
Record its assumptions, exact symbols, revision, and uninspected portions.
A declaration, analogous code pattern, or tutorial is not a complete mechanism.
Explain concrete Python/LLVM adaptations in the applicable specification before
implementing a departure from SBCL. References inform the model, not its truth.

## Require an uninterrupted source-to-execution explanation

For the selected operation, establish:

- Which fact permits cheaper work, and where SBCL obtains and consumes it.
- Where Mule derives, publishes, and consumes the equivalent fact.
- Which entry context and VOP the actual call reaches.
- What optimized LLVM, separately compiled protocols, and linked code execute.
- What changed in dynamic work and in the original workload's result or cost.

Classify missing information before implementing another pass:

| State | Next investigation |
| --- | --- |
| Available but not derived | Inspect the corresponding SBCL analysis and its Mule counterpart. |
| Derived but not consumed | Find the first propagation, scheduling, context, representation, or lowering failure. |
| Consumer not reached | Trace the actual callable, specialized entry, imported unit, and executable identity. |
| Genuinely unavailable | Demonstrate the mutation, alias, or unknown boundary and inspect the cheapest correct guarded or general path. |

Do not accept “Python is dynamic” as an unexplained large cost.
Do not manufacture facts that the source and approved contracts cannot justify.
Language semantics and algorithms belong in compiled Mule Python.
Rust remains the low-level primitive boundary. No CPython execution fallback.

## Predict, intervene, and resolve discrepancies

Predict the specific work removed and its contribution to the target workload.
Distinguish a measurement from an attribution or an untested hypothesis.
Implement the reusable mechanism, then inspect the same normal compilation path.
An analysis result in a manual probe is not an integrated optimization.

Check semantics, integration, generated work, and representative effectiveness
separately. Follow helper calls so relocating work cannot pass an efficiency test.
Allow required cold fallbacks outside the proven hot path.

If a predicted 15% improvement yields 2%, continue the investigation.
Distinguish wrong attribution or measurement from missing integration, an
incomplete implementation, and an incorrect causal model.
Explain why the prior checks missed the discrepancy and repair meaningful
coverage. Do not weaken a valid assertion or revise the prediction to fit.

Keep benchmark settings comparable. The standard suite uses `--jobs 4`.
Do not change concurrency or established settings without user agreement.
Monitor CPU activity, memory pressure, swapping, and I/O before and during runs.
Distinguish configured benchmark activity from unrelated interference.
See the procedure for the harness's CPU guard and its limitations.

## Preserve corrected understanding and finish the actual task

Keep the current model, rejected assumptions, evidence, and remaining
discrepancies in appropriate task records and affected specifications.
Update dependent implementation, tests, and delegated work after a correction.
Separate original observations from later interpretations and current contracts.
Do not create competing agent-memory or instruction authorities.

Before reporting completion, reconcile actual changes, completed checks,
artifact identity, and the original workload result.
**SBCL parity is the mandatory completion gate:** never commit, merge, or claim
completion until the implementation is at least as good as SBCL in stage
placement, IR mechanism, and domain coverage (annotated/unannotated functions,
recursion, closures, mixed calling conventions, and general operand shapes),
with zero test workarounds for unhandled cases.
A local mechanism may work while the original performance target or full SBCL
parity remains unmet.
A merged commit, valid IR, passing suite, or isolated speedup is not that target.
If blocked or redirected, preserve the unresolved state without claiming success.
