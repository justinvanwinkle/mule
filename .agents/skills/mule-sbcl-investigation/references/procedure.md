# Repeatable Mule investigation procedure

Commands run from the active repository worktree unless stated otherwise.
Follow `AGENTS.md` for worktrees, authorization, checks, and benchmark policy.
Inspection recipes are not assertions that a new measurement was performed.

## 1. Preserve the objective and select the immediate question

Record the original program, command, inputs, output, timer boundary, and target.
Identify required identity, mutation, rebinding, cleanup, and concurrency behavior.
Separate compiler-host execution from the compiled program and its CPython oracle.

| Assignment | First useful action |
| --- | --- |
| Unknown runtime deficit | Reproduce it and identify a dominant dynamic operation. |
| Named SBCL/code-generation discrepancy | Inspect that operation's actual emitted entries and helpers. |
| Cold compilation or linking delay | Separate analysis, emission, LLVM compilation, runtime build, and link costs. |
| Wrong result or crash | Reproduce it against the oracle before interpreting performance. |

Use retained decisive evidence when it already establishes the defect.
Do not launch an unrelated profiling campaign instead of the requested comparison.
Keep any reduced diagnostic alongside the original program.

## 2. Establish comparable, identified artifacts

Read `perf/README.md` and the actual runtime configuration first.
Check source revisions/diffs, interpreter/toolchain versions, target, and hashes
of relevant source and executables. A path alone does not identify a binary.
Keep baseline and candidate artifacts distinct.

The configured suite supports:

```sh
uv run python -m perf.suite.perfutils list
uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --jobs 4 --samples 3
```

A targeted diagnostic can select one named case without recording full history:

```sh
uv run python -m perf.suite.perfutils run \
  --config perf/runtimes.example.toml --jobs 4 --samples 3 \
  --no-history closure_pipeline
```

These are starting recipes for a new investigation, not permission to change
an existing comparison's settings. Preserve its sample count, warmups, inputs,
seed, loops, affinity, optimization/LTO policy, runtime config, and concurrency.
Use the full configured suite when that is the requested check.
Do not override `XDG_CACHE_HOME` or interpret a cache-skipped analysis as a
measurement of that compiler phase.

Builds precede validation and timing. Validate each result against CPython.
The harness separates preparation, kernel, process, and build costs.
Runtime order rotates within scenarios; configured scenario jobs can overlap.
Retain raw samples and rejected attempts, not just favorable medians.

Monitor unrelated CPU activity, memory pressure, swapping, and I/O throughout.
On Linux, inspect available system tools such as `vmstat 1`, `top`, or `iostat`.
Stop monitoring processes when the run ends. Do not install missing tools silently.
The suite's CPU-noise guard measures external utilization and can retry the
timing phase with unchanged settings. Read its current options and report fields.
It does not establish freedom from memory pressure, I/O contention, or throttling.
Configured benchmark work is not unrelated load.
If interference invalidates results, report it and rerun the same experiment
after it clears. Do not silently reduce `--jobs` to obtain cleaner numbers.

## 3. Account for one material cost

Use an available profiler on the real executable and relevant SBCL counterpart.
Follow hot callers and callees, including compiled protocols and Rust primitives.
Separate exclusive costs from inclusive samples. Do not sum nested percentages.
Keep static code size, dynamic instructions, allocations, and elapsed time distinct.
Keep gross counters separate from loop-subtracted counters.
Profiler-instrumented elapsed time is attribution evidence, not a normal timing.

Record one operation, its frequency, repeated work, and contribution to the
original deficit. If it explains only a small fraction, keep the unexplained
remainder explicit. Do not collect more data without a distinguishing question.

## 4. Find the matching SBCL producer and consumer

Start with `spec/sbcl-compiler.md`, `spec/native-lowering.md`, and
`spec/compiler-gaps.md`, then inspect actual source in `~/checkout/sbcl`.

| Mechanism | SBCL starting source | Mule starting source |
| --- | --- | --- |
| Flow types and rewrite feedback | `constraint.lisp`, `ir1opt.lisp` | `constraint.py`, `locall.py`, `ir1opt.py` |
| Local calls and entries | `locall.lisp`, `ltn.lisp` | `locall.py`, `ltn.py`, `codegen.py` |
| Sequence transforms | `seqtran.lisp` | `seqtran.py`, `ir2tran.py` |
| Environment and ownership adaptation | `envanal.lisp`, IR2 lowering | `envanal.py`, representation and cleanup consumers |
| Representations and coercions | `represent.lisp`, `ir2tran.lisp` | `represent.py`, `ir2tran.py`, `vops.py` |
| File compilation and FASL/module loading | `main.lisp` (`compile-file`), `../code/module.lisp`, `../code/load.lisp`, `../code/target-load.lisp` | `driver.py`, `modules.py`, `mule/runtime/imports.rs` |

SBCL entries above are under `src/compiler/`; Mule entries under `mule/compiler/`.
Follow the exact operation beyond this starting table.
Inspect SBCL-generated code, not just declarations or helper definitions.
Match the workload, algorithm, declarations, safety policy, and dynamic behavior.
Explain any source-level differences before attributing them to the compiler.

The existing `perf/suite/log_aggregation.lisp` defines `run-kernel`.
One inspection route is:

```sh
mkdir -p /tmp/mule-sbcl-inspect
sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --eval '(compile-file "perf/suite/log_aggregation.lisp" :output-file "/tmp/mule-sbcl-inspect/log_aggregation.fasl")'
sbcl --noinform --no-sysinit --no-userinit --non-interactive \
  --load /tmp/mule-sbcl-inspect/log_aggregation.fasl \
  --eval '(disassemble (function run-kernel))'
```

Check the current scenario's entry/declarations before reusing this recipe.
Compare the configured standalone-image build for performance claims.
Consult the `llvm` skill and matching LLVM sources for lowering uncertainties.
Use other local language implementations to investigate concrete adaptations,
not to replace SBCL's architecture or Mule's language/runtime boundaries.

## 5. Trace Mule through the actual entry and compilation units

For each transition, record the permitting fact and its consumer:

1. Source operation and observable effects.
2. IR1 operand/result facts and applicable transforms.
3. Call-target proof and selected entry context.
4. IR2 VOP, primitive type, storage representation, and ownership operations.
5. LLVM before and after optimization in every relevant unit.
6. Linked instructions, helper paths, and dynamic work.

Use [the worked trace](stage-trace.md) for actual inspection APIs.
Distinguish a general wrapper from specialized, numeric, or unpacked entries.
Follow imported protocol units instead of inspecting only the caller.
Per-unit optimized LLVM is pre-link, including when LTO is enabled.
Inspect linked assembly for cross-unit and Rust effects.

For realistic native regressions, inspect
`test/mule/compiler/rewrite_support.py` before using `build_case`.
Its returned case exposes `unit_paths`, `unit_ir(unit, optimized=...)`,
`function_ir(...)`, `assembly`, `function_assembly(...)`, and `compare(...)`.
Use its actual build flags and discover real emitted symbols.
These helpers build native programs; they are not cheap analysis-only probes.

Record whether a fact is derived, published, consumed, and exercised.
A manually invoked analysis or constructed proof is not normal-pipeline evidence.
If static information is genuinely unavailable, demonstrate the counterexample
and examine the cheapest correct guarded/general path.

## 6. Predict and implement one reusable improvement

State the expensive work, the missing mechanism, and the expected observation
after fixing it. Explain the expected workload effect from measured attribution.
Identify an observation that would contradict the explanation.

Establish one realistic failing efficiency regression before broad expansion.
Use normal source constructs and runtime inputs. Include meaningful near misses.
Confirm that failure means the unwanted work remains, not a guessed symbol,
unrelated compile error, constant-folded case, or legitimately inlined helper.
Follow helper calls and data dependencies so moving work cannot satisfy the test.
Keep required cold fallbacks outside the proven hot path.

Implement the corresponding SBCL mechanism at its responsible stage.
Do not add benchmark-specific paths, manually seed framework-owned facts, move
language algorithms to Rust, or change ownership/concurrency contracts silently.
Document a necessary architectural departure in the relevant specification first.
Label diagnostic monkey-patches and source rewrites as experiments, not fixes.

## 7. Verify and explain discrepancies

Keep four results separate: semantic equivalence, normal-pipeline integration,
efficient generated work, and effectiveness on the original program.
Run affected repository checks and spot-check actual outputs.
An IR verifier establishes structural validity, not efficient execution.

If the predicted effect is absent, do not stop at passing tests.
Test whether measurement, cost attribution, the causal model, entry selection,
propagation, or implementation is wrong. Explain why previous checks missed it.
A correct strategy may need an implementation repair, not replacement.
A correct implementation may expose an incorrect workload-cost model.
Do not adjust the prediction after the fact without supporting evidence.

## 8. Preserve the model and reconcile the original target

Keep a task record with the goal, current explanation, assumptions, source and
artifact identity, SBCL correspondence, predictions, observations, rejected
explanations, and next distinguishing experiment.
Update affected specifications when behavior changes and correct obsolete claims.
Keep raw evidence separate from interpretation and pass corrections to workers.

Reconcile actual merged work and completed results before claiming success.
Separate a completed local mechanism from the still-unmet workload objective.
If blocked or redirected, state the remaining discrepancy rather than declaring
completion. Do not repeat the entire investigation when existing evidence resolves
the next question.
