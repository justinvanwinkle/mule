# Historical case studies

These cases separate correctness, generated-code evidence, and measured performance. They describe historical checkpoints, not current Mule behavior. Check current contracts and tests before applying a lesson.

## Binarytrees: large improvement, not one matched comparison

An unchanged, unannotated binarytrees program had a 14.641-second committed-baseline median in a three-sample run. A later five-sample candidate median was 0.656 seconds. The roughly 22-fold cross-run comparison is not matched before/after evidence. Later same-run pooling changed 0.653911 to 0.505649 seconds. Structural destructuring changed 0.516152 to 0.367288 seconds, while CPython measured 0.297749 seconds. The later Mule result remained about 1.23 times slower. Allocation attribution put 94.3% in storage for all tree tuples, not leaves alone.

Preserve sample counts and provenance, compare matched artifacts, and distinguish improvement from parity. See `examples/benchmarks-game/binarytrees/`, `spec/performance.md`, and `test/mule/compiler/`.

## Closure slots: measured improvement

A closure-slot lowering experiment reported five alternating samples, matching outputs, and a median change from 3.236 to 1.566 seconds (2.07x). This is a workload result, not a general speed claim. Preserve closure semantics, inspect the actual generated path, and measure the same workload. See `mule/compiler/envanal.py`, `mule/compiler/codegen.py`, and `test/mule/compiler/`.

## Dictionary analysis: a pass is not an optimization until consumed

A historical escape-analysis probe found 129 non-escaping parameters among 2,356, but none of the relevant dictionary parameters received a non-escape proof. The assistant explicitly reported that the pass was not connected to code generation. The probe establishes the absence of relevant parameter proofs at that checkpoint. The integration status is a contemporary admission, not an exhaustive reconstruction of historical consumers.

A separate non-atomic reference-count experiment changed `getitem` from 477 to 201 gross cycles. The concurrent ownership invariant then failed: the final count was 73 rather than 1. The change was reverted, and the invariant test passed again. This timing did not qualify as an optimization because it violated the concurrency contract. See `spec/`, `mule/runtime/object.rs`, and `test/mule/compiler/` for current contracts and test locations.

## Tokenizer: diagnostics must preserve semantics

A local-binding diagnostic recovered type facts and measured about 2.3% faster, but changed global-rebinding semantics. It was not a valid fix. A narrow borrowed-text probe on the real scanner measured about 3.74% faster with matching output, but used a temporary one-function/context rule that was not integrated as a general transformation. Neither establishes a shipped tokenizer speedup.

Trace the normal executed entry, preserve rebindable-global behavior, and test the rule through ordinary analysis. See `perf/suite/json_tokenizer.py`, `mule/compiler/constraint.py`, `mule/compiler/envanal.py`, and `test/mule/compiler/`.

## Float coercion: capability without a speed claim

A historical representation change implemented identity-aware float coercions and checked the generated recurrence. Mixed-boundary medians were 20.677 ms before and 20.031 ms after, within variation. This supports a representation capability and code-generation result, not an application speedup. Keep correctness, optimized-code shape, and performance separate. See `mule/compiler/represent.py`, `mule/compiler/vops.py`, `mule/compiler/codegen.py`, and `spec/performance.md`.

## LLVM-visible boxing: kernel is not application evidence

Controlled LLVM probes removed visible allocation/store/load/free roundtrips, while opaque producer calls retained work. This established an optimizer visibility boundary, not that normal Python programs would eliminate boxing. A production-codec microbenchmark later measured 8.15x for a float loop and 26.03x for a supported wide-integer loop. The report classified these as microbenchmarks, not application results.

Use isolated LLVM probes to explain a mechanism, then verify the actual Mule producer/consumer path and a representative workload. See `mule/compiler/codegen.py`, `mule/runtime/object.rs`, and `test/mule/compiler/`.

## Optimizer cap: negative evidence can prevent speculative work

A fresh-compilation scan found no optimizer-loop cap hits in completed scopes. Direct suite scenarios reached at most three rounds; the broader scan reached seven after 441 units and was incomplete. The limit of ten was left unchanged. This ruled out the cap hypothesis for sampled inputs, not every program. See `mule/compiler/ir1opt.py` and `test/mule/compiler/`.

## Rewrite audits: tests can encode wrong semantics

A historical SBCL-guided audit found native wrong-result cases and an unsound tuple-length fact. One `not in` lowering was instructive: existing tests expected the incorrect transform. Compare behavior with Python semantics and SBCL's corresponding stage. A passing test establishes only what its assertion covers. Resolve semantic defects before trusting performance from the affected transformation. See `mule/compiler/constraint.py`, `mule/compiler/ir1opt.py`, and `test/mule/compiler/`.

## Transform integration: an isolated win may not affect the application

Historical tokenizer transforms reported isolated kernel improvements, but their required type facts were available in an IR2 `EntryContext`, not in the IR1 pass first attempting to consume them. Several rounds therefore left the workload unchanged. Status messages said a combined run had started, but the reviewed evidence did not establish its result. Require a real compiled-path codegen test and a completed measurement. See `mule/compiler/ltn.py`, `mule/compiler/ir2tran.py`, `mule/compiler/constraint.py`, and `perf/suite/json_tokenizer.py`.

## LRU: reproduce the actual operation, then close the investigation

A user-directed comparison built equivalent Mule and SBCL executables with
matching output. The real `_unlink` LLVM body retained attribute helpers,
checks, and reference operations. A later complete suite reported 7.789 seconds
for Mule, 3.958 for CPython, and 1.704 for SBCL on LRU. Those were five-sample,
four-job suite observations, not an isolated patch experiment. The reported
history delta combined other changes and could not identify this session's effect.

This was useful diagnosis, not completed SBCL-level efficiency. Passing compiler
tests did not establish that the repeated work disappeared. Preserve the request
to compare actual output paths, identify the missing fact/consumer, and continue
until the remaining gap is explained. See `perf/suite/lru_simulation.py` and its
Lisp counterpart. Historical source: `20261020T0652Z_bepfraus`, full prefix and
`command_outputs/tutnnfme_output.log`.

## Efficiency-test expansion: validate the inspector before scaling it

A large rewrite-test campaign generated many parallel child tasks before its
inspection approach was established. Failures included guessed symbols,
legitimate inlining, wrong-stage assumptions, weak inspection conditions, and
real semantic defects. Selected native probes succeeded while aggregate checks
remained red or incomplete. The historical session ended with work pending.

Do not infer that parallelism caused these failures. The case demonstrates that
parallel edits and passing selected probes do not establish an effective suite.
First prove one normal-path regression fails for the intended missing work and
passes only when that work changes. Repair an inspector by finding the actual
path, not by accepting an empty selection or removing a valid requirement.
Historical source: `20261022T0410Z_ugvvjuea`, full prefix and child records.

## Cold compilation and command timing answer different questions

Historical discussions mixed kernel timings, whole-process timings, cold compilation, and cached command execution. State whether compilation and startup are included, and use the same boundary in each comparison. See `perf/README.md` and `spec/performance.md` for repository timing conventions.

## Evidence limits

These summaries rely on retained historical session records and outputs, not fresh reproductions. Some source or binary identities were not recorded, and some analyses were bounded or incomplete. Treat each number as evidence for its stated experiment, not a current benchmark or claim about current Mule. Current contracts and tests take precedence.

## Optional historical source locators

The skill is self-contained. These locators permit deeper checks on the research
machine when retained records exist. Missing local archives do not establish
that an experiment failed or authorize reconstructing its result from memory.

Clive records below are under `~/.cache/clive/<session-id>_session/`.
Use full `prefix.log` chronology as well as `conversation.jsonl`: compaction can
omit earlier and later phases. Child histories can repeat their parent's context.
Read actual command outputs before promoting a summary into measurement evidence.

| Case | Retained source |
| --- | --- |
| Binarytrees | `20261010T2122Z_cgpjndwa`; `command_outputs/gmfkmubz_output.log` is the three-sample baseline; `dwczjscn_output.log` is a separate five-sample experiment; `swhbeukp_output.log` is pooling; `bpccexef_output.log` is destructuring. |
| Closure slots | `spec/performance.md`, section “Closure slot lowering, 2026-10-22”; session `20261022T0047Z_uccfcxxn` also includes the later dictionary work. |
| Escape facts and non-atomic counts | `20261020T0132Z_kgscnaxv`; outputs `fkksmbjk`, `hzfafach`, `kgdttbcp`, `ajskucrc`, and `mwagwejk` record the probe, counters, failed invariant, and post-revert check. Each output uses the `_output.log` suffix under `command_outputs/`. |
| Tokenizer diagnostics | `20261021T2105Z_vmyexbpj`, full prefix; final merge does not establish completion of the requested performance comparison. |
| Float coercions | `20261021T0007Z_ctbxxhxh`; `spec/performance.md`, “Mixed-boundary float coercions, 2026-10-21”. |
| Visible boxing | `20261020T1618Z_jwdaxhjx`; `spec/performance.md`, “Scalar boxing kernels, 2026-10-20”. |
| Round limit | `20261020T2200Z_yfdjppbm`; distinguish entry-module coverage from the incomplete recursive scan. |
| Semantic rewrite audit | `20261021T0007Z_nxzfszue`; this is distinct from the float-coercion session at the same timestamp. |
| Efficiency-test campaign | `20261022T0410Z_ugvvjuea`; inherited child histories are not independent experiments, and started probes do not establish completed verification. |
| Agy transforms and integration | Conversation `eda420d2-cf0d-4eea-9472-19ca0c96a0fb`, particularly steps 205, 263, and 664; type-inference initiative `a141755b-4240-4710-92a7-8bbe7d5b0043` ends with a requested suite rerun at step 9548 without a captured result. |

The Agy CLI records were read from its local SQLite conversation store.
Recovered user/model text and tool summaries have different evidence strength
from retained raw results. Numeric database status values were not mapped.
An assigned child task, a completion claim, or a user's accusation alone does
not prove integration, effectiveness, or deceptive intent.
