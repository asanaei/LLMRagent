# cran-comments for LLMRagent 0.8.1

## Submission

Update to 0.8.1, a fast-follow correction of the 0.8.0 release. Study
archives write a data-only, hash-sealed record (no live objects, no
credentials); blocking tool guardrails are evaluated before a tool's side
effect runs; call and token budgets are re-checked across memory
compaction, tool loops, and retrieval-memory embedding operations;
save_agent() refuses a config carrying a literal API key; and the public
surface narrows to supported contracts (the R6 generator is internal;
agent() is the constructor).

## Dependencies

Imports LLMR (>= 0.8.11), which is on CRAN. All other Imports and Suggests
are on CRAN.

## Test environments

- local macOS (arm64), R 4.4.3: `R CMD check --as-cran`

## R CMD check results

0 errors | 0 warnings | 3 notes

All three notes are environmental: CRAN incoming feasibility (new-version
housekeeping), "unable to verify current time" (no network time service on
the build machine), and the HTML manual note from an older system `tidy`
that does not recognize the HTML5 elements R generates.

## Tests and examples

The test suite is fully offline: model calls are exercised through injected
test doubles, so no API keys or network access are needed. Tests that would
reach live providers are guarded by environment-variable keys and
skip_on_cran(). Examples that would make a network call are wrapped in
\dontrun{}.
