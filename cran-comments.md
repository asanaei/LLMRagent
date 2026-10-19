# cran-comments for LLMRagent 0.8.2

## Submission

Update of LLMRagent 0.8.1 on CRAN. This release corrects records that were
wrong without saying so. An agent no longer runs a tool call whose arguments
did not parse, or that came from a reply cut off at the token limit; the
model receives an error result instead. Converted experiments, robustness
batteries, and fan-out results keep their failed cells, measurements, and
calls rather than dropping them, and a workflow replay that stops early is
rejected. A finished run's manifest
and state view no longer change when its agents are reused. Diagnostics,
manifests, reports, and archives report missing values with a warning where
they used to report zeros, and archives no longer copy a whole live audit
log. NEWS.md lists each change, with the new errors and warnings it
introduces. Manifest hashes are computed as in 0.8.1.

## Dependencies

Imports LLMR (>= 0.8.13). That version reports whether a tool call's
arguments parsed, which this release relies on, and it is submitted to CRAN
ahead of this package. All other Imports and Suggests are on CRAN.

## Test environments

- local macOS 26.7 (arm64), R 4.4.3: `R CMD check --as-cran --no-manual`,
  against LLMR 0.8.13

## R CMD check results

0 errors | 0 warnings | 1 note

The note is environmental: "unable to verify current time" (no network time
service on the build machine).

## Tests and examples

The test suite is offline: model calls go through injected test doubles or a
stubbed transport, so no API keys or network access are needed. Tests that
would reach live providers are guarded by environment-variable keys and
skip_on_cran(). Examples run offline; the parts that would make a network
call are wrapped in \dontrun{}.
