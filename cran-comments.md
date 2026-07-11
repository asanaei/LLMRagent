# cran-comments for LLMRagent 0.8.0

## Submission

This is a new (first) submission.

## Test environments

- local macOS (arm64), R 4.4.x: `R CMD check --as-cran --no-manual`

## R CMD check results

Status: 2 NOTEs

- "New submission": this is the package's first CRAN release.
- "Possibly misspelled words in DESCRIPTION: essentialist, sandboxed": both are
  correctly spelled. "anti-essentialist" is a standard social-science term, and
  "sandboxed" describes the isolated tool execution the package provides.
- "unable to verify current time" (checking for future file timestamps):
  the check machine had no network access to a time service; the package's
  file timestamps are not in the future.

0 errors, 0 warnings.

An earlier upload of this version failed one test on CRAN's incoming check
(`test-leakage.R`) because a diagnostic's print method rendered a tibble that
the console width abbreviated on the check machine; the print now lists the
findings in plain text, and the test passes at any width.

## Dependencies

Imports LLMR (>= 0.8.10), which is on CRAN. All other Imports and Suggests
are on CRAN.

## Tests and examples

The test suite is fully offline: model calls are exercised through injected
test doubles (a `caller` seam on the agent, a `transport` seam on the MCP
client, an `executor` seam on sandboxed tools), so no API keys or network
access are needed. The few tests that would reach live providers are guarded
with `skip_on_cran()`. Examples that would require API keys are wrapped in
`\dontrun{}`.
