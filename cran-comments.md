# cran-comments for LLMRagent 0.8.0

## Submission

This is a new (first) submission.

## Test environments

- local macOS (arm64), R 4.4.x: `R CMD check --as-cran --no-manual`

## R CMD check results

Status: 2 NOTEs

- "New submission": this is the package's first CRAN release.
- "unable to verify current time" (checking for future file timestamps):
  the check machine had no network access to a time service; the package's
  file timestamps are not in the future.

0 errors, 0 warnings.

## Dependencies

Imports LLMR (>= 0.8.9), which is on CRAN. All other Imports and Suggests
are on CRAN.

## Tests and examples

The test suite is fully offline: model calls are exercised through injected
test doubles (a `caller` seam on the agent, a `transport` seam on the MCP
client, an `executor` seam on sandboxed tools), so no API keys or network
access are needed. The few tests that would reach live providers are guarded
with `skip_on_cran()`. Examples that would require API keys are wrapped in
`\dontrun{}`.
