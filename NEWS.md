# LLMRagent 0.8.0

Initial CRAN release.

- Reproducible LLM agents built on 'LLMR': personas, native R tools,
  pluggable memory, and hard budgets checked before every call.
- Multi-agent conversations over a shared transcript, with turn-taking
  presets for debates, focus groups, interviews, and deliberations with
  votes; factorial experiments over those designs run in parallel and
  return tidy results.
- Provenance throughout: every run converts to one object with utterance,
  event, call, tool, and state views; a study manifest of content hashes;
  and a sealed replication archive whose call log reads back through
  'LLMR'.
- Governance: declared tool side effects, guardrails on inputs, outputs,
  and tool results, human approval gates that pause and resume, sandboxed
  tool execution, and a governed Model Context Protocol client.
- A small workflow runtime with checkpoint, resume, fork, and
  hash-verified replay.
