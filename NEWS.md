# LLMRagent 0.8.2

- `agent_experiment()` refuses design columns named `rep`, `result`, `error`,
  or `duration`, which it used to overwrite, and accepts only a positive whole
  number for `reps`. Converting an experiment with `as_agent_run()` records the
  status of every cell in a `cells` artifact and warns when cells contributed
  no events; failed cells and cells that returned plain values used to vanish
  from the converted run.

- `check_state_leakage()` reports `clean = NA`, with a warning and the list of
  unexamined cells, when no leak is found but some cells could not be
  examined. It used to report `clean = TRUE`, including for experiments whose
  cells return reply text and therefore expose no agents at all.

- `archive_agent_study()` never copies a whole live audit log into an archive.
  It used to do so when the run had no request hashes or the log could not be
  read, which put other studies' requests and replies into `calls.jsonl`, even
  with `include_messages = FALSE`. Records are matched on response id as well
  as request hash, which keeps an earlier identical request out, and a privacy
  option that cannot be applied removes the copied records.

# LLMRagent 0.8.1

- Fixed archive privacy, pre-execution tool guardrails, per-round budgets, and
  governed-tool limits; kept only the supported user functions.
- Retrieval-memory embedding operations now pass the agent's budget and are
  recorded in the trace as `embed` events; the model call is re-gated after
  any accounted work earlier in the same turn.
- `save_agent()` refuses a config carrying a literal API key instead of
  warning and writing the key to disk.
- The `Agent` R6 generator is internal; construct agents with `agent()`.

# LLMRagent 0.8.0

Initial CRAN release.

- Reproducible LLM agents built on 'LLMR': personas, native R tools,
  pluggable memory, and declared call, token, tool, and time budgets.
- Multi-agent conversations over a shared transcript, with turn-taking
  presets for debates, focus groups, interviews, and deliberations with
  votes; factorial experiments over those designs run in parallel and
  return tidy results.
- Provenance throughout: every run converts to one object with utterance,
  event, call, tool, and state views; a study manifest of content hashes;
  and a sealed replication archive whose call log reads back through
  'LLMR'.
- Governance: declared tool side effects, guardrails on inputs, outputs,
  and tool calls, human approval gates that pause and resume, and a governed
  Model Context Protocol client.
- A small workflow runtime with checkpoint, resume, fork, and
  hash-verified replay.
