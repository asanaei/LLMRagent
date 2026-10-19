# LLMRagent 0.8.2

- `agent_experiment()` refuses design columns named `rep`, `result`, `error`,
  or `duration`, which it used to overwrite, and accepts only a positive whole
  number for `reps`. Converting an experiment with `as_agent_run()` records the
  status of every cell in a `cells` artifact and warns when cells contributed
  no events; failed cells and cells that returned plain values used to vanish
  from the converted run. The run's design reports the number of converted
  cells as `n_converted`. The manifest hash leaves that count out, so an
  experiment's hash is the one 0.8.1 computed for it.

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

- `agent_robustness()` counts a failed measurement (an error, `NA`, or a
  non-scalar value from `measure`) as a failed cell, records it in
  `measure_error`, and warns; such cells used to vanish while the failure rate
  stayed at zero. With nothing measured, the summary is one row with
  `fragile = NA`, and the dispersion is `NA`, where it used to be 0.
  Agreement across an axis now uses every replicate; with two or more axes it
  used only the first replicate of each unit and level. A paraphrase
  shortfall warns, and no usable paraphrase is an error.

- A failed `agent_fanout_synthesis()` worker becomes a failed call in the
  converted run, with its error and request hash; it used to have no call
  record, so diagnostics reported no failures. A strong-model call whose
  request hash cannot be computed is recorded without one, with a warning,
  instead of being dropped.

- `replay_run()` raises `llmragent_replay_mismatch` when a replay finishes
  before reaching the end of the recorded run; such a replay used to pass. A
  recorded log line that is not valid JSON, or a missing input snapshot, now
  stops the replay; the line used to be dropped and the input replaced by an
  empty state.

- A completed conversation, preset, or pipeline records its agents' tools,
  generation parameters, and memory when it closes. Its manifest and state view
  used to read them from the live agents, so changing or reusing an agent
  afterwards rewrote the earlier run's record. A bare `Agent` remains a live
  view.

- `diagnostics()` reports `NA`, with a warning naming the level and the
  reason, for every figure that depends on a level of the run it cannot read,
  and it adds a column `n_unknown_tokens`, the number of successful calls that
  reported no total-token usage. It used to report zeros in both cases, so a
  run whose call records could not be read showed no calls and no failures,
  and a provider that reported no usage showed `tokens_total = 0`.
  `duration_s` is `NA`, where it used to be 0, when calls exist but none
  recorded a duration.

- `agent_manifest()` adds `models_basis` (whether the model list came from the
  call records or from the participating agents) and `n_unknown_tokens`, both
  outside the hash. When the call records cannot be read it warns and reports
  `n_calls` and the token sums as `NA`; it used to report 0 calls and take the
  model list from the participants without saying so.

- The call level of a run (`tibble::as_tibble(run, "call")`) raises an error
  naming the event when a call event has no readable response record; such
  events used to be left out, so the run seemed to have made fewer calls.
  Records written under different LLMR versions are bound by column name, a
  column missing from some records being filled with missing values of the
  type the others use; they used to fail to bind, and diagnostics then showed
  no calls. Column types that cannot be combined are an error.

- When a reply arrives but its call record cannot be built, an agent or
  `agent_fanout_synthesis()` keeps the reply and its usage, warns ("The reply
  was received, but its call record could not be constructed."), and records
  the exchange as an event with status `"unrecorded"` and the reason in its
  note. Such an exchange used to appear as a successful event without a call
  row, or, for fan-out workers and strong-model calls, to vanish from the
  converted run.

- `agent$usage()` adds `n_unknown_usage`, the number of model calls and
  embedding operations whose sent or received token count was not reported;
  `save_agent()` and approval checkpoints keep it. Unreported usage still adds
  nothing to the token totals, but it used to leave no trace. Once such a call
  has occurred, an agent with a finite `max_tokens` warns before each call
  that the limit is checked against recorded tokens only. Agents saved by
  earlier versions load with `n_unknown_usage = NA`.

- `report()` says in its text when the call records could not be read or some
  calls reported no total-token usage, and warns when the methods paragraph or
  the claim-language check could not be produced. It used to print "Methods
  paragraph unavailable (no call records)" for every such failure and to skip
  a failed claim-language check without notice. `view_run()` shows the reason
  in place of a table it cannot build, where it used to show an empty table,
  and marks token totals that are unavailable or incomplete.

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
