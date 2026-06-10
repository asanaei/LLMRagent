# LLMRAgent 0.7.1

A ground-up rewrite on LLMR (>= 0.8.3). The package now centers on one
R6 `Agent` and a small set of composable layers above it.

## Agents

- `agent()`: persona + `LLMR::llm_config()` + tools + memory + budgets.
  `chat()` is stateful, `reply()` stateless, `ask_structured()` returns
  schema-shaped answers parsed into R lists.
- `chat(stream = TRUE)` prints the reply token by token as it is generated.
- Budgets (`budget()`) are checked before every call; the call that would
  exceed a limit raises a typed `llmragent_budget_error` instead of spending.
  Budget integrity is end to end: tool loops report every internal model
  call and its tokens (via LLMR's `tool_loop` attribute), `max_tool_calls`
  is enforced inside a running tool loop (not only between turns), memory
  compaction calls land on the agent's meter, and [load_agent()] restores
  the counters so a budget keeps binding across sessions.
- Failures are errors, never replies: an API error propagates and leaves
  memory untouched.
- `trace()` records every call, tool run, memory compaction, and budget stop
  with tokens and timings; `usage()` totals them.
- `ask_structured()` no longer sends the agent's tools alongside a schema
  request, avoiding provider conflicts between tool-choice and schema mode.

## Delegation and pipelines

- `agent_as_tool()` exposes an agent as an `LLMR::llm_tool()`, so other
  agents can consult it: supervisor/specialist hierarchies with attributed
  spend and nested budgets.
- `agent_pipeline()` chains agents into an assembly line; every
  intermediate product is kept in a tidy `steps` frame.

## Memory

- Three drop-in policies: `memory_buffer()` (last n), `memory_summary()`
  (auto-compacts older history into a summary note; optionally billed to a
  dedicated cheaper model via `config =`), and `memory_recall()`
  (embedding-based retrieval of relevant older exchanges).
- `save_agent()` / `load_agent()` round-trip an agent (config keys stay
  environment references; tools re-attach at load).

## Conversations and study presets

- `conversation()`: shared, speaker-attributed transcripts; round-robin,
  random, or moderator-chosen turn order; stop rules.
- Presets returning tidy, classed objects with print and `as.data.frame()`
  methods: `debate()` (phased transcript + structured verdict),
  `focus_group()` (rotating speaking order + moderator synthesis),
  `interview()` (scripted questions + adaptive probes), `deliberate()`
  (discussion + private structured votes + tally).

## Experiments and orchestration

- `agent_experiment()`: factorial designs (conditions x replications),
  sequential or parallel via `future`, per-cell error capture, one tidy
  results frame.
- `think_harder()`: one strong model plans, synthesizes, and verifies while
  many cheap models work the approaches in parallel; all intermediate
  products are retained.
