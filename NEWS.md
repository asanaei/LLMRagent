# LLMRagent 0.8.0

A reproducibility, governance, validity, and scale overhaul. The core
agent is unchanged; every run now funnels through one unified object, and
new layers wrap that object with provenance, control, defensible
inference, and an auditable workflow runtime. Depends on LLMR (>= 0.8.7).

## Provenance and reproducibility

- [as_agent_run()] converts any run (an `Agent`, [conversation()],
  [debate()], [focus_group()], [interview()], [deliberate()],
  [agent_pipeline()], [agent_experiment()], or [think_harder()]) into one
  unified `agent_run`: a rich event graph of utterances, model calls, tool
  calls, state transitions, and budget/guardrail events.
- `tibble::as_tibble(run, level = )` exposes five tidy views of a run:
  `"utterance"`, `"event"`, `"call"`, `"tool"`, and `"state"`.
- [agent_manifest()] builds a study manifest of content hashes (persona,
  tools, workflow) plus the environment, via [hash_persona()],
  [hash_tool_spec()], and [hash_workflow()]; the hashes interoperate with
  LLMR.
- [archive_agent_study()] writes a sealed, replayable archive whose call
  log is replayable with [replay_run()].
- `diagnostics()` and `report()` (LLMR generics) gain `agent_run` methods
  for integrity checks and human-readable summaries.

## Governance and control

- [agent_tool()] declares a tool's side effects (`side_effects =`),
  approval needs (`requires_approval =`), and quotas (`max_calls =`,
  `timeout_s =`, `max_bytes =`) up front; the manifest hashes them, so a
  writing tool cannot pass as inert.
- [guardrail()] and [guardrails()] attach input, output, and tool checks to
  an agent (`agent(guardrails = )`); a rejection raises
  `llmragent_guardrail_block`.
- [human_gate()] wraps a tool so a call pauses for a person; the pause
  raises `llmragent_pending_approval` carrying a checkpoint that
  [approve_tool_call()] and [resume_run()] act on.
- [check_state_leakage()] diagnoses cross-condition contamination in an
  experiment.

## Validity

- [persona_frame()] builds a provenanced persona (`source =`, `scope =`,
  `attributes =`); [persona_variants()] generates counterfactual sets and
  [persona_audit()] scans for essentialism and stereotype.
- [mark_claim_type()] enforces claim-type discipline and [llm_claim_lint()]
  flags claims unsupported by a run's evidence; an unsupported assertion
  raises `llmragent_claim_error`.
- [agent_calibrate()] bridges imperfect labels to a defensible estimate via
  design-based and prediction-powered estimation (`method =`,
  `estimand =`); [attach_calibration()] binds it to a run, and a
  `"calibrated_inference"` claim requires it. [as_llmrcontent_validation()]
  coerces to an LLMR content validation.
- [agent_robustness()] runs a robustness battery (`vary =`, `measure =`)
  with [vary_models()], [vary_temperature()], [vary_prompt()],
  [vary_persona()], and [vary_option_order()].

## Workflows and scale

- [agent_workflow()], [add_node()], [add_edge()], and [run_workflow()] add a
  small, auditable DAG runtime; every node boundary is a checkpoint, so
  [resume_workflow()], [fork_workflow()], and [replay_run()] reproduce or
  branch a run, and a divergent replay raises `llmragent_replay_mismatch`.
  [workflow_from_pipeline()] lifts an [agent_pipeline()] into a workflow.
- [mcp_tools()] is a governed Model Context Protocol client (`policy =`,
  `transport =`); it defaults to read-only with schema pinning and raises
  `llmragent_mcp_schema_drift` when a server's schema changes.
- [sandbox_tool()] runs a tool under isolation (`mode =`, `executor =`); a
  breach raises `llmragent_sandbox_violation`.
- [agent_population()], [society()], [step_interaction()],
  [collect_measures()], [exposure_matrix()], and [contamination_report()]
  scaffold social simulation; [view_run()] renders an HTML run inspector.

## Breaking changes

- [interview()] now returns a classed `agent_interview` that carries
  provenance; use `as.data.frame()` for the old tibble.
- [agent_experiment()] now returns a classed `agent_experiment` rather than
  a bare results frame.
- [save_agent()] and the `.rds` format changed: spans replace the former
  `trace` field, so archives written by 0.7.x do not round-trip.

# LLMRagent 0.7.1

A ground-up rewrite on LLMR (>= 0.8.7). The package now centers on one
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
