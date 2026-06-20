---
name: llmragent
description: Reproducible LLM agents for R on LLMR - personas, native tools, pluggable memory, hard budgets, agents delegating to agents, multi-agent conversations (debate, focus group, interview, deliberation), factorial experiments, plus a 1.0 reproducibility/governance/validity/scale stack - unified run objects with tidy event graphs, study manifests of content hashes, sealed replication archives, declared tool side effects and guardrails, human approval gates, robustness batteries, a calibration bridge, anti-essentialist persona tooling, claim-type discipline, an auditable workflow runtime with checkpoint/resume/fork/replay, a governed MCP client, sandboxed tools, and social-simulation scaffolding.
---

# LLMRagent — usage capsule for AI assistants

One file to use the package correctly. For depth: the vignettes
(`getting-started`, `designed-conversations`, `super-brain`,
`deliberation-experiment`).

The package has five layers. The core agent is unchanged from 0.7.x; the
1.0 release wraps it with provenance, governance, validity, and scale
helpers that all funnel through one unified run object.

## Install

```r
remotes::install_github("asanaei/LLMRagent")   # depends on LLMR (>= 0.8.7)
```

## Core API

```r
agent(name, config, persona = NULL, tools = list(),
      memory = memory_buffer(), budget = budget(), quiet = FALSE)
# methods: $chat(text, ..., stream = FALSE)  $reply(messages, ...)
#          $ask_structured(text, schema, ...) $trace() $usage()
#          $transcript() $reset()

budget(max_calls = Inf, max_tokens = Inf, max_tool_calls = Inf, max_seconds = Inf)
memory_buffer(keep = 40L)
memory_summary(threshold_chars = 12000L, keep_last = 10L, config = NULL)
memory_recall(embed_config, keep_recent = 6L, k = 4L)

agent_as_tool(x, name = NULL, description = NULL)   # agents calling agents
agent_pipeline(agents, input, quiet = FALSE, ...)

conversation(agents, topic, ..., turn_policy = c("round_robin", "random",
             "moderator"), max_turns = 2L * length(agents))
debate(pro, con, topic, rounds = 2L, judge = NULL, quiet = FALSE, ...)
focus_group(moderator, participants, topic, questions = NULL, ...)
interview(interviewer, respondent, topic, questions = NULL, ...)
deliberate(agents, proposal, rounds = 2L,
           options = c("yes", "no", "abstain"), ...)

agent_experiment(design, run_fn, reps = 1L, parallel = FALSE, quiet = FALSE)
think_harder(problem, strong_config, cheap_config, n_approaches = 4L,
             verify = TRUE, quiet = FALSE, ...)
save_agent(x, path); load_agent(path, tools = list(), embed_config = NULL)
```

### Provenance and archiving

```r
# Any run -> one unified run object with a rich event graph.
as_agent_run(x)                          # Agent, conversation, debate, focus_group,
                                         # interview, deliberate, pipeline, experiment, super_brain
tibble::as_tibble(run, level = "utterance")   # also "event", "call", "tool", "state"
agent_manifest(run)                      # study manifest: content hashes + environment
archive_agent_study(run, path)           # sealed, replayable archive on disk
diagnostics(run)                         # provenance + integrity checks (LLMR generic)
report(run)                              # human-readable summary (LLMR generic)
hash_persona(persona); hash_tool_spec(tool); hash_workflow(workflow)
```

### Governance and control

```r
# A tool whose side effects, approval needs, and quotas are declared up front.
agent_tool(fn, name, description, parameters,
           side_effects = "none",       # "none" | "read" | "write" | "network" | ...
           requires_approval = FALSE, max_calls = Inf,
           timeout_s = Inf, max_bytes = Inf)

guardrail(name, check, on_fail = "block", stage = "input")  # "input"|"output"|"tool"
guardrails(...)                          # bundle several
agent(name, config, guardrails = guardrails(...))           # attach to an agent

human_gate(tool)                         # wrap a tool so calls pause for a person
approve_tool_call(checkpoint, decision)  # "approve" | "deny" | "edit"
resume_run(checkpoint)                   # continue after approval
check_state_leakage(experiment)          # cross-condition contamination diagnostic
```

### Validity

```r
persona_frame(text, source = NULL, scope = NULL, attributes = NULL)  # provenanced persona
persona_variants(p, vary = NULL)         # counterfactual persona set
persona_audit(p)                         # essentialism / stereotype scan

mark_claim_type(run, type)               # "descriptive" | "predictive" | "calibrated_inference" | ...
llm_claim_lint(run)                      # flag claims unsupported by the run's evidence

agent_calibrate(predictions, gold, method = NULL, estimand = NULL)  # design-based / PPI bridge
attach_calibration(run, cal)             # bind a calibration to a run
as_llmrcontent_validation(x)             # coerce to an LLMR content validation

agent_robustness(run_fn, vary = NULL, measure = NULL)
vary_models(...); vary_temperature(...); vary_prompt(...)
vary_persona(...); vary_option_order(...)
```

### Scale and reach

```r
# Auditable workflow runtime (a small DAG with checkpoints).
agent_workflow(name)
add_node(workflow, name, fn, ...); add_edge(workflow, from, to)
run_workflow(workflow, input, ...); resume_workflow(checkpoint)
fork_workflow(checkpoint); replay_run(archive)
workflow_from_pipeline(pipeline)         # lift an agent_pipeline into a workflow

mcp_tools(config, policy = NULL, transport = NULL)   # governed Model Context Protocol client
sandbox_tool(fn, mode = NULL, executor = NULL)       # isolated tool execution

# Social-simulation scaffolding.
agent_population(...); society(...)
step_interaction(society, ...); collect_measures(society, ...)
exposure_matrix(society); contamination_report(society)

view_run(run)                            # HTML run inspector
```

## Canonical patterns

```r
library(LLMRagent)
cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.7)

# one agent, stateful
ada <- agent("Ada", cfg, persona = "A meticulous statistician. Brief.")
ada$chat("What is overfitting?"); ada$chat("How do I detect it?")

# delegation: the supervisor decides when to consult
lead <- agent("Lead", cfg, persona = "Consult specialists, then synthesize.",
              tools = list(agent_as_tool(
                agent("Stat", cfg, persona = "A PhD statistician."))))

# a deliberation with private structured votes
d <- deliberate(list(agent("A", cfg, persona = "Cautious."),
                     agent("B", cfg, persona = "Bold.")),
                proposal = "Adopt the pilot.", quiet = TRUE)
d$votes; d$decision

# strong planner + cheap workers
out <- think_harder("Hard problem text...",
                    strong_config = LLMR::llm_config("deepseek", "deepseek-reasoner"),
                    cheap_config  = cfg)

# PROVENANCE + ARCHIVE: turn any run into the unified object, inspect, seal it
run <- as_agent_run(d)
tibble::as_tibble(run, level = "utterance")   # tidy turns
tibble::as_tibble(run, level = "event")       # the full event graph
diagnostics(run)                              # integrity + provenance checks
m <- agent_manifest(run)                       # content hashes of persona/tools/workflow
archive_agent_study(run, path = tempfile(fileext = ".zip"))   # replayable

# GOVERNED TOOL + HUMAN GATE: a writing tool that pauses for approval
writer <- agent_tool(
  fn = function(path, text) writeLines(text, path),
  name = "write_note", description = "Write text to a file.",
  parameters = list(path = "string", text = "string"),
  side_effects = "write", requires_approval = TRUE, max_calls = 3L)
gated <- agent("Scribe", cfg, tools = list(human_gate(writer)),
               guardrails = guardrails(
                 guardrail("no_secrets",
                           check = function(x) !grepl("API_KEY", x),
                           on_fail = "block", stage = "output")))
# a pending call raises llmragent_pending_approval, carrying a checkpoint:
res <- tryCatch(gated$chat("Save a hello note."),
                llmragent_pending_approval = function(e) e)
# approve_tool_call(res$checkpoint, "approve"); resume_run(res$checkpoint)

# ROBUSTNESS + CALIBRATION: stress a run, then bridge to a defensible estimate
rob <- agent_robustness(
  run_fn = function(...) deliberate(list(agent("J", cfg)), "Adopt?", quiet = TRUE),
  vary   = vary_temperature(0, 0.7),
  measure = function(r) r$decision)
diagnostics(rob)
cal <- agent_calibrate(predictions = c(1, 0, 1), gold = c(1, 1, 1),
                       method = "ppi", estimand = "mean")
graded <- mark_claim_type(attach_calibration(run, cal), "calibrated_inference")

# A TINY WORKFLOW: two nodes, run, then replay from the archive
wf <- agent_workflow("triage")
wf <- add_node(wf, "draft", function(input, ...) list(text = input))
wf <- add_node(wf, "review", function(text, ...) list(ok = nchar(text) > 0))
wf <- add_edge(wf, "draft", "review")
wrun <- run_workflow(wf, input = "a claim to triage")
```

## Semantics that matter

- Budgets are checked BEFORE each call; exceeding raises
  `llmragent_budget_error` (catch with `tryCatch`). Tool loops count every
  internal model call and enforce `max_tool_calls` mid-loop. Counters
  survive `save_agent()`/`load_agent()`; the wall clock restarts.
- Failures are errors, never replies: an API error propagates and memory
  stays clean.
- `$chat()` writes memory; `$reply()` and `$ask_structured()` are
  stateless (conversations use `reply`, so the shared transcript is the
  single source of truth).
- `agent_as_tool()` consultations run on the SPECIALIST's meter and budget;
  a specialist's budget stop reads back to the supervisor as a tool-error
  string.
- `stream = TRUE` prints tokens live; unavailable with tools (falls back,
  with one warning); silent when the agent was built `quiet = TRUE`.
- Local seeds affect only the `"random"` turn policy and parallel streams;
  model sampling is server-side — do not sprinkle `set.seed()` for it.
- Provenance accrues automatically while an agent or conversation runs;
  you do not opt in. `$trace()` is a flat projection (unchanged), whereas
  `as_agent_run()` reconstructs the rich event graph: utterances, model
  calls, tool calls, state transitions, and budget/guardrail events.
- Tool side effects are part of the contract: `agent_tool()` declares them,
  and the manifest hashes them, so a `side_effects = "write"` tool cannot
  pass as inert. `mcp_tools()` defaults to READ-ONLY with schema pinning;
  it refuses calls when a server's schema drifts from the pinned copy.
- Calibration is a precondition, not decoration: a run cannot carry a
  `"calibrated_inference"` claim until a calibration is attached
  (`agent_calibrate()` then `attach_calibration()`); `mark_claim_type()`
  enforces the discipline and `llm_claim_lint()` flags overreach.
- Workflows are deterministic by construction: every node boundary is a
  checkpoint, so `resume_workflow()`, `fork_workflow()`, and
  `replay_run()` reproduce or branch a run; a divergent replay is an error,
  not a silent drift.

The 1.0 layers raise typed conditions so studies can fail loudly and be
caught precisely: `llmragent_budget_error`, `llmragent_guardrail_block`,
`llmragent_claim_error`, `llmragent_pending_approval`,
`llmragent_mcp_schema_drift`, `llmragent_replay_mismatch`,
`llmragent_sandbox_violation`, and `llmragent_workflow_error`.

## Test seam

`Agent$new(..., caller = , stream_caller = )` accepts injected callers; a
caller receives `(config, messages, tools, ...)` and returns an
`llmr_response`-shaped object. All presets accept agents built this way,
so whole studies run offline. Provenance, manifests, archives, robustness,
and workflows all work against injected callers, so the entire 1.0 stack
is exercisable without network access.

## Error meanings

- `llmragent_budget_error` → the budget refused the next call; raise the
  budget or accept the stop.
- `llmragent_guardrail_block` → a guardrail rejected input, output, or a
  tool call at the named stage.
- `llmragent_pending_approval` → a `human_gate()` tool needs a decision;
  the condition carries a checkpoint for `approve_tool_call()` /
  `resume_run()`.
- `llmragent_claim_error` → a claim type was asserted without the evidence
  it requires (e.g. `"calibrated_inference"` with no calibration attached).
- `llmragent_mcp_schema_drift` → an MCP server's tool schema no longer
  matches the pinned copy; re-pin deliberately before proceeding.
- `llmragent_sandbox_violation` → a `sandbox_tool()` breached its mode or
  resource limits.
- `llmragent_replay_mismatch` / `llmragent_workflow_error` → a replay
  diverged from its archive, or a workflow node/edge was misconfigured.
- "must be created with budget()" / "must be a list of Agent objects" →
  constructor misuse.
- Moderator policy requires `moderator = agent(...)`.
