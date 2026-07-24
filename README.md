# LLMRagent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRagent logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRagent/)
<!-- badges: end -->

Language-model agents for R, built on
[LLMR](https://github.com/asanaei/LLMR).

## What LLMRagent supports

LLMRagent defines governed single-agent and multi-agent studies. An agent
combines a model configuration and persona with memory, declared tool powers,
boundary checks, and a budget. Designed conversations and factorial studies
return classed results with transcripts, question-and-answer tables, votes, or
condition-level results. Calls, tool use, and state changes can be collected in
an analysis-ready run record.

## Install and configure a model

```r
# install.packages("remotes")
remotes::install_github("asanaei/LLMRagent")

library(LLMRagent)
cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
```

`agent()` accepts any model configuration supported by LLMR. Provider
credentials remain in the environment rather than in the agent definition.

## Build an agent with explicit limits

```r
ada <- agent(
  "Ada",
  cfg,
  persona = "A meticulous statistician. Be brief.",
  memory = memory_buffer(keep = 20L),
  budget = budget(
    max_calls = 6L,
    max_tokens = 8000L,
    max_tool_calls = 4L,
    max_seconds = 120
  )
)

ada$chat("What is overfitting?")
ada$chat("How do I detect it?")
ada$usage()
ada$trace()
```

`chat()` stores successful exchanges in the selected memory policy. Call and
tool-call ceilings are checked before the corresponding operation. Token and
elapsed-time limits stop the next model round after recorded use reaches the
limit. Failures raise conditions and are not stored as model replies.

## Give tools declared powers

`agent_tool()` exposes an R function while declaring its `side_effects`,
approval requirement, and per-tool limits. `side_effects` is one of `"none"`,
`"read"`, `"write"`, or `"external"`. A `guardrail()` checks an agent input,
output, or tool call at a named boundary.

```r
lookup_gdp <- agent_tool(
  fn = function(country) {
    values <- c(chile = 335, uruguay = 81, bolivia = 47)
    value <- values[tolower(country)]
    if (is.na(value)) "unknown" else paste0("$", value, " billion")
  },
  name = "lookup_gdp",
  description = "Look up a country's illustrative GDP in USD billions.",
  parameters = list(country = list(type = "string")),
  required = "country",
  side_effects = "read",
  requires_approval = FALSE,
  max_calls = 5L
)

no_file_writes <- guardrail(
  "no_file_writes",
  check = function(payload, context) {
    if (identical(payload$name, "write_file")) "file writes are not allowed" else TRUE
  },
  stage = "tool"
)

analyst <- agent(
  "Analyst",
  cfg,
  tools = list(lookup_gdp),
  guardrails = guardrails(no_file_writes)
)
```

Use `human_gate()` to add the same requirement as
`requires_approval = TRUE`. A gated call pauses before the function runs. The
checkpoint exposes the proposed tool name and arguments for a decision.

```r
write_note <- human_gate(agent_tool(
  function(path, text) writeLines(text, path),
  name = "write_note",
  description = "Write text to a file.",
  parameters = list(
    path = list(type = "string"),
    text = list(type = "string")
  ),
  required = c("path", "text"),
  side_effects = "write"
))

scribe <- agent("Scribe", cfg, tools = list(write_note))
pending <- tryCatch(
  scribe$chat("Write a short note to notes.txt."),
  llmragent_pending_approval = function(e) e$checkpoint
)
pending$pending

approved <- approve_tool_call(pending, decision = "approve")
resumed <- resume_run(approved)
resumed$text
```

Approval can approve, reject, or edit the proposed arguments. Plain
`LLMR::llm_tool()` objects remain accepted, but they do not carry the declared
powers and limits supplied by `agent_tool()`.

## Run designed multi-agent studies

`conversation()` provides general dialogue over one attributed transcript.
The study presets define turn structure according to the result sought.

| Function | Purpose | Primary result |
|---|---|---|
| `conversation()` | General multi-agent exchange | Attributed transcript |
| `debate()` | Phased opposing cases | Transcript and optional verdict |
| `focus_group()` | Reactions within a moderated group | Transcript and moderator synthesis |
| `interview()` | Questions and probes for one respondent | Tidy question-and-answer data in `$qa` |
| `deliberate()` | Discussion followed by a group decision | Transcript, private votes, and decision |

```r
panel <- list(
  agent("Morgan", cfg, persona = "An operations manager who values predictability."),
  agent("Sam", cfg, persona = "A young engineer who values flexibility."),
  agent("Ren", cfg, persona = "A finance director focused on costs.")
)

d <- deliberate(panel, "Adopt a four-day work week for a one-year pilot.")
d$transcript
d$votes
d$decision
```

## Vary conditions and assess robustness

`agent_experiment()` expands a design by replication, runs one fresh procedure
per cell, and records cell-level errors without stopping the study.

```r
design <- expand.grid(
  framing = c("benefit", "cost"),
  stringsAsFactors = FALSE
)

study <- agent_experiment(design, reps = 3L, run_fn = function(cond, rep) {
  subject <- agent("Subject", cfg, quiet = TRUE)
  subject$reply(paste("Assess the proposal using a", cond$framing, "frame."))
})
```

`agent_robustness()` applies declared perturbation axes and reports how a
chosen measure changes. The axis helpers are `vary_models()`,
`vary_temperature()`, `vary_prompt()`, `vary_persona()`, and
`vary_option_order()`. `persona_variants()` builds planned persona contrasts.
`mark_claim_type()` records whether a run is an instrument pilot, theory probe,
or coding exercise; none of these labels turns model output into a population
estimate.

## Inspect and archive run records

`as_agent_run()` gives high-level results a common record with utterance,
event, call, tool, and state views. Diagnostics, methods text, and a manifest
are derived from that record.

```r
run <- as_agent_run(d)
tibble::as_tibble(run, level = "utterance")
tibble::as_tibble(run, level = "tool")
diagnostics(run)
report(run)

manifest <- agent_manifest(run)
archive <- archive_agent_study(
  run,
  path = "four-day-study",
  include_messages = FALSE
)
```

`archive_agent_study()` writes an inspectable directory containing projected
run data, call records, the study manifest, methods text, artifacts, and file
hashes. The archive omits live agents and functions. It is a record for
inspection, not a general mechanism for executing the study again.

## Delegate and coordinate models

`agent_as_tool()` lets a supervisor decide when to consult a specialist. The
specialist's calls count against its own budget and appear in its usage record.

```r
stat <- agent(
  "Stat",
  cfg,
  persona = "A statistician who states assumptions and threats to inference."
)
lead <- agent(
  "Lead",
  cfg,
  persona = "A research lead who consults specialists before synthesis.",
  tools = list(agent_as_tool(stat))
)
lead$chat("Could falling crime cause rising policing budgets, rather than vice versa?")
```

`agent_pipeline()` runs a fixed sequence of specialists and retains each
intermediate result in its `steps` table. `agent_fanout_synthesis()` assigns
several independent approaches to one worker configuration, then uses another
configuration for planning, synthesis, and optional verification.

## Run resumable procedures

Workflows represent procedures that need branches, checkpoints, forks, or
resumption beyond the higher-level study functions.

```r
wf <- agent_workflow("triage")
wf <- add_node(wf, "clean", function(state) {
  state$text <- trimws(state$input)
  state
})
wf <- add_node(wf, "review", function(state) {
  state$ok <- nzchar(state$text)
  state
})
wf <- add_edge(wf, "clean", "review")

wrun <- run_workflow(wf, input = " a claim ")
replayed <- replay_run(wrun, wf, verify = "structural")
```

`run_workflow()` records state transitions and can write checkpoints.
`resume_workflow()` continues a checkpointed workflow, and `fork_workflow()`
branches from recorded state. `replay_run()` re-executes a workflow run and
checks it at the requested verification level. It does not re-execute an
arbitrary archived agent run.

## External tools and inspection

`mcp_tools()` exposes tools from a Model Context Protocol server under a
read-only or read-write policy, with optional approval for writes and schema
pinning. `view_run()` writes a self-contained HTML view of a run record.

Agent persistence is separate from study archives. `save_agent()` stores an
agent's configuration, memory, budget, guardrails, and accounting, but omits
tool functions. `load_agent()` restores that state and accepts tools to
reattach.

## Vignettes

- *LLMRagent in 10 minutes*: agents, declared tools, memory, and budgets.
- *Governed tools and human review*: tool powers, boundary checks, and approval.
- *Designed conversations*: debates, focus groups, interviews, and deliberations.
- *A deliberation experiment*: a factorial study with analysis.
- *Validity: robustness, personas, and scoped claims*: perturbations and claim scope.
- *Provenance and archiving*: run views, manifests, reports, and archives.
- *Fan-out synthesis*: coordination across worker and synthesis models.
- *Workflows*: checkpoints, branches, resumption, forks, and workflow replay.

All articles and reference: <https://asanaei.github.io/LLMRagent/>

## Relation to LLMR

[LLMR](https://asanaei.github.io/LLMR/) version 0.8.11 or later supplies the
common provider interface used by LLMRagent. `agent()` accepts an
`LLMR::llm_config()` object, so provider and model settings are shared between
the packages.

## The LLMR ecosystem

LLMRagent is one of a family of packages for LLM-assisted research built on
[LLMR](https://asanaei.github.io/LLMR/), the shared provider interface.
[LLMRcontent](https://asanaei.github.io/LLMRcontent/) codes text from a codebook
and evaluates the resulting labels against held-out human labels.
[LLMRpanel](https://asanaei.github.io/LLMRpanel/) administers survey and
experimental instruments to panels of model personas for design-stage studies.
[FocusGroup](https://asanaei.github.io/FocusGroup/) runs simulated moderated
discussions and supports experiments on how one turn changes the next. The
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/) introduces the whole
family.

## Contributing

Report bugs and feature requests in the
[GitHub repository](https://github.com/asanaei/LLMRagent). Pull requests may be
submitted there.

## License

This project uses the MIT License; see `LICENSE`.
