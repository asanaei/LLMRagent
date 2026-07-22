---
name: llmragent
description: Governed language-model agents for research workflows in R, built on LLMR, with run records that omit live agents and functions and controlled tool execution.
---

# LLMRagent usage capsule

LLMRagent 0.8.1 requires LLMR 0.8.11 or later. It treats agents as research
instruments whose prompts, calls, tool use, and limits should remain
inspectable.

## Install

```r
install.packages("LLMRagent")
library(LLMRagent)
```

Provider keys come from environment variables through `LLMR::llm_config()`.
Do not place literal keys in scripts or persisted objects.

## Core API

```r
cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")

a <- agent(
  name = "Ada",
  config = cfg,
  persona = "A meticulous statistician. Be brief.",
  tools = list(),
  memory = memory_buffer(),
  budget = budget(),
  guardrails = NULL,
  quiet = FALSE
)

a$chat("What is overfitting?")
a$reply("Give a stateless answer.")
a$ask_structured("Classify this.", schema = list(type = "object"))
a$trace()
a$usage()
a$transcript()
a$reset()

budget(max_calls = Inf, max_tokens = Inf,
       max_tool_calls = Inf, max_seconds = Inf)
memory_buffer(keep = 40L)
memory_summary(threshold_chars = 12000L, keep_last = 10L, config = NULL)
memory_recall(embed_config, keep_recent = 6L, k = 4L)
```

`max_calls` is checked before every actual model round, including compaction
and rounds inside a tool loop. `max_tool_calls` is checked before a tool runs.
`max_tokens` is a recorded-use gate: a response can move recorded use past the
limit, after which the next model round is refused. `max_seconds` likewise
stops the next round after recorded elapsed time reaches the limit.

`$chat()` writes the user turn and successful reply to memory. `$reply()` and
`$ask_structured()` are stateless. Failures are errors and are not stored as
model replies. Streaming is available through `$chat(..., stream = TRUE)` when
the agent has no tools.

## Governed tools and guardrails

```r
tool <- agent_tool(
  fn = function(city) paste0("22 C in ", city),
  name = "weather",
  description = "Read the current weather for a city.",
  parameters = list(city = list(type = "string")),
  side_effects = "external",
  requires_approval = FALSE,
  timeout_s = NULL,
  max_calls = 5L,
  max_bytes = 10000L
)

g <- guardrail(
  "no_write",
  check = function(payload, context) {
    if (identical(payload$name, "write_file")) "write blocked" else TRUE
  },
  on_fail = "block",
  stage = "tool"
)

guarded <- agent("G", cfg, tools = list(tool), guardrails = guardrails(g))
```

`side_effects` accepts `"none"`, `"read"`, `"write"`, or `"external"`.
Tool guardrails receive the tool name and arguments before execution and the
result after execution. A blocking pre-execution verdict prevents the call.
If `timeout_s` is set, `callr` must be installed so the timeout can be
enforced. `max_bytes` includes the truncation marker.

```r
gated_tool <- human_gate(agent_tool(
  function(path, text) writeLines(text, path),
  name = "write_note",
  description = "Write text to a file.",
  parameters = list(
    path = list(type = "string"),
    text = list(type = "string")
  ),
  side_effects = "write"
))

pending <- tryCatch(
  agent("Scribe", cfg, tools = list(gated_tool))$chat("Write a note."),
  llmragent_pending_approval = function(e) e$checkpoint
)
approved <- approve_tool_call(pending, decision = "approve")
resumed <- resume_run(approved)
resumed$text
resumed$agent
resumed$checkpoint
```

Approval decisions are `"approve"`, `"reject"`, and `"edit"`. Rejection does
not run the tool. `resume_run()` returns an `agent_resume_result` with ordinary
fields rather than attributes.

## Delegation, pipelines, and conversations

```r
agent_as_tool(x, name = NULL, description = NULL)
agent_pipeline(agents, input, quiet = FALSE, ...)

conversation(agents, topic, turn_policy = "round_robin", max_turns = 6L)
debate(pro, con, topic, rounds = 2L, judge = NULL)
focus_group(moderator, participants, topic, questions = NULL)
iv <- interview(interviewer, respondent, topic, questions = NULL)
iv$qa
deliberate(agents, proposal, rounds = 2L,
           options = c("yes", "no", "abstain"))

agent_experiment(design, run_fn, reps = 1L,
                 parallel = FALSE, quiet = FALSE)

out <- agent_fanout_synthesis(
  problem,
  strong_config,
  cheap_config,
  n_approaches = 4L,
  verify = TRUE
)
out$answer
out$workers
```

Conversations use one attributed transcript. Each speaker's own earlier turns
are rendered as assistant messages and other speakers' turns as labeled user
messages. The interview return is an `agent_interview` object; its tidy
question-and-answer frame is in `$qa` and `as.data.frame(iv)`.

`agent_fanout_synthesis()` returns an `agent_fanout_result`. It uses a strong
model to plan and synthesize, plus several inexpensive worker calls. Model
sampling remains nondeterministic.

## Provenance and archiving

```r
run <- as_agent_run(a)
tibble::as_tibble(run, level = "utterance")
tibble::as_tibble(run, level = "event")
tibble::as_tibble(run, level = "call")
tibble::as_tibble(run, level = "tool")
tibble::as_tibble(run, level = "state")

agent_manifest(run)
hash_persona(a$persona, a$name)
hash_tool_spec(a$tools[[1]])
diagnostics(run)
report(run)

archive_agent_study(
  run,
  path = "study-archive",
  include_messages = FALSE,
  redact = NULL,
  overwrite = FALSE
)
```

`hash_tool_spec()` identifies the declared tool fields and function body. It
does not identify values captured in the function's enclosing environment.

An archive directory contains data views, call records, a manifest, methods
text, artifacts, file hashes, and an optional data-only `run.rds`. It never
serializes live agents, callers, tool functions, or configuration secrets.
Message omission and redaction apply to the data-only RDS as well as the text
formats. A nonempty destination is refused unless `overwrite = TRUE`.

## Personas, claims, and robustness

```r
p <- persona_frame(
  "A first-time voter in a competitive district.",
  source = "synthetic"
)
ps <- persona_variants(p, vary = list(age = c("22", "52")))
persona_audit(ps)

run <- mark_claim_type(run, "theory_probe")
llm_claim_lint("The configured model supports the policy.", run)

batt <- agent_robustness(
  run_fn,
  vary = list(temperature = c(0, 0.7)),
  measure = function(x) x$decision,
  config = cfg
)
diagnostics(batt)

vary_models("model-a", "model-b")
vary_temperature(0, 0.7)
vary_prompt("Question A", "Question B")
vary_persona("Cautious", "Risk tolerant")
vary_option_order("as_is", "reverse")
```

Claim types are `"instrument_pilot"`, `"theory_probe"`, and `"coding"`.
They scope what a run can support; none converts model output into a population
estimate. `persona_variants()`, `persona_audit()`, `vary_prompt()`, and
`agent_robustness()` use `config` when they consume one model configuration.

## Workflows

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
fork_workflow(wrun, wf)
replay_run(wrun, wf, verify = "structural")

workflow_from_pipeline(list(agent("A", cfg), agent("B", cfg)))
```

Plain function nodes receive and return `state`. Agent nodes read one state
field and write another. Checkpoints make resumption and branching explicit.
Strict replay can reproduce deterministic function nodes. A workflow that
calls a model is not deterministic unless its calls are served from recorded
responses; structural replay does not require sampled model text to match.

## MCP and inspection

```r
mcp_tools(config, policy = "read_only", approve_writes = TRUE,
          pin_schemas = TRUE, transport = NULL)
view_run(run, output = "run.html", open = FALSE)
check_state_leakage(experiment)
save_agent(a, "agent.rds")
load_agent("agent.rds", tools = list())
```

Under its default policy, the MCP client refuses calls to tools that are not
positively marked read-only. It pins advertised signatures, sanitizes
injection-like descriptions, and can route writes through approval.
`save_agent()` is separate from study archives: it persists a live agent and
refuses a config that contains a literal key (use an environment-variable
reference, the default).

## Main conditions

- `llmragent_budget_error`: the next model or tool call was refused by budget.
- `llmragent_guardrail_block`: a guardrail blocked input, output, or a tool call.
- `llmragent_pending_approval`: a tool call paused for a human decision.
- `llmragent_claim_error`: prose or a claim label exceeds the run's scope.
- `llmragent_mcp_schema_drift`: an MCP tool no longer matches its pinned spec.
- `llmragent_replay_mismatch`: a workflow replay diverged from recorded state.
- `llmragent_workflow_error`: a workflow failed its graph or step contract.

## Offline testing

Create agents with `agent()`. For offline tests, replay stored calls or replace
`LLMR::call_llm()` in the test.
