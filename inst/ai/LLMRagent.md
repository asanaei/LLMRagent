---
name: llmragent
description: LLM agents for R built on LLMR - personas, tools, pluggable memory, hard budgets, agents delegating to agents, multi-agent conversations (debate, focus group, interview, deliberation), factorial agent experiments, strong-plus-cheap orchestration.
---

# LLMRagent — usage capsule for AI assistants

One file to use the package correctly. For depth: the four vignettes
(`getting-started`, `designed-conversations`, `super-brain`,
`deliberation-experiment`).

## Install

```r
remotes::install_github("asanaei/LLMRagent")   # depends on LLMR (>= 0.8.3)
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

## Test seam

`Agent$new(..., caller = , stream_caller = )` accepts injected callers; a
caller receives `(config, messages, tools, ...)` and returns an
`llmr_response`-shaped object. All presets accept agents built this way,
so whole studies run offline.

## Error meanings

- `llmragent_budget_error` → the budget refused the next call; raise the
  budget or accept the stop.
- "must be created with budget()" / "must be a list of Agent objects" →
  constructor misuse.
- Moderator policy requires `moderator = agent(...)`.
