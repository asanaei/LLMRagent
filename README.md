# LLMRAgent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRAgent logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRAgent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRAgent/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRAgent/)
<!-- badges: end -->

Language-model **agents for R**, built on [LLMR](https://github.com/asanaei/LLMR):
personas, native tool calling, pluggable memory, hard budgets, agents that
delegate to other agents, multi-agent conversations with tidy transcripts,
factorial agent experiments, and a strong-plus-cheap model orchestrator.
Designed for social scientists running agent-based studies, and equally for
anyone in R who wants a capable agent in five lines.

```r
# install.packages("remotes")
remotes::install_github("asanaei/LLMRAgent")

library(LLMRAgent)
cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")   # any LLMR provider works

ada <- agent("Ada", cfg, persona = "A meticulous statistician. Be brief.")
ada$chat("What is overfitting?")
ada$chat("How do I detect it?")                # remembers the thread
ada$chat("Now explain it to a child.", stream = TRUE)   # tokens print live
ada$usage()                                    # calls, tokens, tool calls, seconds
```

## What it gives you

**Agents** — `agent()` wraps a persona and an `LLMR::llm_config()` with:

- *Tools*: expose any R function via `LLMR::llm_tool()`; the agent's tool
  calls are executed automatically and fed back until it answers.
- *Memory*: last-n buffer, auto-summarizing memory that compacts itself when
  the conversation grows (optionally billed to a cheaper model), or
  embedding-based recall (`?memory`).
- *Budgets*: `budget(max_calls, max_tokens, max_tool_calls, max_seconds)` is
  checked **before** each call; the call that would exceed it raises a typed
  error instead of spending.
- *Traces*: `agent$trace()` is a tibble of every call, tool run, and memory
  compaction, with tokens and timings. Failures raise errors; they are never
  recorded as something the model said.

**Agents calling agents** — `agent_as_tool(specialist)` turns an agent into
a tool any other agent can consult. Supervisors route work to specialists at
their own discretion; each consultation lands on the specialist's own meter
and respects its own budget.

```r
stat <- agent("Stat", cfg, persona = "A PhD statistician. Precise about assumptions.")
lead <- agent("Lead", cfg, persona = "A research lead. Consult specialists, then synthesize.",
              tools = list(agent_as_tool(stat)))
lead$chat("Could falling crime cause rising policing budgets, rather than vice versa?")
```

**Pipelines** — `agent_pipeline()` chains specialists into an assembly line
(extract, then verify, then rewrite), keeping every intermediate product in
a tidy `steps` frame.

**Multi-agent conversations** — `conversation()` runs agents over a shared,
speaker-attributed transcript (everyone sees the full dialogue), with
round-robin, random, or moderator-chosen turn order. Ready-made study
formats, each returning analysis-ready tibbles:

| Preset | Returns |
|---|---|
| `debate(pro, con, topic, judge =)` | phased transcript + structured verdict |
| `focus_group(moderator, participants, topic)` | utterance-level transcript + moderator synthesis |
| `interview(interviewer, respondent, topic)` | tidy question/answer frame with adaptive probes |
| `deliberate(agents, proposal)` | discussion transcript + private structured votes + tally |

**Agent experiments** — `agent_experiment(design, run_fn, reps)` runs a
factorial design (conditions x replications), sequentially or in parallel,
with per-cell error capture, returning one tidy results frame. Combine with
`LLMR::llm_log_enable()` for a per-call audit file of the entire study.

**The super-brain** — `think_harder(problem, strong_config, cheap_config)`:
one strong model plans and synthesizes; many cheap models work the
approaches in parallel; an optional hostile-reviewer pass repairs flaws.
Strong-model spend stays at two to four calls regardless of fan-out, and all
intermediate products are kept for inspection.

## A taste of multi-agent work

```r
panel <- list(
  agent("Morgan", cfg, persona = "An operations manager who values predictability."),
  agent("Sam",    cfg, persona = "A young engineer, enthusiastic about flexibility."),
  agent("Ren",    cfg, persona = "A finance director fixated on costs. Blunt.")
)

d <- deliberate(panel, "Adopt a four-day work week for a one-year pilot.")
d$transcript     # tidy: turn, round, speaker, text
d$votes          # private structured votes with reasons
d$decision
```

## Vignettes

- *LLMRAgent in 10 minutes* — agents, tools, budgets, delegation, pipelines.
- *Designed conversations* — debates, focus groups, interviews, deliberations.
- *The super-brain pattern* — strong-plus-cheap orchestration.
- *A deliberation experiment* — a complete factorial study with analysis.

All articles and reference: <https://asanaei.github.io/LLMRAgent/>

## Relation to LLMR

[LLMR](https://github.com/asanaei/LLMR) supplies the provider layer: 14+
providers, retries, structured output, tool execution, streaming, parallel
calls, audit logging, batch APIs. LLMRAgent adds the agent abstractions on
top. Anything configured in LLMR (provider, model, sampling, caching,
logging) works unchanged here.

## License

MIT. Author: Ali Sanaei.
