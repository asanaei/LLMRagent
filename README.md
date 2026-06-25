# LLMRagent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRagent logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRagent/)
<!-- badges: end -->

Language-model **agents for R**, built on
[LLMR](https://github.com/asanaei/LLMR). An agent here is a model and a persona
that carries memory, calls tools natively, and works under a budget it cannot
overspend. Agents consult one another and hold conversations over a shared
transcript. A factorial design runs hundreds of them at once. Each run records
its own provenance and seals into a replication archive, while the governance
and validity tooling stays in reserve for studies that need it. The package
suits social scientists running agent-based studies, and anyone in R who needs
an agent with memory, tools, and a budget it keeps to.

```r
# install.packages("remotes")
remotes::install_github("asanaei/LLMRagent")

library(LLMRagent)
cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")   # any LLMR provider works

ada <- agent("Ada", cfg, persona = "A meticulous statistician. Be brief.")
ada$chat("What is overfitting?")
ada$chat("How do I detect it?")                # remembers the thread
ada$chat("Now explain it to a child.", stream = TRUE)   # tokens print live
ada$usage()                                    # calls, tokens, tool calls, seconds
```

## What it does

**Agents.** `agent()` wraps a persona and an `LLMR::llm_config()` with:

- *Tools*: expose any R function via `LLMR::llm_tool()`; the agent's tool
  calls are run and their results returned to the model until it answers.
- *Memory*: last-n buffer, auto-summarizing memory that compacts itself when
  the conversation grows (optionally billed to a cheaper model), or
  embedding-based recall (`?memory`).
- *Budgets*: `budget(max_calls, max_tokens, max_tool_calls, max_seconds)` is
  checked **before** each call; the call that would exceed it raises a typed
  error instead of spending.
- *Traces*: `agent$trace()` is a tibble of every call, tool run, and memory
  compaction, with tokens and timings. Failures raise errors; they are never
  recorded as something the model said.

**Agents calling agents.** `agent_as_tool(specialist)` turns an agent into
a tool any other agent can consult. Supervisors route work to specialists at
their own discretion; each consultation lands on the specialist's own meter
and respects its own budget.

```r
stat <- agent("Stat", cfg, persona = "A PhD statistician. Precise about assumptions.")
lead <- agent("Lead", cfg, persona = "A research lead. Consult specialists, then synthesize.",
              tools = list(agent_as_tool(stat)))
lead$chat("Could falling crime cause rising policing budgets, rather than vice versa?")
```

**Pipelines.** `agent_pipeline()` passes text through a fixed sequence of
specialists (extract, then verify, then rewrite), keeping every intermediate
product in a tidy `steps` frame.

**Multi-agent conversations.** `conversation()` runs agents over a shared,
speaker-attributed transcript (everyone sees the full dialogue), with
round-robin, random, or moderator-chosen turn order. Ready-made study
formats, each returning analysis-ready tibbles:

| Preset | Returns |
|---|---|
| `debate(pro, con, topic, judge =)` | phased transcript + structured verdict |
| `focus_group(moderator, participants, topic)` | utterance-level transcript + moderator synthesis |
| `interview(interviewer, respondent, topic)` | tidy question/answer frame with adaptive probes |
| `deliberate(agents, proposal)` | discussion transcript + private structured votes + tally |

**Agent experiments.** `agent_experiment(design, run_fn, reps)` runs a
factorial design (conditions x replications), sequentially or in parallel,
with per-cell error capture, returning one tidy results frame. Combine with
`LLMR::llm_log_enable()` for a per-call audit file of the entire study.

These primitives combine. As one worked example, `think_harder()` puts a strong
model and a pool of cheap ones through a plan, work, and synthesize loop. It is
built from the pieces above, not a separate idea.

## A small multi-agent example

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

- *LLMRagent in 10 minutes*: agents, tools, budgets, delegation, pipelines.
- *Designed conversations*: debates, focus groups, interviews, deliberations.
- *Coordinating strong and cheap models*: a worked example with `think_harder()`.
- *A deliberation experiment*: a complete factorial study with analysis.

All articles and reference: <https://asanaei.github.io/LLMRagent/>

## Relation to LLMR

[LLMR](https://asanaei.github.io/LLMR/) supplies the provider layer for more
than fourteen providers: retries and structured output, native tool execution,
streaming and parallel calls, audit logging, and the batch APIs. LLMRagent adds
the agent abstractions on top, so anything configured in LLMR (provider, model,
sampling, caching, logging) works unchanged here.

## The LLMR ecosystem

LLMRagent is one of a family of packages for LLM-assisted research built on
[LLMR](https://asanaei.github.io/LLMR/), the shared provider layer.
[LLMRcontent](https://asanaei.github.io/LLMRcontent/) is the measurement
package: codebook-first coding, validation against held-out human labels,
robustness audits, and replication archives built from the audit log.
[LLMRpanel](https://asanaei.github.io/LLMRpanel/) administers survey instruments
to panels of model personas for design-stage work, and marks its output
uncalibrated until it is compared against a human benchmark.
[FocusGroup](https://asanaei.github.io/FocusGroup/) is the dedicated package for
moderated discussion. LLMRagent's `focus_group()` preset runs a session in a few
lines; use the FocusGroup package for a richer implementation, with desire-based
turn-taking and the turn-level dynamics studied in their own right. The
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/) introduces the whole
family.

