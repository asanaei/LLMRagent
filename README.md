# LLMRagent <img src="man/figures/logo.png" align="right" width="120" alt="LLMRagent logo" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/asanaei/LLMRagent/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Website](https://img.shields.io/badge/docs-pkgdown-blue.svg)](https://asanaei.github.io/LLMRagent/)
<!-- badges: end -->

Language-model **agents for R**, built on
[LLMR](https://github.com/asanaei/LLMR). An agent combines a model configuration
and persona with memory, tools, and a declared budget. Agents consult one
another and hold conversations over a shared transcript. `agent_experiment()`
runs factorial designs over conditions and replications. Each run records calls
and tool use in a trace that can be stored in a replication archive. The
package is intended for agent-based studies in R.

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
a tool any other agent can consult. The calling agent chooses when to invoke
the specialist. Each consultation is recorded in the specialist's usage and
counts against its budget.

```r
stat <- agent("Stat", cfg, persona = "A PhD statistician. Precise about assumptions.")
lead <- agent("Lead", cfg, persona = "A research lead. Consult specialists, then synthesize.",
              tools = list(agent_as_tool(stat)))
lead$chat("Could falling crime cause rising policing budgets, rather than vice versa?")
```

**Pipelines.** `agent_pipeline()` passes text through a fixed sequence of
specialists (extract, then verify, then rewrite), keeping every intermediate
product in a tidy `steps` frame.

**Multi-agent conversations.** `conversation()` records each speaker's turns
in a shared transcript that all agents receive. Turn order can be round-robin,
random, or selected by a moderator. The following study formats return data
frames for analysis:

| Preset | Returns |
|---|---|
| `debate(pro, con, topic, judge =)` | phased transcript + structured verdict |
| `focus_group(moderator, participants, topic)` | utterance-level transcript + moderator synthesis |
| `interview(interviewer, respondent, topic)` | tidy question/answer frame with adaptive probes |
| `deliberate(agents, proposal)` | discussion transcript + private structured votes + tally |

**Agent experiments.** `agent_experiment(design, run_fn, reps)` runs a
factorial design over conditions and replications. It can run sequentially or
in parallel and records errors by design cell in the returned data frame.
Combine with `LLMR::llm_log_enable()` for a per-call audit file of the entire
study.

`think_harder()` composes these functions by using one model for planning and
synthesis and a pool of less expensive models for the intervening work.

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

[LLMR](https://asanaei.github.io/LLMR/) supplies the common provider interface
used by LLMRagent. `agent()` accepts an `LLMR::llm_config()` object, so provider
and model settings are shared between the packages.

## The LLMR ecosystem

LLMRagent is one of a family of packages for LLM-assisted research built on
[LLMR](https://asanaei.github.io/LLMR/), the shared provider layer.
[LLMRcontent](https://asanaei.github.io/LLMRcontent/) codes text from a codebook
and evaluates the resulting labels against held-out human labels.
[LLMRpanel](https://asanaei.github.io/LLMRpanel/) administers survey and
experimental instruments to panels of model personas for design-stage studies.
[FocusGroup](https://asanaei.github.io/FocusGroup/) runs simulated moderated
discussions and supports experiments on how one turn changes the next. The
[ecosystem page](https://asanaei.github.io/LLMR-ecosystem/) introduces the whole
family.
