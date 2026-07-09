# Stage 2: tool governance, guardrails, and human approval gates. Offline.

# ---- governed tools ---------------------------------------------------------

test_that("agent_tool is an llmr_tool carrying a governance policy", {
  t <- agent_tool(function(city) paste0("sunny in ", city),
                  name = "weather", description = "weather",
                  parameters = list(city = list(type = "string")),
                  side_effects = "external", max_calls = 2)
  expect_s3_class(t, "llmr_tool")
  gov <- attr(t, "governance")
  expect_identical(gov$side_effects, "external")
  expect_equal(gov$max_calls, 2)
  # the tool runs and respects its own policy
  expect_match(t$fn(city = "Cairo"), "sunny in Cairo")
})

test_that("a governed tool refuses calls beyond max_calls", {
  t <- agent_tool(function() "ok", name = "ping", description = "ping", max_calls = 1)
  expect_match(t$fn(), "ok")
  expect_match(t$fn(), "BLOCKED")    # second call refused, not executed
})

test_that("a governed tool truncates oversized results", {
  t <- agent_tool(function() paste(rep("x", 1000), collapse = ""),
                  name = "big", description = "big", max_bytes = 50)
  out <- t$fn()
  expect_true(grepl("truncated", out))
  expect_lte(nchar(out), 120)
})

test_that("max_bytes is enforced in bytes, on character boundaries", {
  # a multibyte payload: each character is 2 bytes in UTF-8
  t <- agent_tool(function() paste(rep("é", 100), collapse = ""),
                  name = "wide", description = "wide", max_bytes = 50)
  out <- t$fn()
  expect_true(grepl("truncated", out))
  kept <- sub(" \\.\\.\\..*$", "", out)
  expect_lte(nchar(kept, type = "bytes"), 50)   # the BYTE cap holds
  expect_true(all(strsplit(kept, "")[[1]] == "é"))  # no split code point
})

test_that("budget() validates its limits", {
  expect_error(budget(max_calls = -1), "max_calls")
  expect_error(budget(max_tokens = NA), "max_tokens")
  expect_error(budget(max_tool_calls = "3"), "max_tool_calls")
  expect_error(budget(max_seconds = c(1, 2)), "max_seconds")
  b <- budget(max_calls = 2L)               # integers are accepted
  expect_identical(b$max_calls, 2)
})

test_that("hash_tool_spec folds the governance policy into identity", {
  t1 <- agent_tool(function(x) x, "t", "d",
                   parameters = list(x = list(type = "string")), max_calls = 5)
  t2 <- agent_tool(function(x) x, "t", "d",
                   parameters = list(x = list(type = "string")), max_calls = 2)
  expect_false(identical(hash_tool_spec(t1), hash_tool_spec(t2)))  # different apparatus
})

# ---- guardrails -------------------------------------------------------------

test_that("a blocking input guardrail stops the call and records an event", {
  g <- guardrail("no_secret",
                 function(payload, context) if (grepl("secret", payload)) "mentions secret" else TRUE,
                 stage = "input")
  a <- fake_agent("A", list("ok"), guardrails = guardrails(g))
  expect_error(a$chat("tell me a secret"), class = "llmragent_guardrail_block")
  # the block is a durable event, not a silent failure
  ev <- tibble::as_tibble(as_agent_run(a), level = "event")
  expect_true(any(ev$event_type == "guardrail" & ev$status == "blocked"))
  # and trace() does NOT surface it (legacy vocabulary only)
  expect_false("guardrail" %in% a$trace()$event)
})

test_that("a passing input guardrail lets the call through and records ok", {
  g <- guardrail("len_ok", function(payload, context) if (nchar(payload) > 1000) "too long" else TRUE,
                 stage = "input")
  a <- fake_agent("A", list("the reply"), guardrails = guardrails(g))
  expect_identical(a$chat("short question"), "the reply")
  ev <- tibble::as_tibble(as_agent_run(a), level = "event")
  expect_true(any(ev$event_type == "guardrail" & ev$status == "ok"))
})

test_that("a warning guardrail proceeds but warns", {
  g <- guardrail("warn_x", function(payload, context) if (grepl("x", payload)) "has x" else TRUE,
                 on_fail = "warn", stage = "output")
  a <- fake_agent("A", list("xyz reply"), guardrails = guardrails(g))
  expect_warning(a$chat("hi"), "warn_x")
})

test_that("a tool-stage guardrail inspects executed tool calls", {
  # a guardrail that blocks when a tool result looks like an exfiltration
  g <- guardrail("no_exfil",
                 function(payload, context) {
                   if (grepl("BEGIN-EXFIL", payload$result %||% "")) "tool result flagged" else TRUE
                 },
                 stage = "tool")
  th_clean <- tibble::tibble(round = 1L, name = "lookup",
                             arguments = '{"q":"x"}', result = "a normal answer")
  th_bad <- tibble::tibble(round = 1L, name = "lookup",
                           arguments = '{"q":"x"}', result = "BEGIN-EXFIL secrets")

  # clean tool result passes, and a guardrail event is recorded
  a1 <- fake_agent("A", list(fake_response("ok", tool_history = th_clean)),
                   guardrails = guardrails(g))
  a1$chat("look it up")
  ev <- tibble::as_tibble(as_agent_run(a1), level = "event")
  expect_true(any(ev$event_type == "guardrail" & ev$tool == "no_exfil"))

  # a poisoned tool result is blocked
  a2 <- fake_agent("A", list(fake_response("ok", tool_history = th_bad)),
                   guardrails = guardrails(g))
  expect_error(a2$chat("look it up"), class = "llmragent_guardrail_block")
})

test_that("a blocked output is not written to memory", {
  g <- guardrail("block_out", function(payload, context) "always blocks",
                 stage = "output")
  a <- fake_agent("A", list("the reply"), guardrails = guardrails(g))
  expect_error(a$chat("hi"), class = "llmragent_guardrail_block")
  expect_equal(nrow(a$transcript()), 0L)   # nothing stored
})

# ---- human approval gates (the native pausable tool loop) -------------------

test_that("an approval-gated tool suspends the run with a checkpoint", {
  # The native loop calls LLMR::call_llm_robust directly, so stub it to return
  # a raw OpenAI-style tool-call response.
  tool_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 10L, rec = 2L, total = 12L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r1", duration_s = 0.01,
    raw = list(choices = list(list(message = list(
      content = "", tool_calls = list(list(
        id = "call_1", type = "function",
        `function` = list(name = "danger", arguments = '{"x":1}')))))))
  ), class = "llmr_response")

  gated <- agent_tool(function(x) paste0("did danger with ", x),
                      name = "danger", description = "a dangerous action",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("A", LLMR::llm_config("groq", "fake"), tools = list(gated), quiet = TRUE)

  cp <- with_stub_llmr("call_llm_robust", function(config, messages, ...) tool_resp, {
    tryCatch(a$chat("do the danger"),
             llmragent_pending_approval = function(e) e$checkpoint)
  })
  expect_s3_class(cp, "llmragent_checkpoint")
  expect_identical(cp$pending$name, "danger")
  # the checkpoint is serializable (resume tomorrow on another machine)
  f <- withr::local_tempfile(fileext = ".rds")
  saveRDS(cp, f); cp2 <- readRDS(f)
  expect_identical(cp2$pending$name, "danger")
})

test_that("approve + resume runs the tool and completes", {
  tool_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m", finish_reason = "tool",
    usage = list(sent = 10L, rec = 2L, total = 12L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r1", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = "call_1", type = "function",
      `function` = list(name = "danger", arguments = '{"x":1}')))))))), class = "llmr_response")
  final_resp <- fake_response("All done after approval.")

  # The tool records that it ran, so we can assert it actually executed on
  # resume (not just that the final model text came back).
  ran <- new.env(); ran$called <- FALSE; ran$arg <- NA
  gated <- agent_tool(function(x) { ran$called <- TRUE; ran$arg <- x; paste0("executed:", x) },
                      name = "danger", description = "d",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("A", LLMR::llm_config("groq", "fake"), tools = list(gated), quiet = TRUE)

  captured_convo <- NULL
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_resp else { captured_convo <<- messages; final_resp }
  }, {
    cp <- tryCatch(a$chat("do it"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    expect_false(ran$called)               # not run before approval
    cp <- approve_tool_call(cp, "approve")
    resume_run(cp)
  })
  expect_match(as.character(out), "All done after approval")
  expect_true(ran$called)                  # the approved tool actually ran
  expect_equal(ran$arg, 1)                 # with the model's argument
  # and the tool result was spliced into the conversation sent on resume
  convo_txt <- paste(vapply(captured_convo, function(m)
    paste(unlist(m$content), collapse = " "), character(1)), collapse = " ")
  expect_match(convo_txt, "executed:1")
})

test_that("rejecting a tool feeds a denial and does not run it", {
  tool_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m", finish_reason = "tool",
    usage = list(sent = 10L, rec = 2L, total = 12L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r1", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = "call_1", type = "function",
      `function` = list(name = "danger", arguments = '{"x":1}')))))))), class = "llmr_response")
  ran <- new.env(); ran$called <- FALSE
  gated <- agent_tool(function(x) { ran$called <- TRUE; "executed" },
                      name = "danger", description = "d",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("A", LLMR::llm_config("groq", "fake"), tools = list(gated), quiet = TRUE)
  i <- 0L
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_resp else fake_response("ok, I won't.")
  }, {
    cp <- tryCatch(a$chat("do it"), llmragent_pending_approval = function(e) e$checkpoint)
    cp <- approve_tool_call(cp, "reject")
    resume_run(cp)
  })
  expect_false(ran$called)   # rejected -> never executed
})

test_that("human_gate(tool) sets the approval requirement", {
  t <- agent_tool(function() "x", "t", "d")
  expect_false(attr(t, "governance")$requires_approval)
  t2 <- human_gate(t, prompt = "ok?")
  expect_true(attr(t2, "governance")$requires_approval)
})

# ---- budgets bind across and inside tool loops ------------------------------

test_that("a mid-loop tool-limit abort still charges the budget", {
  # A caller standing in for LLMR's loop when the model always wants more
  # tools than the allowance: it aborts with llmr_tool_limit. The agent must
  # charge the exchange conservatively, so the ceiling binds cumulatively
  # instead of resetting every exchange.
  hungry <- function(config, messages, tools, ...) {
    stop(structure(class = c("llmr_tool_limit", "error", "condition"),
                   list(message = "would exceed max_tool_calls; stopping.")))
  }
  t <- agent_tool(function() "ok", name = "ping", description = "d")
  a <- Agent$new("H", LLMR::llm_config("groq", "fake-model"), tools = list(t),
                 budget = budget(max_tool_calls = 3), caller = hungry,
                 quiet = TRUE)
  expect_error(a$chat("go"), class = "llmragent_budget_error")
  expect_identical(a$usage()$tool_calls, 3L)   # the allowance was charged
  # the remaining allowance is 0: the next exchange is refused BEFORE calling
  expect_error(a$chat("again"), class = "llmragent_budget_error")
  expect_identical(a$usage()$tool_calls, 3L)   # and nothing more was spent
})

test_that("the pausable (gated) tool loop enforces max_tool_calls too", {
  # The agent has one gated tool (which forces the native loop) and one free
  # tool the model calls forever. The native loop must enforce the same
  # cumulative ceiling as LLMR's loop.
  free_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = "call_f", type = "function",
      `function` = list(name = "free", arguments = '{}')))))))), class = "llmr_response")
  n_free <- 0L
  free <- agent_tool(function() { n_free <<- n_free + 1L; "ok" },
                     name = "free", description = "d")
  gated <- agent_tool(function() "never", name = "danger", description = "d",
                      requires_approval = TRUE)
  a <- agent("N", LLMR::llm_config("groq", "fake"), tools = list(free, gated),
             budget = budget(max_tool_calls = 2), quiet = TRUE)
  with_stub_llmr("call_llm_robust", function(config, messages, ...) free_resp, {
    expect_error(a$chat("go"), class = "llmragent_budget_error")
  })
  expect_identical(n_free, 2L)                 # executed up to the ceiling only
  expect_identical(a$usage()$tool_calls, 2L)   # and the spend was accounted
  expect_error(a$chat("again"), class = "llmragent_budget_error")
  expect_identical(n_free, 2L)
})

test_that("gated tools refuse providers the native loop cannot speak", {
  gated <- agent_tool(function() "x", name = "danger", description = "d",
                      requires_approval = TRUE)
  a <- agent("G", LLMR::llm_config("gemini", "gemini-2.0-flash"),
             tools = list(gated), quiet = TRUE)
  expect_error(a$chat("go"), "OpenAI-compatible and Anthropic")
})

# ---- resume restores the whole suspended turn -------------------------------

test_that("resume answers every call of a multi-tool turn (OpenAI shape)", {
  # One assistant turn asks for TWO tools: the gated one first, then a free
  # one. On resume, both must receive results, or the provider would reject
  # the conversation (a dangling tool_call id).
  multi_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(
      list(id = "call_g", type = "function",
           `function` = list(name = "danger", arguments = '{"x":1}')),
      list(id = "call_f", type = "function",
           `function` = list(name = "free", arguments = '{}')))))))), class = "llmr_response")
  ran_free <- FALSE
  free <- agent_tool(function() { ran_free <<- TRUE; "free-ran" },
                     name = "free", description = "d")
  gated <- agent_tool(function(x) paste0("gated-ran:", x),
                      name = "danger", description = "d",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("M", LLMR::llm_config("groq", "fake"), tools = list(gated, free),
             quiet = TRUE)
  captured <- NULL
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) multi_resp else { captured <<- messages; fake_response("done") }
  }, {
    cp <- tryCatch(a$chat("do both"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    expect_false(ran_free)                     # nothing ran before the gate
    resume_run(approve_tool_call(cp, "approve"))
  })
  expect_match(as.character(out), "done")
  expect_true(ran_free)                        # the trailing free call ran
  ids <- vapply(captured, function(m) as.character(m$tool_call_id %||% ""), "")
  expect_true(all(c("call_g", "call_f") %in% ids))   # both calls answered
})

test_that("resume restores already-executed results (Anthropic shape)", {
  # The free call comes FIRST and executes before the pause; its tool_result
  # block lives in the checkpoint's result_blocks and must reach the resumed
  # conversation alongside the approved call's result.
  multi_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(content = list(
      list(type = "tool_use", id = "call_f", name = "free", input = list()),
      list(type = "tool_use", id = "call_g", name = "danger", input = list(x = 1))))),
    class = "llmr_response")
  free <- agent_tool(function() "free-ran", name = "free", description = "d")
  gated <- agent_tool(function(x) paste0("gated-ran:", x),
                      name = "danger", description = "d",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("An", LLMR::llm_config("anthropic", "claude-sonnet-4-5"),
             tools = list(free, gated), quiet = TRUE)
  captured <- NULL
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) multi_resp else { captured <<- messages; fake_response("done") }
  }, {
    cp <- tryCatch(a$chat("do both"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    resume_run(approve_tool_call(cp, "approve"))
  })
  expect_match(as.character(out), "done")
  # the resumed user turn carries a tool_result for BOTH call ids
  result_ids <- unlist(lapply(captured, function(m) {
    if (!is.list(m$content)) return(character(0))
    vapply(m$content, function(b)
      if (is.list(b) && identical(b$type, "tool_result"))
        as.character(b$tool_use_id) else "", "")
  }))
  expect_true(all(c("call_f", "call_g") %in% result_ids))
})

test_that("resume keeps the agent's guardrails (no silent policy bypass)", {
  tool_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = "call_1", type = "function",
      `function` = list(name = "danger", arguments = '{}')))))))), class = "llmr_response")
  g <- guardrail("no_exfil",
                 function(payload, context)
                   if (grepl("EXFIL", payload$result %||% "")) "flagged" else TRUE,
                 stage = "tool")
  gated <- agent_tool(function() "EXFIL payload", name = "danger",
                      description = "d", requires_approval = TRUE)
  a <- agent("P", LLMR::llm_config("groq", "fake"), tools = list(gated),
             guardrails = guardrails(g), quiet = TRUE)
  i <- 0L
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_resp else fake_response("done")
  }, {
    cp <- tryCatch(a$chat("do it"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    # the guardrail must survive the checkpoint round trip and block on resume
    expect_error(resume_run(approve_tool_call(cp, "approve")),
                 class = "llmragent_guardrail_block")
  })
})

test_that("resume without agent_state still drives the serialized tools", {
  tool_resp <- function(id) structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = id, type = "function",
      `function` = list(name = if (id == "call_1") "danger" else "free",
                        arguments = '{}')))))))), class = "llmr_response")
  ran_free <- FALSE
  free <- agent_tool(function() { ran_free <<- TRUE; "free-ran" },
                     name = "free", description = "d")
  gated <- agent_tool(function() "gated-ran", name = "danger",
                      description = "d", requires_approval = TRUE)
  a <- agent("S", LLMR::llm_config("groq", "fake"), tools = list(gated, free),
             quiet = TRUE)
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_resp("call_1")
    else if (i == 2L) tool_resp("call_2")     # the model asks for the free tool
    else fake_response("done")
  }, {
    cp <- tryCatch(a$chat("go"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    cp$agent_state <- NULL                    # e.g. a checkpoint built without an agent
    resume_run(approve_tool_call(cp, "approve"))
  })
  expect_match(as.character(out), "done")
  expect_true(ran_free)   # the continuation could still execute tools
})

test_that("an edited approval records the edited arguments in the history", {
  tool_resp <- structure(list(
    text = "", provider = "fake", model = "m", model_version = "m",
    finish_reason = "tool",
    usage = list(sent = 5L, rec = 1L, total = 6L, reasoning = NA_integer_, cached = NA_integer_),
    response_id = "r", duration_s = 0.01,
    raw = list(choices = list(list(message = list(content = "", tool_calls = list(list(
      id = "call_1", type = "function",
      `function` = list(name = "danger", arguments = '{"x":1}')))))))), class = "llmr_response")
  ran <- new.env(); ran$arg <- NA
  gated <- agent_tool(function(x) { ran$arg <- x; paste0("did:", x) },
                      name = "danger", description = "d",
                      parameters = list(x = list(type = "number")),
                      requires_approval = TRUE)
  a <- agent("E", LLMR::llm_config("groq", "fake"), tools = list(gated),
             quiet = TRUE)
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_resp else fake_response("done")
  }, {
    cp <- tryCatch(a$chat("do it"),
                   llmragent_pending_approval = function(e) e$checkpoint)
    resume_run(approve_tool_call(cp, "edit", edit = list(x = 42)))
  })
  expect_equal(ran$arg, 42)                        # the tool ran the edit
  ag2 <- attr(out, "agent")
  tl <- tibble::as_tibble(as_agent_run(ag2), level = "tool")
  args <- tl$arguments[tl$name == "danger"]
  expect_true(any(grepl("42", args)))              # the history records the edit
  expect_false(any(grepl("\"x\":1[^0-9]|\"x\":1$", args)))  # not the originals
})
