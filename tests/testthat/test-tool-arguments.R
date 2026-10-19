# Tool calls whose arguments did not parse, or that come from a reply cut off
# at the output-token limit, are never run: the model receives an error result,
# as with LLMR::call_llm_tools(). Offline, through agent() with a stubbed
# transport.

tool_call_reply <- function(arguments, name = "act", finish = "tool", id = "call_1") {
  resp <- fake_response("", sent = 10L, rec = 2L, finish = finish)
  resp$raw <- list(choices = list(list(message = list(content = "", tool_calls = list(
    list(id = id, type = "function",
         `function` = list(name = name, arguments = arguments)))))))
  resp
}

# A recording tool, gated or not; `ran` collects the arguments of every run.
recording_tool <- function(ran, gated = FALSE, params = TRUE) {
  fn <- if (params) function(x = "default") { ran$args <- c(ran$args, x); paste0("ran:", x) }
        else function() { ran$args <- c(ran$args, "<none>"); "ran" }
  agent_tool(fn, name = "act", description = "d",
             parameters = if (params) list(x = list(type = "string")) else NULL,
             requires_approval = gated)
}

# Run one exchange: the first reply asks for the tool, the second answers.
# Returns the agent, the reply, and the messages sent on the second request.
run_exchange <- function(first, tool, limits = budget(max_calls = 10)) {
  a <- agent("A", LLMR::llm_config("groq", "fake"), tools = list(tool),
             budget = limits, quiet = TRUE)
  sent <- NULL
  i <- 0L
  out <- with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) first else { sent <<- messages; fake_response("final answer") }
  }, a$chat("go"))
  list(agent = a, out = out, sent = sent)
}

tool_messages <- function(sent) {
  vapply(Filter(function(m) identical(m$role, "tool"), sent),
         function(m) m$content, character(1))
}

test_that("truncated and non-object arguments are refused, not run with defaults", {
  for (args in c('{"x": "a', '[1, 2]', '"just text"', "42")) {
    ran <- new.env()
    x <- run_exchange(tool_call_reply(args), recording_tool(ran))
    expect_null(ran$args)
    expect_identical(x$out, "final answer")
    msg <- tool_messages(x$sent)
    expect_length(msg, 1L)
    expect_match(msg, "not valid JSON, so the tool was not run", fixed = TRUE)
    expect_match(msg, args, fixed = TRUE)
    tl <- tibble::as_tibble(as_agent_run(x$agent), "tool")
    expect_identical(tl$status, "refused")
    expect_identical(tl$arguments, args)
    expect_identical(x$agent$usage()$tool_calls, 0L)
  }
})

test_that("a reply cut off at the token limit runs none of its calls", {
  ran <- new.env()
  x <- run_exchange(tool_call_reply('{"x": "complete"}', finish = "length"),
                    recording_tool(ran))
  expect_null(ran$args)
  expect_match(tool_messages(x$sent), "cut off by the output-token limit", fixed = TRUE)
  expect_identical(tibble::as_tibble(as_agent_run(x$agent), "tool")$status, "refused")
  expect_identical(x$agent$usage()$tool_calls, 0L)
})

test_that("valid and empty arguments still run", {
  ran <- new.env()
  x <- run_exchange(tool_call_reply('{"x": "given"}'), recording_tool(ran))
  expect_identical(ran$args, "given")
  expect_identical(tool_messages(x$sent), "ran:given")
  expect_identical(tibble::as_tibble(as_agent_run(x$agent), "tool")$status, NA_character_)
  expect_identical(x$agent$usage()$tool_calls, 1L)
  # Empty argument text means no arguments, which a tool without parameters
  # needs; LLMR::call_llm_tools() treats it the same way.
  for (args in c("", "null")) {
    ran <- new.env()
    x <- run_exchange(tool_call_reply(args), recording_tool(ran, params = FALSE))
    expect_identical(ran$args, "<none>")
    expect_identical(tool_messages(x$sent), "ran")
  }
})

test_that("refused calls do not use up the tool-call budget", {
  # With two tool calls allowed, the follow-up round after one refused and one
  # executed call is still within budget; counting the refusal would stop it.
  ran <- new.env()
  first <- tool_call_reply('{"x": "a')
  first$raw$choices[[1]]$message$tool_calls[[2]] <- list(
    id = "call_2", type = "function",
    `function` = list(name = "act", arguments = '{"x": "b"}'))
  x <- run_exchange(first, recording_tool(ran), limits = budget(max_tool_calls = 2))
  expect_identical(x$out, "final answer")
  expect_identical(ran$args, "b")
  expect_identical(x$agent$usage()$tool_calls, 1L)
  expect_identical(tibble::as_tibble(as_agent_run(x$agent), "tool")$status,
                   c("refused", NA_character_))
})

test_that("the approval gate never pauses for, or runs, unparseable arguments", {
  ran <- new.env()
  x <- run_exchange(tool_call_reply('{"x": "a'), recording_tool(ran, gated = TRUE))
  expect_null(ran$args)
  expect_identical(x$out, "final answer")
  expect_match(tool_messages(x$sent), "not valid JSON", fixed = TRUE)
  # A truncated reply asking for a gated tool does not pause either.
  ran <- new.env()
  x <- run_exchange(tool_call_reply('{"x": "a"}', finish = "length"),
                    recording_tool(ran, gated = TRUE))
  expect_null(ran$args)
  expect_identical(x$out, "final answer")
})

test_that("the reviewer sees the argument text as sent, and approval runs it", {
  ran <- new.env()
  a <- agent("A", LLMR::llm_config("groq", "fake"),
             tools = list(recording_tool(ran, gated = TRUE)), quiet = TRUE)
  i <- 0L
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_call_reply('{"x": "as sent"}') else fake_response("done")
  }, {
    cp <- tryCatch(a$chat("go"), llmragent_pending_approval = function(e) e$checkpoint)
    expect_identical(cp$pending$arguments_raw, '{"x": "as sent"}')
    expect_true(cp$pending$arguments_ok)
    expect_output(print(cp), 'arguments as sent: "\\{\\\\"x\\\\": \\\\"as sent\\\\"\\}"')
    out <- resume_run(approve_tool_call(cp, "approve"))
  })
  expect_identical(ran$args, "as sent")
  expect_identical(out$text, "done")
})

test_that("an older checkpoint holding unparseable arguments cannot run them", {
  ran <- new.env()
  a <- agent("A", LLMR::llm_config("groq", "fake"),
             tools = list(recording_tool(ran, gated = TRUE)), quiet = TRUE)
  i <- 0L
  sent <- NULL
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    i <<- i + 1L
    if (i == 1L) tool_call_reply('{"x": "fine"}') else { sent <<- messages; fake_response("done") }
  }, {
    cp <- tryCatch(a$chat("go"), llmragent_pending_approval = function(e) e$checkpoint)
    # Make it look like a checkpoint saved before LLMR flagged unparseable
    # arguments: the assistant turn holds truncated text, and the pending call
    # was read as having no arguments.
    cp$convo[[length(cp$convo)]]$tool_calls[[1]]$`function`$arguments <- '{"x": "fi'
    cp$pending$arguments <- structure(list(), names = character(0))
    cp$pending$arguments_raw <- NULL
    cp$pending$arguments_ok <- NULL
    expect_output(print(cp), "not valid JSON.*\\{\\\\\"x\\\\\": \\\\\"fi")
    expect_error(approve_tool_call(cp, "approve"), "not valid JSON.*Reject it")
    # A decision recorded by an earlier version is not honored either.
    cp$decision <- list(decision = "approve", edit = NULL)
    expect_warning(out <- resume_run(cp), "was not run")
    expect_identical(out$text, "done")
    expect_null(ran$args)
    expect_match(tool_messages(sent), "not valid JSON", fixed = TRUE)
    # The reviewer can still supply the arguments.
    cp$decision <- NULL
    i <- 1L
    out <- resume_run(approve_tool_call(cp, "edit", edit = list(x = "edited")))
    expect_identical(ran$args, "edited")
  })
})

test_that("the LLMR tool loop's refused calls are recorded but not counted", {
  ran <- new.env()
  # No budget or guardrails: the agent hands the exchange to call_llm_tools().
  x <- run_exchange(tool_call_reply('{"x": "a'), recording_tool(ran), limits = budget())
  expect_null(ran$args)
  expect_match(tool_messages(x$sent), "not valid JSON", fixed = TRUE)
  expect_identical(x$agent$usage()$tool_calls, 0L)
  expect_identical(tibble::as_tibble(as_agent_run(x$agent), "tool")$status, "refused")
})
