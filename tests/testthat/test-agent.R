test_that("chat is stateful and traces tokens", {
  a <- fake_agent("Ada", list("first reply", "second reply"),
                  persona = "Be brief.")
  r1 <- a$chat("hello")
  r2 <- a$chat("again")
  expect_identical(r1, "first reply")
  expect_identical(r2, "second reply")

  tr <- a$transcript()
  expect_identical(tr$role, c("user", "assistant", "user", "assistant"))
  expect_identical(tr$content[2], "first reply")

  u <- a$usage()
  expect_equal(u$calls, 2L)
  expect_equal(u$tokens_sent, 20L)
  expect_equal(u$tokens_received, 10L)

  trace <- a$trace()
  expect_equal(sum(trace$event == "call"), 2L)
})

test_that("reply is stateless", {
  a <- fake_agent("Ada", list("one-shot"))
  out <- a$reply("ping")
  expect_identical(out, "one-shot")
  expect_equal(nrow(a$transcript()), 0L)
  expect_equal(a$usage()$calls, 1L)
})

test_that("the persona is prepended unless the caller supplies a system turn", {
  seen <- NULL
  cal <- function(config, messages, tools, ...) {
    seen <<- messages
    fake_response("x")
  }
  cfg <- LLMR::llm_config("groq", "fake-model")
  a <- Agent$new("P", cfg, persona = "PERSONA TEXT", caller = cal, quiet = TRUE)
  a$reply("hi")
  expect_identical(names(seen)[1], "system")
  expect_identical(unname(seen[1]), "PERSONA TEXT")
  a$reply(c(system = "OVERRIDE", user = "hi"))
  expect_identical(unname(seen[names(seen) == "system"]), "OVERRIDE")
})

test_that("ask_structured parses the model's JSON", {
  a <- fake_agent("S", list('{"label": "positive", "score": 0.9}'))
  out <- a$ask_structured("classify this",
                          schema = list(type = "object",
                                        properties = list(label = list(type = "string"))))
  expect_identical(out$label, "positive")
  expect_equal(out$score, 0.9)
})

test_that("budgets stop the agent before the offending call", {
  a <- fake_agent("B", list("r1", "r2"), budget = budget(max_calls = 1))
  a$chat("one")
  expect_error(a$chat("two"), class = "llmragent_budget_error")
  expect_equal(a$usage()$calls, 1L)   # the second call never happened
  expect_true(any(a$trace()$event == "budget_stop"))
})

test_that("token budgets are enforced", {
  a <- fake_agent("B", list(fake_response("big", sent = 900L, rec = 200L), "next"),
                  budget = budget(max_tokens = 1000))
  a$chat("one")
  expect_error(a$chat("two"), class = "llmragent_budget_error")
})

test_that("a failing call raises and leaves memory clean", {
  cal <- function(config, messages, tools, ...) stop("server exploded")
  cfg <- LLMR::llm_config("groq", "fake-model")
  a <- Agent$new("F", cfg, caller = cal, quiet = TRUE)
  expect_error(a$chat("hi"), "server exploded")
  expect_equal(nrow(a$transcript()), 0L)   # error text never becomes a reply
})

test_that("tool history is counted and traced", {
  th <- tibble::tibble(round = 1L, name = "lookup",
                       arguments = "{}", result = "42")
  a <- fake_agent("T", list(fake_response("done", tool_history = th)))
  a$chat("use the tool")
  expect_equal(a$usage()$tool_calls, 1L)
  expect_true(any(a$trace()$event == "tool" & a$trace()$tool == "lookup"))
})

test_that("aggregate tool-loop spend is accounted, not just the final call", {
  r <- fake_response("done", sent = 5L, rec = 5L)
  attr(r, "tool_loop") <- list(model_calls = 3L, sent = 60L, rec = 15L,
                               tool_calls = 2L)
  attr(r, "tool_history") <- tibble::tibble(
    round = c(1L, 2L), name = c("a", "b"),
    arguments = c("{}", "{}"), result = c("1", "2"))
  a <- fake_agent("T", list(r))
  a$chat("go")
  u <- a$usage()
  expect_equal(u$calls, 3L)          # every internal round, not 1
  expect_equal(u$tokens_sent, 60L)   # loop aggregate, not the final call's 5
  expect_equal(u$tool_calls, 2L)
})

test_that("memory compaction spend lands on the agent's meter", {
  m <- memory_summary(threshold_chars = 50, keep_last = 2)
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    fake_response("THE-SUMMARY", sent = 7L, rec = 3L)
  }, {
    a <- fake_agent("S", list("r1", "r2", "r3"), memory = m)
    a$chat("a long opening message that easily exceeds fifty characters")
    a$chat("another long message to push us over the threshold")
    a$chat("third")   # triggers compaction first
    u <- a$usage()
    expect_equal(u$calls, 4L)   # 3 chats + 1 compaction call
    tr <- a$trace()
    expect_true(any(tr$event == "compact" & tr$tokens_sent == 7L))
  })
})

test_that("agent() validates its inputs", {
  cfg <- LLMR::llm_config("groq", "fake-model")
  expect_error(agent("", cfg))
  expect_error(agent("X", "not a config"), "llm_config")
  expect_error(agent("X", cfg, budget = list()), "budget")
})

test_that("chat(stream = TRUE) streams chunks and accounts usage", {
  sc <- function(config, messages, callback, ...) {
    for (ch in c("Hel", "lo")) callback(ch)
    fake_response("Hello", sent = 3L, rec = 2L)
  }
  cfg <- LLMR::llm_config("groq", "fake-model")
  a <- Agent$new("S", cfg, caller = scripted_caller(list("unused")),
                 stream_caller = sc, quiet = FALSE)
  printed <- capture.output(r <- a$chat("hi", stream = TRUE))
  expect_identical(r, "Hello")
  expect_match(paste(printed, collapse = ""), "Hello")
  expect_equal(a$usage()$calls, 1L)
  expect_equal(a$usage()$tokens_sent, 3L)
  expect_identical(a$transcript()$content, c("hi", "Hello"))

  # a quiet agent streams the transport but prints nothing
  q <- Agent$new("Q", cfg, caller = scripted_caller(list("unused")),
                 stream_caller = sc, quiet = TRUE)
  silent <- capture.output(rq <- q$chat("hi", stream = TRUE))
  expect_identical(rq, "Hello")
  expect_false(any(grepl("Hello", silent)))
})

test_that("streaming falls back to the tool loop when tools are present", {
  tl <- LLMR::llm_tool(function(x) "1", name = "t1", description = "d",
                       parameters = list(x = list(type = "string")))
  a <- fake_agent("T", list("plain"), tools = list(tl))
  expect_warning(r <- a$chat("hi", stream = TRUE), "Streaming")
  expect_identical(r, "plain")
})

test_that("ask_structured does not send the agent's tools", {
  seen_tools <- NULL
  cal <- function(config, messages, tools, ...) {
    seen_tools <<- tools
    fake_response('{"x": 1}')
  }
  tl <- LLMR::llm_tool(function(z) "1", name = "t1", description = "d",
                       parameters = list(z = list(type = "string")))
  cfg <- LLMR::llm_config("groq", "fake-model")
  a <- Agent$new("S", cfg, tools = list(tl), caller = cal, quiet = TRUE)
  a$ask_structured("q", schema = list(type = "object",
                                      properties = list(x = list(type = "number"))))
  expect_length(seen_tools, 0L)
  a$reply("plain question")
  expect_length(seen_tools, 1L)   # regular calls still carry the tools
})
