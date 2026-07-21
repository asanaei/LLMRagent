test_that("agents round-trip through save_agent/load_agent and stay functional", {
  a <- fake_agent("Keeper", list("remembered reply"),
                  persona = "You keep records.")
  a$chat("note this down")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  save_agent(a, path)

  b <- load_agent(path)
  expect_identical(b$name, "Keeper")
  expect_identical(b$persona, "You keep records.")
  expect_identical(b$config$provider, a$config$provider)
  # the key survives as an env reference, so the agent can actually call
  expect_true(inherits(b$config$api_key, "llmr_secret_env"))
  # memory contents survive
  tr <- b$transcript()
  expect_identical(tr$content, c("note this down", "remembered reply"))
})

test_that("load_agent rejects foreign files", {
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  saveRDS(list(unrelated = TRUE), path)
  expect_error(load_agent(path), "saved LLMRagent agent")
})

test_that("guardrails survive save/load (no silent policy bypass)", {
  check <- function(payload, context) {
    if (grepl("secret", payload)) "mentions secret" else TRUE
  }
  environment(check) <- baseenv()
  g <- guardrail("no_secret", check, stage = "input")
  a <- fake_agent("Guarded", list("ok"), guardrails = guardrails(g))
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  save_agent(a, path)

  b <- load_agent(path)
  expect_s3_class(b$guardrail_set(), "agent_guardrails")
  # the restored policy still blocks (rebuild with a fake caller to exercise it)
  b2 <- LLMRagent:::Agent$new(
    name = b$name, config = b$config, memory = b$memory,
    guardrails = b$guardrail_set(), quiet = TRUE,
    caller = scripted_caller(list("ok")))
  expect_error(b2$chat("tell me a secret"), class = "llmragent_guardrail_block")
})

test_that("save_agent warns when the config carries a literal API key", {
  cfg <- suppressWarnings(
    LLMR::llm_config("groq", "fake-model", api_key = "sk-LITERAL"))
  a <- Agent$new("Leaky", cfg, caller = scripted_caller(list("ok")), quiet = TRUE)
  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  expect_warning(save_agent(a, path), "literal API key")
  # the usual env-reference config saves silently
  b <- fake_agent("Clean", list("ok"))
  path2 <- tempfile(fileext = ".rds")
  on.exit(unlink(path2), add = TRUE)
  expect_no_warning(save_agent(b, path2))
})

test_that("budgets keep binding across save/load: counters carry over", {
  a <- fake_agent("Spender", list("one", "two"),
                  budget = budget(max_calls = 2))
  a$chat("first")

  path <- tempfile(fileext = ".rds")
  on.exit(unlink(path))
  save_agent(a, path)

  b <- load_agent(path)
  expect_equal(b$usage()$calls, 1L)              # the past is remembered
  expect_true(nrow(b$trace()) > 0L)              # trace came along
  # one more call is allowed, the next must hit the (restored) ceiling
  b2 <- LLMRagent:::Agent$new(
    name = b$name, config = b$config, persona = b$persona,
    memory = b$memory, budget = b$budget, quiet = TRUE,
    caller = scripted_caller(list("two", "three")))
  b2$restore_accounting(usage = list(calls = 1L, tokens_sent = 10L,
                                     tokens_received = 5L, tool_calls = 0L))
  b2$chat("second")
  expect_error(b2$chat("third"), class = "llmragent_budget_error")
})
