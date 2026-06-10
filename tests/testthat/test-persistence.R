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
  expect_error(load_agent(path), "saved LLMRAgent agent")
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
  b2 <- LLMRAgent:::Agent$new(
    name = b$name, config = b$config, persona = b$persona,
    memory = b$memory, budget = b$budget, quiet = TRUE,
    caller = scripted_caller(list("two", "three")))
  b2$restore_accounting(usage = list(calls = 1L, tokens_sent = 10L,
                                     tokens_received = 5L, tool_calls = 0L))
  b2$chat("second")
  expect_error(b2$chat("third"), class = "llmragent_budget_error")
})
