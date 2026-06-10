test_that("agent_as_tool exposes a working specialist", {
  spec <- fake_agent("Spec", list("the answer is 42"),
                     persona = "A numerologist. Loves numbers above all.")
  tool <- agent_as_tool(spec)
  expect_s3_class(tool, "llmr_tool")
  expect_identical(tool$name, "ask_spec")
  expect_match(tool$description, "A numerologist\\.")

  out <- tool$fn(question = "what is the answer?")
  expect_identical(out, "the answer is 42")
  expect_equal(spec$usage()$calls, 1L)       # spend lands on the specialist
  expect_equal(nrow(spec$transcript()), 0L)  # consultation is stateless
})

test_that("agent_as_tool honors custom name and description", {
  spec <- fake_agent("Dr. Wu", list("ok"))
  tool <- agent_as_tool(spec, name = "consult_wu", description = "Ask Wu.")
  expect_identical(tool$name, "consult_wu")
  expect_identical(tool$description, "Ask Wu.")
  # the default name is sanitized to a valid tool identifier
  expect_identical(agent_as_tool(spec)$name, "ask_dr_wu")
})

test_that("a delegated budget stop raises the typed error", {
  spec <- fake_agent("Spec", list("ok"), budget = budget(max_calls = 0))
  tool <- agent_as_tool(spec)
  expect_error(tool$fn(question = "hi"), class = "llmragent_budget_error")
})

test_that("agent_as_tool rejects non-agents", {
  expect_error(agent_as_tool("not an agent"))
})
