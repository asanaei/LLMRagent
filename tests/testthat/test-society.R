# society.R: the thin social-simulation scaffolding. All offline via
# fake_agent, so no API key is touched. The point of these tests is structure
# and discipline: populations build, networks resolve into exposure, a round
# appends utterances, measures stay tidy and uncalibrated, a reused agent is
# flagged, and the report always prints the model-conditioned caveat.

test_that("agent_population from a list of Agents keeps n and ids", {
  pop <- agent_population(list(
    fake_agent("p1", list("a")),
    fake_agent("p2", list("b")),
    fake_agent("p3", list("c"))))
  expect_s3_class(pop, "agent_population")
  expect_equal(pop$n, 3L)
  expect_length(pop$ids, 3L)
  expect_true(all(nzchar(pop$ids)))
})

test_that("society with a fixed edge list builds the right exposure matrix", {
  pop <- agent_population(list(
    fake_agent("p1", list("a")),
    fake_agent("p2", list("b")),
    fake_agent("p3", list("c"))))
  edges <- data.frame(from = c("p1", "p2"), to = c("p2", "p3"),
                      stringsAsFactors = FALSE)
  soc <- society(pop, network = edges)
  expect_s3_class(soc, "society")
  expect_equal(nrow(soc$edges), 2L)

  ex <- exposure_matrix(soc)
  expect_equal(dim(ex), c(3L, 3L))
  expect_equal(rownames(ex), pop$ids)
  # p1-p2 and p2-p3 connected (symmetric); p1-p3 not; diagonal zero.
  expect_equal(ex[1, 2], 1); expect_equal(ex[2, 1], 1)
  expect_equal(ex[2, 3], 1); expect_equal(ex[3, 2], 1)
  expect_equal(ex[1, 3], 0); expect_equal(ex[3, 1], 0)
  expect_equal(sum(diag(ex)), 0)
})

test_that("default (NULL) network is fully connected", {
  pop <- agent_population(list(
    fake_agent("p1", list("a")),
    fake_agent("p2", list("b")),
    fake_agent("p3", list("c"))))
  soc <- society(pop)            # NULL network
  expect_equal(nrow(soc$edges), 3L)   # all unordered pairs of 3 agents
  ex <- exposure_matrix(soc)
  expect_equal(sum(ex), 6)            # 3 edges, symmetric
})

test_that("step_interaction advances the step and appends utterances", {
  pop <- agent_population(list(
    fake_agent("p1", list("p1-r1", "p1-r2", "p1-r3")),
    fake_agent("p2", list("p2-r1", "p2-r2", "p2-r3")),
    fake_agent("p3", list("p3-r1", "p3-r2", "p3-r3"))))
  soc <- society(pop)            # fully connected -> all speak
  expect_equal(soc$step, 0L)
  expect_equal(nrow(soc$history), 0L)

  soc <- step_interaction(soc, prompt = "Say something.")
  expect_equal(soc$step, 1L)
  expect_equal(nrow(soc$history), 3L)
  expect_setequal(soc$history$speaker, c("p1", "p2", "p3"))
  expect_true(all(soc$history$step == 1L))

  soc <- step_interaction(soc)
  expect_equal(soc$step, 2L)
  expect_equal(nrow(soc$history), 6L)   # history grows
})

test_that("only `who` agents speak when `who` is given", {
  pop <- agent_population(list(
    fake_agent("p1", list("p1-r1", "p1-r2")),
    fake_agent("p2", list("p2-r1", "p2-r2")),
    fake_agent("p3", list("p3-r1", "p3-r2"))))
  soc <- society(pop)
  soc <- step_interaction(soc, who = c("p1", "p3"))
  expect_equal(nrow(soc$history), 2L)
  expect_setequal(soc$history$speaker, c("p1", "p3"))
  expect_false("p2" %in% soc$history$speaker)
})

test_that("collect_measures returns a tidy uncalibrated frame", {
  pop <- agent_population(list(
    fake_agent("p1", list("x", "y")),
    fake_agent("p2", list("x", "y"))))
  soc <- society(pop)
  soc <- step_interaction(soc)

  m <- collect_measures(soc)        # default: n_utterances
  expect_s3_class(m, "tbl_df")
  expect_true(all(c("agent_id", "name", "measure", "value", "step") %in% names(m)))
  expect_true(isTRUE(attr(m, "uncalibrated")))
  expect_equal(unique(m$measure), "n_utterances")
  expect_true(all(m$value == 1))    # one utterance each after one round

  # A custom measure function is applied per agent.
  m2 <- collect_measures(soc, measures = list(
    name_len = function(a) nchar(a$name)))
  expect_equal(unique(m2$measure), "name_len")
  expect_true(all(m2$value == 2))   # "p1", "p2" each length 2
  expect_true(isTRUE(attr(m2, "uncalibrated")))
})

test_that("a measure that errors yields NA, not a crash", {
  pop <- agent_population(list(fake_agent("p1", list("x"))))
  soc <- society(pop)
  m <- collect_measures(soc, measures = list(
    boom = function(a) stop("nope")))
  expect_true(is.na(m$value[1]))
})

test_that("contamination_report flags a reused agent instance", {
  shared <- fake_agent("p1", list("x"))
  # The SAME object placed in two slots: duplicate agent_id is the tell.
  pop <- agent_population(list(shared, shared, fake_agent("p2", list("y"))))
  soc <- society(pop)
  rep <- contamination_report(soc)
  expect_s3_class(rep, "society_contamination")
  expect_false(isTRUE(attr(rep, "clean")))
  expect_equal(nrow(rep), 1L)
  expect_equal(rep$agent_id, shared$id())
  expect_equal(rep$n, 2L)
})

test_that("contamination_report is clean for distinct agents", {
  pop <- agent_population(list(
    fake_agent("p1", list("x")),
    fake_agent("p2", list("y"))))
  rep <- contamination_report(society(pop))
  expect_true(isTRUE(attr(rep, "clean")))
  expect_equal(nrow(rep), 0L)
})

test_that("report.society always prints the model-conditioned caveat", {
  pop <- agent_population(list(
    fake_agent("p1", list("x")),
    fake_agent("p2", list("y"))))
  soc <- society(pop)
  rep <- report(soc)
  expect_s3_class(rep, "agent_report")
  txt <- paste(unclass(rep), collapse = " ")
  expect_match(txt, "model-conditioned")
  expect_match(txt, "not population facts")
  expect_match(txt, "UNCALIBRATED")
})

test_that("agent_population replicates a single brief into n copies", {
  pop <- agent_population("A cautious voter.", n = 3,
                          config = LLMR::llm_config("groq", "fake-model"))
  expect_equal(pop$n, 3L)
  nms <- vapply(pop$agents, function(a) a$name, character(1))
  expect_equal(nms, c("p1", "p2", "p3"))
})

test_that("integer-indexed edges resolve to agent names", {
  pop <- agent_population(list(
    fake_agent("p1", list("a")),
    fake_agent("p2", list("b")),
    fake_agent("p3", list("c"))))
  soc <- society(pop, network = matrix(c(1, 2, 2, 3), ncol = 2, byrow = TRUE))
  expect_equal(nrow(soc$edges), 2L)
  expect_setequal(soc$edges$from, c("p1", "p2"))
})

test_that("print methods are stable", {
  pop <- agent_population(list(fake_agent("p1", list("a")),
                              fake_agent("p2", list("b"))))
  soc <- society(pop)
  expect_output(print(pop), "agent_population")
  expect_output(print(soc), "society \\| 2 agent")
})
