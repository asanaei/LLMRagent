# A manifest hash identifies a study's specification across package versions.
# The expected values below were computed by LLMRagent 0.8.1 for the same
# runs, with the same LLMR and the package versions fixed as shown.

fixed_versions <- function(r) {
  r$pkg_versions <- list(LLMRagent = "0.8.1", LLMR = "0.8.11")
  r
}

test_that("an experiment's manifest hash equals the one 0.8.1 computed", {
  design <- data.frame(framing = c("gains", "losses"), stringsAsFactors = FALSE)
  ex <- agent_experiment(design, reps = 2L, quiet = TRUE, run_fn = function(cond, rep) {
    if (identical(cond$framing, "losses") && rep == 2L) stop("cell failed")
    a <- fake_agent("Subject", list(paste(cond$framing, rep)), persona = "A voter.")
    a$chat(paste("Decide under", cond$framing, "framing."))
    a
  })
  expect_warning(r <- as_agent_run(ex), "contributed no events")
  m <- agent_manifest(fixed_versions(r))
  expect_identical(m$manifest_hash,
                   "39e82e654f453ef899848f22d8b750a05410399f619759bfddb5f9a318a7444c")
  # The count of converted cells is reported but does not enter the hash.
  expect_identical(m$design$n_converted, 3L)
  expect_null(m$workflow$design$n_converted)
  r$design$n_converted <- 0L
  expect_identical(agent_manifest(fixed_versions(r))$manifest_hash, m$manifest_hash)
  # A change to the specification still changes the hash.
  r$design$n_cells <- 5L
  expect_false(identical(agent_manifest(fixed_versions(r))$manifest_hash, m$manifest_hash))
})

test_that("agent and conversation manifest hashes equal the ones 0.8.1 computed", {
  a <- fake_agent("Ada", list("first", "second"), persona = "Terse.")
  a$chat("q1")
  a$chat("q2")
  expect_identical(agent_manifest(fixed_versions(as_agent_run(a)))$manifest_hash,
                   "87e3fc796e3593f4c6e9ba7fcdebeb1dade5eb61258999757509a7c4c8ecf999")
  b <- fake_agent("Bo", list("b1", "b2"), persona = "Skeptic.")
  c1 <- fake_agent("Cy", list("c1", "c2"), persona = "Planner.")
  conv <- conversation(list(b, c1), topic = "Pedestrian center?", max_turns = 4,
                       quiet = TRUE)
  expect_identical(agent_manifest(fixed_versions(as_agent_run(conv)))$manifest_hash,
                   "649863e04775e34688809010d2ae47c20083b486dad3c5b2c300830aa2c224a8")
})
