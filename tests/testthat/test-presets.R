test_that("debate produces phased transcript and a structured verdict", {
  pro <- fake_agent("Pro", list("pro-open", "pro-rebut", "pro-close"))
  con <- fake_agent("Con", list("con-open", "con-rebut", "con-close"))
  judge <- fake_agent("Judge",
    list('{"winner": "Pro", "confidence": 0.8, "reasoning": "stronger evidence"}'))
  d <- debate(pro, con, topic = "test motion", rounds = 1, judge = judge,
              quiet = TRUE)
  expect_identical(d$transcript$phase,
                   c("opening", "opening", "rebuttal", "rebuttal",
                     "closing", "closing"))
  expect_identical(d$transcript$speaker[1:2], c("Pro", "Con"))
  expect_identical(d$verdict$winner, "Pro")
})

test_that("focus group rotates speaking order and returns a summary", {
  mod <- fake_agent("Mod", list("the summary of themes"))
  p1 <- fake_agent("P1", list("p1-a1", "p1-a2"))
  p2 <- fake_agent("P2", list("p2-a1", "p2-a2"))
  fg <- focus_group(mod, list(p1, p2), topic = "t",
                    questions = c("q one?", "q two?"), quiet = TRUE)
  tr <- fg$transcript
  expect_equal(sum(tr$speaker == "Mod"), 2L)           # two questions
  # order rotates: q1 -> P1 then P2; q2 -> P2 then P1
  q1 <- tr$speaker[tr$question_id == 1 & tr$speaker != "Mod"]
  q2 <- tr$speaker[tr$question_id == 2 & tr$speaker != "Mod"]
  expect_identical(q1, c("P1", "P2"))
  expect_identical(q2, c("P2", "P1"))
  expect_identical(fg$summary, "the summary of themes")
})

test_that("interview returns a tidy Q/A frame; NONE suppresses the probe", {
  iv <- fake_agent("Iver", list("NONE", "What did that feel like?", "NONE"))
  resp <- fake_agent("Resp", list("answer one", "answer two", "probe answer"))
  out <- interview(iv, resp, topic = "t",
                   questions = c("First question?", "Second question?"),
                   quiet = TRUE)
  # interview() now returns a classed object carrying provenance; the Q/A frame
  # lives in $qa (and as.data.frame() returns it).
  expect_s3_class(out, "agent_interview")
  expect_identical(out$qa$type, c("scripted", "scripted", "probe"))
  expect_identical(out$qa$question[3], "What did that feel like?")
  expect_identical(out$qa$answer[3], "probe answer")
  expect_identical(as.data.frame(out)$answer[3], "probe answer")
})

test_that("deliberation collects discussion and independent votes", {
  mk_panelist <- function(name, vote) {
    fake_agent(name, list(
      paste0(name, " round 1"), paste0(name, " round 2"),
      sprintf('{"vote": "%s", "reason": "because"}', vote)))
  }
  d <- deliberate(list(mk_panelist("A", "yes"),
                       mk_panelist("B", "yes"),
                       mk_panelist("C", "no")),
                  proposal = "adopt the pilot", rounds = 2, quiet = TRUE)
  expect_equal(nrow(d$transcript), 6L)
  expect_identical(sort(d$votes$vote), c("no", "yes", "yes"))
  expect_identical(d$decision, "yes")
  expect_equal(as.integer(d$tally[["yes"]]), 2L)
})

test_that("deliberate(rounds = 0) goes straight to the vote", {
  mk <- function(name, vote) {
    fake_agent(name, list(sprintf('{"vote": "%s", "reason": "r"}', vote)))
  }
  d <- deliberate(list(mk("A", "yes"), mk("B", "yes")),
                  proposal = "p", rounds = 0, quiet = TRUE)
  expect_equal(nrow(d$transcript), 0L)     # no discussion happened
  expect_identical(sort(d$votes$vote), c("yes", "yes"))
  expect_identical(d$decision, "yes")
})

test_that("focus_group and interview stop when question drafting is unparseable", {
  # the moderator/interviewer replies with prose, not the requested JSON
  mod <- fake_agent("Mod", list("I would rather chat than emit JSON."))
  p1 <- fake_agent("P1", list("a")); p2 <- fake_agent("P2", list("b"))
  expect_error(
    focus_group(mod, list(p1, p2), topic = "t", n_questions = 2, quiet = TRUE),
    "no parseable questions")

  iv <- fake_agent("Iv", list("Still prose."))
  resp <- fake_agent("R", list("an answer"))
  expect_error(
    interview(iv, resp, topic = "t", n_questions = 2, quiet = TRUE),
    "no parseable questions")
})

test_that("a tied deliberation yields an NA decision", {
  mk <- function(name, vote) {
    fake_agent(name, list(paste0(name, " talks"),
                          sprintf('{"vote": "%s", "reason": "r"}', vote)))
  }
  d <- deliberate(list(mk("A", "yes"), mk("B", "no")),
                  proposal = "p", rounds = 1, quiet = TRUE)
  expect_true(is.na(d$decision))
})

test_that("preset returns are classed with as.data.frame methods", {
  pro <- fake_agent("Pro", list("o", "c"))
  con <- fake_agent("Con", list("o", "c"))
  d <- debate(pro, con, topic = "m", rounds = 0, quiet = TRUE)
  expect_s3_class(d, "agent_debate")
  expect_identical(d$motion, "m")
  expect_s3_class(as.data.frame(d), "data.frame")
  expect_output(print(d), "agent_debate")

  mod <- fake_agent("Mod", list("synthesis"))
  fg <- focus_group(mod, list(fake_agent("P1", list("a")),
                              fake_agent("P2", list("b"))),
                    topic = "t", questions = "q?", quiet = TRUE)
  expect_s3_class(fg, "agent_focus_group")
  expect_output(print(fg), "synthesis")

  mk <- function(name) fake_agent(name, list("talk", '{"vote":"yes","reason":"r"}'))
  dl <- deliberate(list(mk("A"), mk("B")), proposal = "p", rounds = 1,
                   quiet = TRUE)
  expect_s3_class(dl, "agent_deliberation")
  expect_identical(dl$proposal, "p")
  expect_output(print(dl), "decision")
  expect_identical(nrow(as.data.frame(dl)), nrow(dl$transcript))
})
