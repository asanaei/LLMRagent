# presets.R --------------------------------------------------------------------
# Ready-made conversation formats for common study designs. Each preset is a
# thin, transparent layer over conversation()/Agent$reply(), and each returns
# tidy objects ready for analysis.

#' Structured debate between two agents
#'
#' Alternating statements in three phases: opening, `rounds` rebuttals each,
#' and closing. An optional judge then delivers a structured verdict. The
#' phase labels make it easy to analyze argument development over time.
#'
#' @param pro,con [Agent]s arguing for and against.
#' @param topic The motion being debated.
#' @param rounds Number of rebuttal exchanges (default 2).
#' @param judge Optional [Agent]; if supplied, returns a verdict with a
#'   winner, a confidence, and reasoning.
#' @param quiet Passed through; FALSE prints utterances live.
#' @param ... Passed to the agents' underlying LLMR calls.
#' @return An object of class `agent_debate`: a list with `transcript`
#'   (tibble: `turn`, `phase`, `speaker`, `text`), `verdict` (list or NULL),
#'   and `motion`. `as.data.frame()` returns the transcript.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.7)
#' d <- debate(
#'   pro = agent("Pro", cfg, persona = "You argue FOR the motion, rigorously."),
#'   con = agent("Con", cfg, persona = "You argue AGAINST the motion, rigorously."),
#'   topic = "Social media does more harm than good to democratic discourse.",
#'   judge = agent("Judge", cfg, persona = "A strict, impartial debate judge.")
#' )
#' d$verdict
#' }
#' @export
debate <- function(pro, con, topic, rounds = 2L, judge = NULL,
                   quiet = FALSE, ...) {
  stopifnot(inherits(pro, "Agent"), inherits(con, "Agent"))
  phases <- c("opening",
              rep("rebuttal", max(0L, as.integer(rounds)) * 2L),
              "closing")
  # full speaking order: pro/con alternate within each phase
  order <- list()
  order[[1]] <- list(agent = pro, phase = "opening")
  order[[2]] <- list(agent = con, phase = "opening")
  for (r in seq_len(max(0L, as.integer(rounds)))) {
    order[[length(order) + 1L]] <- list(agent = pro, phase = "rebuttal")
    order[[length(order) + 1L]] <- list(agent = con, phase = "rebuttal")
  }
  order[[length(order) + 1L]] <- list(agent = pro, phase = "closing")
  order[[length(order) + 1L]] <- list(agent = con, phase = "closing")

  transcript <- tibble::tibble(turn = integer(0), phase = character(0),
                               speaker = character(0), text = character(0))
  for (t in seq_along(order)) {
    spk <- order[[t]]$agent
    phase <- order[[t]]$phase
    side <- if (identical(spk, pro)) "FOR" else "AGAINST"
    sys <- paste(c(
      spk$persona,
      sprintf("You are debating the motion: %s", topic),
      sprintf("You argue %s the motion. This is your %s statement.", side, phase),
      "Address the strongest opposing points; do not strawman. Be concise."),
      collapse = "\n")
    usr <- if (nrow(transcript)) {
      paste0("Debate so far:\n\n",
             .render_dialogue(transcript[, c("speaker", "text")]),
             "\n\nDeliver your ", phase, " statement.")
    } else {
      "Deliver your opening statement."
    }
    text <- spk$reply(c(system = sys, user = usr), ...)
    transcript <- rbind(transcript, tibble::tibble(
      turn = t, phase = phase, speaker = spk$name, text = text))
    if (!quiet) cli::cli_text("{.strong {spk$name}} ({phase}): {text}")
  }

  verdict <- NULL
  if (inherits(judge, "Agent")) {
    verdict <- judge$ask_structured(
      paste0("Judge this debate on the motion: ", topic,
             "\n\nFull transcript:\n\n",
             .render_dialogue(transcript[, c("speaker", "text")]),
             "\n\nDecide the winner on argument quality alone."),
      schema = list(
        type = "object",
        properties = list(
          winner = list(type = "string", enum = list(pro$name, con$name, "tie")),
          confidence = list(type = "number"),
          reasoning = list(type = "string")
        ),
        required = list("winner", "confidence", "reasoning")
      ), ...)
  }
  structure(list(transcript = transcript, verdict = verdict, motion = topic),
            class = "agent_debate")
}

#' @export
print.agent_debate <- function(x, ...) {
  cat(sprintf("<agent_debate | %d statement(s)>\nMotion: %s\n",
              nrow(x$transcript), x$motion))
  if (!is.null(x$verdict)) {
    cat(sprintf("Verdict: %s (confidence %s)\n",
                x$verdict$winner %||% "?",
                format(x$verdict$confidence %||% NA)))
  }
  cat("Transcript in $transcript; full reasoning in $verdict$reasoning.\n")
  invisible(x)
}

#' @export
as.data.frame.agent_debate <- function(x, ...) as.data.frame(x$transcript, ...)

#' A moderated focus group
#'
#' The moderator puts each question to the group; every participant answers
#' (speaking order rotates across questions so nobody anchors every round);
#' participants see the discussion so far, as in a real group. The moderator
#' closes with a synthesis of themes and disagreements.
#'
#' @param moderator An [Agent] running the group.
#' @param participants A list of [Agent]s.
#' @param topic The study topic (context for everyone).
#' @param questions Character vector of questions. If NULL, the moderator
#'   drafts `n_questions` itself, which is useful for piloting.
#' @param n_questions Number of questions to draft when `questions` is NULL.
#' @param quiet FALSE prints the session live.
#' @param ... Passed to the underlying LLMR calls.
#' @return An object of class `agent_focus_group`: a list with `transcript`
#'   (tibble: `turn`, `question_id`, `speaker`, `text`), `questions`,
#'   `summary` (the moderator's synthesis), and `topic`. `as.data.frame()`
#'   returns the transcript.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.9)
#' fg <- focus_group(
#'   moderator = agent("Moderator", cfg, persona = "A neutral focus-group moderator."),
#'   participants = list(
#'     agent("Maya", cfg, persona = "A 34-year-old nurse, prudent with money."),
#'     agent("Tom",  cfg, persona = "A 22-year-old gig worker, risk-tolerant."),
#'     agent("Ines", cfg, persona = "A 58-year-old teacher nearing retirement.")
#'   ),
#'   topic = "Attitudes toward a 4-day work week",
#'   questions = c("What would a 4-day week change in your daily life?",
#'                 "What worries you about it?")
#' )
#' fg$summary
#' }
#' @export
focus_group <- function(moderator, participants, topic,
                        questions = NULL, n_questions = 3L,
                        quiet = FALSE, ...) {
  stopifnot(inherits(moderator, "Agent"), is.list(participants),
            length(participants) >= 2L)
  for (p in participants) {
    if (!inherits(p, "Agent")) stop("`participants` must be Agent objects.", call. = FALSE)
  }
  nms <- vapply(participants, function(a) a$name, character(1))

  if (is.null(questions)) {
    drafted <- moderator$ask_structured(
      paste0("Draft ", n_questions, " open-ended focus-group questions on: ",
             topic, ". Questions must be neutral and non-leading."),
      schema = list(type = "object",
                    properties = list(questions = list(
                      type = "array", items = list(type = "string"))),
                    required = list("questions")), ...)
    questions <- vapply(drafted$questions, as.character, character(1))
  }

  transcript <- tibble::tibble(turn = integer(0), question_id = integer(0),
                               speaker = character(0), text = character(0))
  t <- 0L
  for (q in seq_along(questions)) {
    t <- t + 1L
    transcript <- rbind(transcript, tibble::tibble(
      turn = t, question_id = q, speaker = moderator$name,
      text = questions[[q]]))
    if (!quiet) cli::cli_text("{.strong {moderator$name}}: {questions[[q]]}")

    # rotate speaking order across questions
    order <- ((seq_along(participants) + q - 2L) %% length(participants)) + 1L
    for (i in order) {
      spk <- participants[[i]]
      sys <- paste(c(
        spk$persona,
        paste0("You are ", spk$name, ", a participant in a focus group on: ", topic, "."),
        "Answer the moderator's question honestly, in character, in a few sentences.",
        "React to other participants when you genuinely agree or disagree."),
        collapse = "\n")
      usr <- paste0("Discussion so far:\n\n",
                    .render_dialogue(transcript[, c("speaker", "text")]),
                    "\n\nThe moderator's current question is: ", questions[[q]],
                    "\nYour answer, ", spk$name, ":")
      text <- spk$reply(c(system = sys, user = usr), ...)
      t <- t + 1L
      transcript <- rbind(transcript, tibble::tibble(
        turn = t, question_id = q, speaker = spk$name, text = text))
      if (!quiet) cli::cli_text("{.strong {spk$name}}: {text}")
    }
  }

  summary <- moderator$reply(c(
    system = paste(moderator$persona %||% "",
                   "Synthesize the discussion: main themes, points of agreement,",
                   "points of disagreement, and notable individual differences.",
                   sep = "\n"),
    user = paste0("Focus group on: ", topic, "\n\nFull transcript:\n\n",
                  .render_dialogue(transcript[, c("speaker", "text")]))), ...)
  if (!quiet) cli::cli_text("{.strong {moderator$name} (summary)}: {summary}")

  structure(list(transcript = transcript, questions = questions,
                 summary = summary, topic = topic),
            class = "agent_focus_group")
}

#' @export
print.agent_focus_group <- function(x, ...) {
  cat(sprintf("<agent_focus_group | %d question(s), %d utterance(s)>\nTopic: %s\n\n",
              length(x$questions), nrow(x$transcript), x$topic))
  cat("Moderator's synthesis:\n", x$summary, "\n", sep = "")
  invisible(x)
}

#' @export
as.data.frame.agent_focus_group <- function(x, ...) {
  as.data.frame(x$transcript, ...)
}

#' A semi-structured interview
#'
#' The interviewer works through a question list (or drafts one), asking one
#' question at a time with an optional adaptive follow-up probing the
#' respondent's previous answer. Returns a tidy question/answer frame, the
#' format interview studies analyze.
#'
#' @param interviewer,respondent [Agent]s.
#' @param topic Interview topic.
#' @param questions Character vector; if NULL the interviewer drafts
#'   `n_questions`.
#' @param n_questions Number of questions to draft when `questions` is NULL.
#' @param follow_up If TRUE (default), each scripted question may be followed
#'   by one adaptive probe based on the answer.
#' @param quiet FALSE prints the exchange live.
#' @param ... Passed to the underlying LLMR calls.
#' @return A tibble: `order`, `type` ("scripted" or "probe"), `question`,
#'   `answer`.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
#' iv <- interview(
#'   interviewer = agent("Interviewer", cfg,
#'                       persona = "A careful qualitative researcher."),
#'   respondent  = agent("Respondent", cfg,
#'                       persona = "A first-generation college student, reflective."),
#'   topic = "Experiences with online learning",
#'   n_questions = 3
#' )
#' iv
#' }
#' @export
interview <- function(interviewer, respondent, topic,
                      questions = NULL, n_questions = 5L,
                      follow_up = TRUE, quiet = FALSE, ...) {
  stopifnot(inherits(interviewer, "Agent"), inherits(respondent, "Agent"))

  if (is.null(questions)) {
    drafted <- interviewer$ask_structured(
      paste0("Draft ", n_questions, " open-ended interview questions on: ",
             topic, ". Order them from general to specific."),
      schema = list(type = "object",
                    properties = list(questions = list(
                      type = "array", items = list(type = "string"))),
                    required = list("questions")), ...)
    questions <- vapply(drafted$questions, as.character, character(1))
  }

  rows <- list()
  history <- character(0)
  ask_one <- function(question, type, ord) {
    sys <- paste(c(
      respondent$persona,
      paste0("You are being interviewed about: ", topic, "."),
      "Answer in character, concretely, with examples from your life."),
      collapse = "\n")
    usr <- paste0(
      if (length(history)) paste0("Interview so far:\n\n",
                                  paste(history, collapse = "\n\n"), "\n\n") else "",
      "Interviewer: ", question)
    ans <- respondent$reply(c(system = sys, user = usr), ...)
    history <<- c(history,
                  paste0("Interviewer: ", question),
                  paste0(respondent$name, ": ", ans))
    if (!quiet) {
      cli::cli_text("{.strong Q{ord}}: {question}")
      cli::cli_text("{.strong {respondent$name}}: {ans}")
    }
    rows[[length(rows) + 1L]] <<- tibble::tibble(
      order = ord, type = type, question = question, answer = ans)
    ans
  }

  ord <- 0L
  for (q in questions) {
    ord <- ord + 1L
    ans <- ask_one(q, "scripted", ord)
    if (isTRUE(follow_up)) {
      probe <- interviewer$reply(c(
        system = paste(interviewer$persona %||% "",
                       "You ask exactly one short follow-up question that probes",
                       "the most analytically interesting part of the answer.",
                       "If no follow-up is warranted, reply with exactly: NONE",
                       sep = "\n"),
        user = paste0("Question asked: ", q, "\n\nAnswer received: ", ans)), ...)
      probe <- trimws(probe)
      if (!identical(toupper(probe), "NONE") && nzchar(probe)) {
        ord <- ord + 1L
        ask_one(probe, "probe", ord)
      }
    }
  }
  do.call(rbind, rows)
}

#' Group deliberation with a recorded vote
#'
#' Agents discuss a proposal for a fixed number of rounds (everyone speaks
#' each round, seeing the discussion so far), then vote independently and
#' privately through structured output. Comparing votes cast after
#' deliberation with positions voiced during it is exactly the kind of
#' analysis the tidy return enables.
#'
#' @param agents A list of [Agent]s.
#' @param proposal The proposal under deliberation (character scalar).
#' @param rounds Discussion rounds before the vote (default 2).
#' @param options Vote options. Default `c("yes", "no", "abstain")`.
#' @param quiet FALSE prints the deliberation live.
#' @param ... Passed to the underlying LLMR calls.
#' @return An object of class `agent_deliberation`: a list with `transcript`
#'   (tibble: `turn`, `round`, `speaker`, `text`), `votes` (tibble: `voter`,
#'   `vote`, `reason`), `tally` (table), `decision` (modal vote, NA on ties),
#'   and `proposal`. `as.data.frame()` returns the transcript.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.9)
#' panel <- list(
#'   agent("Aila", cfg, persona = "Data-driven, cautious about side effects."),
#'   agent("Bo",   cfg, persona = "Mission-driven, impatient with delay."),
#'   agent("Cyn",  cfg, persona = "A budget hawk.")
#' )
#' d <- deliberate(panel, "Adopt a four-day work week for one pilot year.")
#' d$tally; d$decision
#' }
#' @export
deliberate <- function(agents, proposal, rounds = 2L,
                       options = c("yes", "no", "abstain"),
                       quiet = FALSE, ...) {
  stopifnot(is.list(agents), length(agents) >= 2L)
  for (a in agents) {
    if (!inherits(a, "Agent")) stop("`agents` must be Agent objects.", call. = FALSE)
  }
  nms <- vapply(agents, function(a) a$name, character(1))

  transcript <- tibble::tibble(turn = integer(0), round = integer(0),
                               speaker = character(0), text = character(0))
  t <- 0L
  for (r in seq_len(max(1L, as.integer(rounds)))) {
    for (i in seq_along(agents)) {
      spk <- agents[[i]]
      sys <- paste(c(
        spk$persona,
        paste0("You are ", spk$name, ", deliberating with colleagues on a proposal."),
        "State your current position and engage the strongest argument you disagree with.",
        "A few sentences only."), collapse = "\n")
      usr <- paste0(
        "Proposal: ", proposal,
        if (nrow(transcript)) paste0("\n\nDeliberation so far:\n\n",
                                     .render_dialogue(transcript[, c("speaker", "text")])) else "",
        "\n\nRound ", r, ". Your contribution, ", spk$name, ":")
      text <- spk$reply(c(system = sys, user = usr), ...)
      t <- t + 1L
      transcript <- rbind(transcript, tibble::tibble(
        turn = t, round = r, speaker = spk$name, text = text))
      if (!quiet) cli::cli_text("{.strong {spk$name}} (round {r}): {text}")
    }
  }

  vote_schema <- list(
    type = "object",
    properties = list(
      vote = list(type = "string", enum = as.list(options)),
      reason = list(type = "string")
    ),
    required = list("vote", "reason")
  )
  votes <- lapply(agents, function(a) {
    v <- a$ask_structured(
      paste0("The deliberation has ended. Proposal: ", proposal,
             "\n\nFull discussion:\n\n",
             .render_dialogue(transcript[, c("speaker", "text")]),
             "\n\nCast your vote now (", paste(options, collapse = "/"),
             ") with a one-sentence reason. Vote your honest position, ",
             "which may differ from what you said publicly."),
      schema = vote_schema, ...)
    tibble::tibble(voter = a$name,
                   vote = as.character(v$vote %||% NA_character_),
                   reason = as.character(v$reason %||% NA_character_))
  })
  votes <- do.call(rbind, votes)
  tally <- table(factor(votes$vote, levels = options))
  top <- names(tally)[tally == max(tally)]
  decision <- if (length(top) == 1L) top else NA_character_
  if (!quiet) {
    cli::cli_text("{.strong Vote}: {paste(sprintf('%s=%d', names(tally), as.integer(tally)), collapse = ', ')}")
  }
  structure(list(transcript = transcript, votes = votes, tally = tally,
                 decision = decision, proposal = proposal),
            class = "agent_deliberation")
}

#' @export
print.agent_deliberation <- function(x, ...) {
  cat(sprintf("<agent_deliberation | %d utterance(s), %d voter(s)>\nProposal: %s\n",
              nrow(x$transcript), nrow(x$votes), x$proposal))
  cat("Tally:",
      paste(sprintf("%s=%d", names(x$tally), as.integer(x$tally)), collapse = ", "),
      "| decision:", if (is.na(x$decision)) "tie" else x$decision, "\n")
  cat("Transcript in $transcript; votes with reasons in $votes.\n")
  invisible(x)
}

#' @export
as.data.frame.agent_deliberation <- function(x, ...) {
  as.data.frame(x$transcript, ...)
}
