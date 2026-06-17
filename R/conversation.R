# conversation.R ---------------------------------------------------------------
# Multi-agent conversations over one shared, speaker-attributed transcript.
# Every participant sees the same rendered dialogue each turn (no
# last-message-only relay), and the transcript is the unit of analysis: a
# tidy tibble with one row per utterance.

#' Run a multi-agent conversation
#'
#' Agents talk over a shared transcript. At each turn the next speaker (chosen
#' by `turn_policy`) receives the full dialogue so far, attributed by name,
#' plus an instruction to answer in character; the reply is appended and the
#' next turn begins. The conversation ends after `max_turns` utterances or
#' when `stop_when(transcript)` returns `TRUE`.
#'
#' Turn policies:
#' - `"round_robin"`: agents speak in the order given, repeatedly.
#' - `"random"`: a random speaker each turn, never the same agent twice in a
#'   row. Set a seed first (e.g. `set.seed(110)`) for a reproducible order.
#' - `"moderator"`: after each utterance the `moderator` agent chooses who
#'   speaks next (a structured one-token decision), which yields organic
#'   dynamics at the cost of one extra cheap call per turn.
#'
#' Replies are stateless ([Agent]'s `reply()`): the shared transcript is the
#' single source of truth, so the same agents can be reused across
#' conversations without cross-contamination.
#'
#' @param agents A list of [Agent] objects (names must be unique).
#' @param topic What the conversation is about; included in every speaker's
#'   instructions.
#' @param opening Optional opening statement placed on the transcript before
#'   the first turn (attributed to `opening_by`).
#' @param opening_by Name to attribute the opening to. Default "Facilitator".
#' @param turn_policy One of `"round_robin"`, `"random"`, `"moderator"`.
#' @param moderator An [Agent]; required for the moderator policy.
#' @param max_turns Total number of utterances to collect.
#' @param stop_when Optional `function(transcript_tibble) -> logical`; checked
#'   after every utterance.
#' @param instruction Extra instruction appended to every speaker's system
#'   message (e.g. "Answer in at most three sentences.").
#' @param quiet If FALSE (default), utterances print as they arrive.
#' @param ... Passed to each agent's underlying LLMR call.
#' @return An object of class `agent_conversation`: a list with `transcript`
#'   (tibble: `turn`, `speaker`, `text`), `topic`, and `agents` (names).
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
#' a <- agent("Rosa",  cfg, persona = "A pragmatic city planner.")
#' b <- agent("Hugo",  cfg, persona = "A skeptical economist.")
#' conv <- conversation(list(a, b),
#'                      topic = "Should the city pedestrianize its center?",
#'                      max_turns = 6)
#' conv$transcript
#' }
#' @seealso [debate()], [focus_group()], [interview()], [deliberate()]
#' @export
conversation <- function(agents, topic,
                         opening = NULL, opening_by = "Facilitator",
                         turn_policy = c("round_robin", "random", "moderator"),
                         moderator = NULL,
                         max_turns = 2L * length(agents),
                         stop_when = NULL,
                         instruction = NULL,
                         quiet = FALSE, ...) {
  turn_policy <- match.arg(turn_policy)
  stopifnot(is.list(agents), length(agents) >= 2L)
  for (a in agents) {
    if (!inherits(a, "Agent")) stop("`agents` must be a list of Agent objects.", call. = FALSE)
  }
  nms <- vapply(agents, function(a) a$name, character(1))
  if (anyDuplicated(nms)) stop("Agent names must be unique.", call. = FALSE)
  names(agents) <- nms
  if (identical(turn_policy, "moderator") && !inherits(moderator, "Agent")) {
    stop("The moderator policy needs `moderator = agent(...)`.", call. = FALSE)
  }

  transcript <- tibble::tibble(turn = integer(0), speaker = character(0),
                               text = character(0))
  if (!is.null(opening)) {
    transcript <- rbind(transcript, tibble::tibble(
      turn = 0L, speaker = opening_by, text = as.character(opening)))
    if (!quiet) cli::cli_text("{.strong {opening_by}}: {opening}")
  }

  last_speaker <- NA_character_
  rr_index <- 0L

  pick_next <- function() {
    if (turn_policy == "round_robin") {
      rr_index <<- rr_index %% length(agents) + 1L
      return(nms[rr_index])
    }
    if (turn_policy == "random") {
      pool <- setdiff(nms, last_speaker)
      return(sample(pool, 1L))
    }
    # moderator policy: a structured one-field choice
    choice <- moderator$ask_structured(
      paste0("You moderate a conversation on: ", topic,
             "\n\nDialogue so far:\n", .render_dialogue(transcript),
             "\n\nWho should speak next? Choose exactly one of: ",
             paste(nms, collapse = ", "),
             if (!is.na(last_speaker)) paste0(". Do not pick ", last_speaker, ".") else "."),
      schema = list(type = "object",
                    properties = list(next_speaker = list(type = "string",
                                                          enum = as.list(nms))),
                    required = list("next_speaker")),
      ...)
    cand <- choice$next_speaker %||% ""
    if (!cand %in% nms || identical(cand, last_speaker)) {
      cand <- sample(setdiff(nms, last_speaker), 1L)
    }
    cand
  }

  for (t in seq_len(max_turns)) {
    who <- pick_next()
    spk <- agents[[who]]
    sys <- paste(c(
      spk$persona,
      paste0("You are taking part in a conversation on: ", topic, "."),
      paste0("You are ", who, ". Reply in character, in your own voice."),
      "Do not write lines for other participants and do not prefix your reply with your name.",
      instruction
    ), collapse = "\n")
    turn_cue <- if (nrow(transcript)) {
      paste0("It is your turn, ", who, ".")
    } else {
      paste0("You speak first, ", who, ". Open the conversation.")
    }
    text <- spk$reply(.dialogue_messages(transcript, who, sys, turn_cue), ...)
    transcript <- rbind(transcript, tibble::tibble(
      turn = t, speaker = who, text = text))
    if (!quiet) cli::cli_text("{.strong {who}}: {text}")
    last_speaker <- who
    if (!is.null(stop_when) && isTRUE(stop_when(transcript))) break
  }

  structure(list(transcript = transcript, topic = topic, agents = nms),
            class = "agent_conversation")
}

#' @export
as.data.frame.agent_conversation <- function(x, ...) {
  as.data.frame(x$transcript, ...)
}

#' @export
print.agent_conversation <- function(x, ...) {
  cat(sprintf("<agent_conversation | %d turn(s) | %s>\n",
              max(c(0L, x$transcript$turn)), paste(x$agents, collapse = ", ")))
  cat("Topic:", x$topic, "\n\n")
  for (i in seq_len(nrow(x$transcript))) {
    cat(sprintf("[%d] %s: %s\n", x$transcript$turn[i],
                x$transcript$speaker[i], x$transcript$text[i]))
  }
  invisible(x)
}
