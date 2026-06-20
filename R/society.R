# society.R -------------------------------------------------------------------
# A thin social-simulation scaffolding layer over the existing agent and
# conversation machinery. It does not introduce a new simulation engine: a
# society is a population of agents, an edge list saying who may see whom, and
# a shared transcript that grows one interaction round at a time. Each round
# drives the Agent's stateless reply() over that shared transcript (the same
# primitive conversation() uses), so the transcript stays the single source of
# truth and agents can be reused without cross-contamination.
#
# The discipline this file enacts is anti-overclaiming. A population of
# language models talking is not a population of people. Every surface that
# returns a result (collect_measures, report) carries an "uncalibrated" mark,
# and report() always prints the line that these are model-conditioned
# simulations, not population facts unless calibrated against human data. The
# point is to make the caveat structural rather than optional.

#' Build a population of agents
#'
#' Assemble a list of [Agent]s from several kinds of input, the way an
#' agent-based study defines its actors. `agent_population()` accepts
#' several input forms and returns one strict output: whatever you pass,
#' you get back a flat list of constructed agents with stable ids.
#'
#' `personas` may be:
#'
#' - a list of pre-built [Agent]s, used as is (no `config` needed);
#' - a [persona_variants()] result (a `persona_set`), one agent per row, each
#'   built from that row's [persona_frame()];
#' - a character vector of persona briefs, one agent each;
#' - a single [persona_frame()] or string with `n > 1`, replicated into `n`
#'   agents named `p1 ... pn`.
#'
#' A population is scaffolding, not a sample: it inherits every limit of the
#' personas it is built from. Pair it with [persona_audit()] before reading any
#' result as if it spoke for the people the briefs sketch.
#'
#' @param personas Pre-built agents, a `persona_set`, a character vector of
#'   briefs, or a single persona (frame or string) to replicate.
#' @param n Number of copies when `personas` is a single persona; ignored
#'   otherwise.
#' @param config An `LLMR::llm_config()` used to build agents. Required unless
#'   `personas` is already a list of [Agent]s.
#' @return An object of class `agent_population`: a list with `agents` (a list
#'   of [Agent]s), `ids` (their agent ids), and `n` (the count).
#' @seealso [society()], [persona_variants()], [agent()]
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' pop <- agent_population(
#'   c("A cautious retiree.", "A risk-tolerant founder."), config = cfg)
#' pop
#' }
#' @export
agent_population <- function(personas, n = NULL, config = NULL) {
  # Already a list of Agents: take them as the population verbatim.
  if (.is_agent_list(personas)) {
    return(.as_agent_population(unname(personas)))
  }

  # A designed persona set: one agent per row, built from its persona_frame.
  if (inherits(personas, "persona_set")) {
    .require_config(config)
    frames <- as.list(personas$persona)
    agents <- lapply(seq_along(frames), function(i) {
      agent(name = paste0("p", i), config = config, persona = frames[[i]])
    })
    return(.as_agent_population(agents))
  }

  # A character vector of briefs: one agent per brief.
  if (is.character(personas) && length(personas) >= 1L) {
    .require_config(config)
    reps <- if (length(personas) == 1L && !is.null(n)) {
      rep(personas, .check_n(n))
    } else {
      personas
    }
    agents <- lapply(seq_along(reps), function(i) {
      agent(name = paste0("p", i), config = config, persona = reps[[i]])
    })
    return(.as_agent_population(agents))
  }

  # A single persona_frame: replicate into n copies.
  if (inherits(personas, "persona_frame")) {
    .require_config(config)
    k <- .check_n(n %||% 1L)
    agents <- lapply(seq_len(k), function(i) {
      agent(name = paste0("p", i), config = config, persona = personas)
    })
    return(.as_agent_population(agents))
  }

  stop("`personas` must be a list of Agent objects, a persona_set, a ",
       "character vector of briefs, or a single persona_frame/string.",
       call. = FALSE)
}

# TRUE for a non-empty list whose every element is an Agent.
#' @keywords internal
#' @noRd
.is_agent_list <- function(x) {
  is.list(x) && !inherits(x, "persona_set") && length(x) >= 1L &&
    all(vapply(x, inherits, logical(1), "Agent"))
}

# config is mandatory unless the personas are already Agents.
#' @keywords internal
#' @noRd
.require_config <- function(config) {
  if (is.null(config)) {
    stop("`config` (an LLMR::llm_config()) is required unless `personas` is ",
         "already a list of Agent objects.", call. = FALSE)
  }
  .check_config(config)
}

# Coerce a replication count to a positive integer.
#' @keywords internal
#' @noRd
.check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 1L) {
    stop("`n` must be a single positive integer.", call. = FALSE)
  }
  as.integer(n)
}

# Wrap a list of Agents in the agent_population structure.
#' @keywords internal
#' @noRd
.as_agent_population <- function(agents) {
  ids <- vapply(agents, function(a) a$id(), character(1))
  structure(list(agents = agents, ids = ids, n = length(agents)),
            class = "agent_population")
}

#' Assemble a society from a population, a network, and measures
#'
#' A `society` is the static apparatus an interaction simulation runs on: the
#' [agent_population()] (its actors), an edge list (who may see whom), an
#' optional set of measurement functions, and an initially empty shared
#' transcript that [step_interaction()] grows one round at a time. It holds no
#' results until you step it.
#'
#' The network constrains co-presence, not the engine. An edge between two
#' agents records that they are connected; [exposure_matrix()] reads the edges
#' as "who could see whom". The current stepping rule keeps the transcript
#' fully shared (every speaker sees the whole history) while still recording the
#' connectivity, so the exposure structure is available for analysis even
#' before a stricter visibility rule is added.
#'
#' @param population An [agent_population()].
#' @param network Who may interact with whom. `NULL` (default) means fully
#'   connected. Otherwise a two-column edge list (a `data.frame` or `matrix`)
#'   of agent ids or integer indices, or an `igraph` graph (its edge list is
#'   read via `igraph::as_edgelist()` when the package is installed).
#' @param measures Optional named list of `function(agent) -> value`, applied by
#'   [collect_measures()].
#' @return An object of class `society`: a list with `population`, `edges` (a
#'   tibble `from`, `to`), `measures`, `history` (a tibble `turn`, `step`,
#'   `speaker`, `text`), and `step` (the round counter, `0L` initially).
#' @seealso [step_interaction()], [collect_measures()], [exposure_matrix()],
#'   [contamination_report()]
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' pop <- agent_population(c("A.", "B.", "C."), config = cfg)
#' soc <- society(pop, network = data.frame(from = c("p1"), to = c("p2")))
#' soc <- step_interaction(soc, prompt = "Introduce yourself in one line.")
#' collect_measures(soc)
#' }
#' @export
society <- function(population, network = NULL, measures = NULL) {
  if (!inherits(population, "agent_population")) {
    stop("`population` must be an agent_population() object.", call. = FALSE)
  }
  if (!is.null(measures)) {
    if (!is.list(measures) || is.null(names(measures)) ||
        any(!nzchar(names(measures))) ||
        !all(vapply(measures, is.function, logical(1)))) {
      stop("`measures` must be a named list of functions(agent) -> value.",
           call. = FALSE)
    }
  }
  edges <- .society_edges(network, population)
  history <- tibble::tibble(turn = integer(0), step = integer(0),
                            speaker = character(0), text = character(0))
  structure(
    list(population = population, edges = edges, measures = measures,
         history = history, step = 0L),
    class = "society")
}

# Normalize the `network` argument into a tibble of from/to agent *names*. The
# fully connected default enumerates every unordered pair. An edge list (frame
# or matrix) is read as its first two columns; integer entries index into the
# population's agents, character entries are matched against agent names or ids.
# An igraph graph is converted through igraph::as_edgelist when available.
#' @keywords internal
#' @noRd
.society_edges <- function(network, population) {
  names_vec <- vapply(population$agents, function(a) a$name, character(1))
  empty <- tibble::tibble(from = character(0), to = character(0))

  if (is.null(network)) {
    # Fully connected: all unordered pairs (an isolate population yields none).
    if (length(names_vec) < 2L) return(empty)
    cb <- utils::combn(names_vec, 2L)
    return(tibble::tibble(from = cb[1, ], to = cb[2, ]))
  }

  if (inherits(network, "igraph")) {
    if (!requireNamespace("igraph", quietly = TRUE)) {
      stop("An igraph `network` needs the igraph package installed.",
           call. = FALSE)
    }
    el <- igraph::as_edgelist(network, names = TRUE)
    return(.resolve_edges(el[, 1], el[, 2], names_vec, population$ids))
  }

  if (is.matrix(network) || is.data.frame(network)) {
    if (ncol(network) < 2L) {
      stop("A `network` edge list must have at least two columns (from, to).",
           call. = FALSE)
    }
    from <- if (is.data.frame(network)) network[[1]] else network[, 1]
    to   <- if (is.data.frame(network)) network[[2]] else network[, 2]
    return(.resolve_edges(from, to, names_vec, population$ids))
  }

  stop("`network` must be NULL, a two-column edge list (data.frame/matrix), ",
       "or an igraph graph.", call. = FALSE)
}

# Map raw edge endpoints to agent names. Integer-ish endpoints index agents in
# order; everything else is matched against agent names first, then agent ids.
# Unresolvable or self endpoints are dropped (a self-loop is not co-presence).
#' @keywords internal
#' @noRd
.resolve_edges <- function(from, to, names_vec, ids) {
  resolve <- function(v) {
    v <- unname(v)
    out <- character(length(v))
    for (i in seq_along(v)) {
      out[i] <- .resolve_one(v[[i]], names_vec, ids)
    }
    out
  }
  f <- resolve(from)
  t <- resolve(to)
  keep <- !is.na(f) & !is.na(t) & nzchar(f) & nzchar(t) & (f != t)
  tibble::tibble(from = f[keep], to = t[keep])
}

# Resolve a single endpoint to an agent name (NA if it cannot be matched).
#' @keywords internal
#' @noRd
.resolve_one <- function(x, names_vec, ids) {
  if (is.factor(x)) x <- as.character(x)
  if (is.numeric(x) || (is.character(x) && grepl("^[0-9]+$", x))) {
    idx <- suppressWarnings(as.integer(x))
    if (!is.na(idx) && idx >= 1L && idx <= length(names_vec)) return(names_vec[idx])
    return(NA_character_)
  }
  x <- as.character(x)
  if (x %in% names_vec) return(x)
  hit <- which(ids == x)
  if (length(hit)) return(names_vec[hit[1]])
  NA_character_
}

#' Advance one interaction round
#'
#' Run a single round of the society: the chosen agents each speak once, given
#' the shared `history` so far, and their utterances are appended with a new
#' step index. Speaking uses the [Agent]'s stateless `reply()` over the shared
#' transcript (the same role-flipped construction [conversation()] uses), so no
#' agent writes to its own memory and the transcript remains the single record.
#'
#' Who speaks: all of `who` (by agent name) when supplied, otherwise every agent
#' that has at least one edge in the network (an isolate with no edges is
#' skipped). Each speaker currently sees the full shared history; the edge list
#' is recorded as the exposure structure (see [exposure_matrix()]) rather than
#' used to mask the transcript, which keeps the round simple while leaving the
#' connectivity available for analysis.
#'
#' @param society A [society()].
#' @param who Optional character vector of agent names to speak this round; the
#'   default lets every connected agent speak.
#' @param prompt Optional instruction appended as the "your turn" cue for each
#'   speaker. Default: "Contribute to the discussion."
#' @param ... Passed to each agent's underlying LLMR call.
#' @return The updated [society()] (its `step` incremented and `history`
#'   extended by one utterance per speaker).
#' @seealso [society()], [collect_measures()], [exposure_matrix()]
#' @examples
#' \dontrun{
#' soc <- step_interaction(soc, prompt = "React in one sentence.")
#' soc$history
#' }
#' @export
step_interaction <- function(society, who = NULL, prompt = NULL, ...) {
  if (!inherits(society, "society")) {
    stop("`society` must be a society() object.", call. = FALSE)
  }
  agents <- society$population$agents
  names(agents) <- vapply(agents, function(a) a$name, character(1))

  speakers <- if (!is.null(who)) {
    who <- as.character(who)
    miss <- setdiff(who, names(agents))
    if (length(miss)) {
      stop("`who` names agents not in the population: ",
           paste(miss, collapse = ", "), ".", call. = FALSE)
    }
    who
  } else {
    .connected_agents(society)
  }
  if (!length(speakers)) return(society)

  step <- society$step + 1L
  history <- society$history
  base_turn <- if (nrow(history)) max(history$turn) else 0L
  cue <- prompt %||% "Contribute to the discussion."

  for (k in seq_along(speakers)) {
    who_k <- speakers[[k]]
    spk <- agents[[who_k]]
    sys <- paste(c(
      spk$persona,
      paste0("You are ", who_k, ", taking part in a small social interaction."),
      "Reply in character, in your own voice.",
      "Do not write lines for other participants and do not prefix your reply with your name."
    ), collapse = "\n")
    # The shared transcript so far drives the reply (role-flipped per speaker),
    # exactly as conversation() builds a turn. history carries an extra `step`
    # column, so pass only the speaker/text view that .dialogue_messages reads.
    text <- spk$reply(
      .dialogue_messages(history[, c("speaker", "text"), drop = FALSE],
                         speaker = who_k, sys = sys, turn = cue),
      ...)
    history <- rbind(history, tibble::tibble(
      turn = base_turn + k, step = step, speaker = who_k, text = text))
  }

  society$history <- history
  society$step <- step
  society
}

# The agents that appear as an endpoint of at least one edge, in population
# order. With no edges (an isolate population), this is empty.
#' @keywords internal
#' @noRd
.connected_agents <- function(society) {
  ep <- unique(c(society$edges$from, society$edges$to))
  names_vec <- vapply(society$population$agents, function(a) a$name, character(1))
  names_vec[names_vec %in% ep]
}

#' Collect measures over a society
#'
#' Apply each measurement function to every agent and return one tidy row per
#' agent-by-measure, stamped with the society's current step. A measure that
#' errors yields `NA` for that agent rather than aborting the sweep. With no
#' measures, the default is each agent's utterance count in the shared history
#' (`"n_utterances"`).
#'
#' The result always carries `attr(out, "uncalibrated") <- TRUE`. These are
#' model outputs, not measurements of people: the attribute is the structural
#' reminder that any quantity here is conditioned on the models and the prompts,
#' not validated against human data.
#'
#' @param society A [society()].
#' @param measures Optional named list of `function(agent) -> value`; defaults
#'   to the society's own `measures`.
#' @return A tibble with columns `agent_id`, `name`, `measure`, `value`, and
#'   `step`, carrying attribute `uncalibrated = TRUE`.
#' @seealso [society()], [step_interaction()]
#' @examples
#' \dontrun{
#' collect_measures(soc, measures = list(
#'   words = function(a) nchar(a$persona %||% "")))
#' }
#' @export
collect_measures <- function(society, measures = NULL) {
  if (!inherits(society, "society")) {
    stop("`society` must be a society() object.", call. = FALSE)
  }
  agents <- society$population$agents
  ms <- measures %||% society$measures
  step <- society$step

  if (is.null(ms) || !length(ms)) {
    # Default measure: how many utterances each agent has contributed so far.
    hist <- society$history
    out <- .measure_rows(agents, "n_utterances", function(a) {
      sum(hist$speaker == a$name)
    }, step)
    attr(out, "uncalibrated") <- TRUE
    return(out)
  }

  parts <- lapply(names(ms), function(nm) {
    .measure_rows(agents, nm, ms[[nm]], step)
  })
  out <- do.call(rbind, parts)
  attr(out, "uncalibrated") <- TRUE
  out
}

# One tibble of agent_id/name/measure/value/step for a single measure applied
# across the agents. A failing measure yields NA for that agent.
#' @keywords internal
#' @noRd
.measure_rows <- function(agents, measure, fn, step) {
  tibble::tibble(
    agent_id = vapply(agents, function(a) a$id(), character(1)),
    name     = vapply(agents, function(a) a$name, character(1)),
    measure  = measure,
    value    = vapply(agents, function(a) {
      v <- tryCatch(fn(a), error = function(e) NA)
      if (length(v) != 1L) NA_real_ else suppressWarnings(as.numeric(v))
    }, numeric(1)),
    step     = as.integer(step))
}

#' Exposure matrix: who could see whom
#'
#' Read the society's edge list as a symmetric agents-by-agents 0/1 matrix:
#' entry `[i, j]` is `1` when agents `i` and `j` are connected (an edge exists
#' in either direction), `0` otherwise. Row and column names are agent ids. This
#' is the "who could see whom" structure the network encodes, separate from what
#' any given round actually showed each agent.
#'
#' @param society A [society()].
#' @return A numeric matrix (agents x agents) of 0/1 with agent ids as
#'   dimnames; the diagonal is `0`.
#' @seealso [society()], [step_interaction()]
#' @examples
#' \dontrun{
#' exposure_matrix(soc)
#' }
#' @export
exposure_matrix <- function(society) {
  if (!inherits(society, "society")) {
    stop("`society` must be a society() object.", call. = FALSE)
  }
  agents <- society$population$agents
  ids   <- vapply(agents, function(a) a$id(), character(1))
  names_vec <- vapply(agents, function(a) a$name, character(1))
  n <- length(agents)
  m <- matrix(0, nrow = n, ncol = n, dimnames = list(ids, ids))

  edges <- society$edges
  if (nrow(edges)) {
    idx <- function(nm) match(nm, names_vec)
    fi <- idx(edges$from)
    ti <- idx(edges$to)
    for (e in seq_len(nrow(edges))) {
      i <- fi[e]; j <- ti[e]
      if (!is.na(i) && !is.na(j) && i != j) {
        m[i, j] <- 1
        m[j, i] <- 1
      }
    }
  }
  m
}

#' Flag shared agent instances across a population
#'
#' A correct population is a set of distinct agents. If two slots hold the same
#' live [Agent] (built once and reused), they share memory, counters, and
#' identity, which silently couples the actors that should be independent. Each
#' Agent carries a stable id, so a duplicate id among the population's slots is
#' direct evidence of one reused instance. `contamination_report()` detects
#' exactly that.
#'
#' @param society A [society()].
#' @return An object of class `society_contamination`: a tibble with columns
#'   `agent_id`, `name`, `slots` (the positions sharing that id), and `n` (how
#'   many slots), one row per id that appears more than once. A `clean`
#'   attribute records whether any duplicate was found.
#' @seealso [check_state_leakage()], [agent_population()]
#' @examples
#' \dontrun{
#' contamination_report(soc)
#' }
#' @export
contamination_report <- function(society) {
  if (!inherits(society, "society")) {
    stop("`society` must be a society() object.", call. = FALSE)
  }
  ids <- society$population$ids
  names_vec <- vapply(society$population$agents, function(a) a$name, character(1))

  dup_ids <- unique(ids[duplicated(ids)])
  rows <- lapply(dup_ids, function(aid) {
    slots <- which(ids == aid)
    # Compute the count before the tibble call: a `slots` column that holds the
    # collapsed string would otherwise shadow the integer vector for `n`, since
    # tibble evaluates its columns sequentially in a shared mask.
    n_slots <- length(slots)
    tibble::tibble(
      agent_id = aid,
      name     = names_vec[slots[1]],
      slots    = paste(slots, collapse = ", "),
      n        = n_slots)
  })

  out <- if (length(rows)) do.call(rbind, rows) else tibble::tibble(
    agent_id = character(0), name = character(0),
    slots = character(0), n = integer(0))
  attr(out, "clean") <- (nrow(out) == 0L)
  class(out) <- c("society_contamination", class(out))
  out
}

#' @rdname society
#' @param x A `society`.
#' @param ... Ignored.
#' @export
print.society <- function(x, ...) {
  cat(sprintf("<society | %d agent(s) | %d edge(s) | step %d>\n",
              x$population$n, nrow(x$edges), x$step))
  invisible(x)
}

#' @rdname agent_population
#' @param x An `agent_population`.
#' @param ... Ignored.
#' @export
print.agent_population <- function(x, ...) {
  names_vec <- vapply(x$agents, function(a) a$name, character(1))
  shown <- paste(utils::head(names_vec, 6L), collapse = ", ")
  if (length(names_vec) > 6L) shown <- paste0(shown, ", ...")
  cat(sprintf("<agent_population | %d agent(s): %s>\n", x$n, shown))
  invisible(x)
}

#' Draft a disciplined report for a society
#'
#' A short account of a [society()] run that refuses to overclaim. It states the
#' population and network size and the number of rounds taken, then always
#' prints the line that these are model-conditioned simulations, not population
#' facts unless calibrated against human data, and an uncalibrated banner. The
#' caveat is not optional: a society of language models is an apparatus, and the
#' report says so every time.
#'
#' @param x A [society()].
#' @param ... Unused.
#' @return An object of class `agent_report` (a character vector with a print
#'   method) that includes the uncalibrated discipline lines.
#' @seealso [society()], [collect_measures()]
#' @importFrom LLMR report
#' @exportS3Method LLMR::report society
report.society <- function(x, ...) {
  pop <- x$population
  body <- c(
    sprintf("Society: %d agent(s), %d edge(s), %d round(s) taken.",
            pop$n, nrow(x$edges), x$step),
    sprintf("Utterances recorded: %d.", nrow(x$history)),
    "",
    "UNCALIBRATED: this society has not been validated against human data.",
    paste0("These are model-conditioned simulations, not population facts ",
           "unless calibrated against human data."))
  structure(body, class = "agent_report")
}

#' @export
print.society_contamination <- function(x, ...) {
  clean <- isTRUE(attr(x, "clean"))
  cat(sprintf("<society_contamination | clean: %s>\n",
              if (clean) "TRUE" else "FALSE"))
  if (clean) {
    cat("  No shared agent instances detected in the population.\n")
  } else {
    print(tibble::as_tibble(x))
  }
  invisible(x)
}
