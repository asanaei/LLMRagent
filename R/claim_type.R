# claim_type.R ----------------------------------------------------------------
# Claim-type discipline. A run is labeled by the kind of model-conditioned
# claim it can support. The methods report consults the claim type, and a prose
# lint refuses or scopes population-estimate language.

#' Mark the kind of claim a run can support
#'
#' Records, on a run, what its results may be used to argue. The three types
#' are `"instrument_pilot"` (the run characterizes an
#' instrument, not a population), `"theory_probe"` (it explores a mechanism or
#' hypothesis), and `"coding"` (it annotates data, with reliability reported).
#'
#' The label flows into [report()]: prose that would overstate the claim is
#' scoped or refused. None of these types turns model output into a population
#' estimate.
#'
#' @param run An object accepted by [as_agent_run()].
#' @param type One of `"instrument_pilot"`, `"theory_probe"`, or `"coding"`.
#' @return The run (an `agent_run`), invisibly, with the claim type recorded.
#' @seealso [report()]
#' @examples
#' \dontrun{
#' run <- as_agent_run(my_deliberation)
#' run <- mark_claim_type(run, "theory_probe")
#' report(run)   # prose is scoped to a theory probe, not a population claim
#' }
#' @export
mark_claim_type <- function(run, type = c("instrument_pilot", "theory_probe",
                                          "coding")) {
  type <- match.arg(type)
  r <- as_agent_run(run)
  r$claim_type <- type
  invisible(r)
}

# Population-estimate constructions the lint flags. Conservative: aimed at
# phrasings that assert a fact about a human population rather than a model.
#' @keywords internal
#' @noRd
.population_claim_patterns <- function() {
  c(
    "(?i)\\b\\d+(\\.\\d+)?\\s*%\\s+of\\s+(people|americans|respondents|the population|adults|voters|women|men)\\b",
    "(?i)\\b(americans|respondents|voters|people|the public)\\s+(believe|think|feel|want|support|oppose|prefer)\\b",
    "(?i)\\bthe population('s)?\\b",
    "(?i)\\bin the general population\\b",
    "(?i)\\bgeneralizes? to (the population|all|humans|people)\\b"
  )
}

# Scan text for population-estimate language; return the offending substrings.
#' @keywords internal
#' @noRd
.scan_population_claims <- function(text) {
  hits <- character(0)
  for (p in .population_claim_patterns()) {
    m <- regmatches(text, gregexpr(p, text, perl = TRUE))[[1]]
    if (length(m)) hits <- c(hits, m)
  }
  unique(hits)
}

#' Assert (or scope) prose against a run's claim type
#'
#' Checks a piece of text for population-estimate language and either appends a
#' scope sentence (`action = "scope"`, the default) or raises
#' `llmragent_claim_error` (`action = "error"`). No current claim type
#' authorizes population-estimate language. Used by [report()]; exported so
#' custom report code can reuse it.
#'
#' @param text Character vector of prose to check.
#' @param run An object accepted by [as_agent_run()] (supplies the claim type
#'   named in a scope message), or `NULL`.
#' @param action `"scope"` (append a caveat when a population claim is found) or
#'   `"error"` (raise).
#' @return The text (a character vector), possibly with a caveat appended.
#' @seealso [mark_claim_type()], [report()]
#' @export
llm_claim_lint <- function(text, run = NULL, action = c("scope", "error")) {
  action <- match.arg(action)
  ct <- NA_character_
  if (!is.null(run)) {
    r <- tryCatch(as_agent_run(run), error = function(e) NULL)
    if (!is.null(r)) ct <- r$claim_type %||% NA_character_
  }
  hits <- .scan_population_claims(paste(text, collapse = "\n"))
  if (!length(hits)) return(text)
  scope <- if (is.na(ct)) "the recorded evidence" else
    sprintf("claim type '%s'", ct)
  if (identical(action, "error")) {
    rlang::abort(
      message = paste0(
        "This text makes population-estimate claims (", paste(utils::head(hits, 3), collapse = "; "),
        ") unsupported by ", scope, ". Scope the language or provide external validation."),
      class = c("llmragent_claim_error", "error", "condition"))
  }
  c(text, "",
    paste0("These results are model-conditioned simulations, not ",
           "estimates of a human population. The phrasing above (e.g. \"",
           utils::head(hits, 1), "\") should be read as describing the configured ",
           "model under this prompt, not people."))
}
