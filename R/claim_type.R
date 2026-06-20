# claim_type.R ----------------------------------------------------------------
# Claim-type discipline. A run is labeled by the kind of claim it can support;
# "calibrated_inference" is refused unless calibration evidence is attached. The
# methods report (report.agent_run, in methods_llmr.R) consults the claim type
# and a prose lint refuses population-estimate language unless the run is
# calibrated. This turns the paper's scope-condition discipline into a default
# the user does not have to remember.

# The ordered set of claim types, weakest to strongest evidentiary standing.
.claim_types <- c("instrument_pilot", "theory_probe", "coding", "calibrated_inference")

#' Mark the kind of claim a run can support
#'
#' Records, on a run, what its results may be used to argue. The four types,
#' from weakest to strongest: `"instrument_pilot"` (the run characterizes an
#' instrument, not a population), `"theory_probe"` (it explores a mechanism or
#' hypothesis), `"coding"` (it annotates data, with reliability reported), and
#' `"calibrated_inference"` (it estimates a quantity validated against human
#' data). The strongest type is refused unless a calibration is attached (see
#' [agent_calibrate()] / [attach_calibration()]), so a simulation cannot be
#' relabeled as a population estimate without the evidence.
#'
#' The label flows into [report()]: prose that would overstate the claim is
#' rewritten or refused for anything short of calibrated inference.
#'
#' @param run An object accepted by [as_agent_run()].
#' @param type One of `"instrument_pilot"`, `"theory_probe"`, `"coding"`,
#'   `"calibrated_inference"`.
#' @return The run (an `agent_run`), invisibly, with the claim type recorded.
#' @seealso [agent_calibrate()], [attach_calibration()], [report()]
#' @examples
#' \dontrun{
#' run <- as_agent_run(my_deliberation)
#' run <- mark_claim_type(run, "theory_probe")
#' report(run)   # prose is scoped to a theory probe, not a population claim
#' }
#' @export
mark_claim_type <- function(run, type = c("instrument_pilot", "theory_probe",
                                          "coding", "calibrated_inference")) {
  type <- match.arg(type)
  r <- as_agent_run(run)
  if (identical(type, "calibrated_inference") && is.null(r$calibration)) {
    rlang::abort(
      message = paste0(
        "Cannot mark a run as \"calibrated_inference\" without calibration ",
        "evidence. Run agent_calibrate() and attach_calibration() first, or use ",
        "a weaker claim type (instrument_pilot, theory_probe, coding)."),
      class = c("llmragent_claim_error", "error", "condition"))
  }
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
#' Checks a piece of text for population-estimate language and, unless the run
#' is a calibrated inference (or carries an attached calibration), either
#' appends a scope caveat (`action = "scope"`, the default) or raises
#' `llmragent_claim_error` (`action = "error"`). Calibrated runs pass through
#' unchanged. Used by [report()]; exported so custom report code can reuse it.
#'
#' @param text Character vector of prose to check.
#' @param run An object accepted by [as_agent_run()] (supplies the claim type
#'   and calibration status), or `NULL` to treat the text as uncalibrated.
#' @param action `"scope"` (append a caveat when a population claim is found) or
#'   `"error"` (raise).
#' @return The text (a character vector), possibly with a caveat appended.
#' @seealso [mark_claim_type()], [report()]
#' @export
llm_claim_lint <- function(text, run = NULL, action = c("scope", "error")) {
  action <- match.arg(action)
  ct <- NA_character_; has_cal <- FALSE
  if (!is.null(run)) {
    r <- tryCatch(as_agent_run(run), error = function(e) NULL)
    if (!is.null(r)) { ct <- r$claim_type %||% NA_character_; has_cal <- !is.null(r$calibration) }
  }
  if (identical(ct, "calibrated_inference") || has_cal) return(text)
  hits <- .scan_population_claims(paste(text, collapse = "\n"))
  if (!length(hits)) return(text)
  if (identical(action, "error")) {
    rlang::abort(
      message = paste0(
        "This text makes population-estimate claims (", paste(utils::head(hits, 3), collapse = "; "),
        ") but the run is not a calibrated inference. Scope the language or attach calibration."),
      class = c("llmragent_claim_error", "error", "condition"))
  }
  c(text, "",
    paste0("[scope caveat] These results are model-conditioned simulation, not ",
           "estimates of a human population. The phrasing above (e.g. \"",
           utils::head(hits, 1), "\") should be read as describing the configured ",
           "model under this prompt, not people, unless calibrated against human data."))
}
