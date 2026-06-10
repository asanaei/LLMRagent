# superbrain.R ------------------------------------------------------------------
# One strong model plans and synthesizes; many cheap models do the legwork in
# parallel. For hard questions this buys much of the strong model's quality at
# a fraction of its price, and the intermediate products stay inspectable.

#' Crack a hard problem with one strong model and many cheap ones
#'
#' A four-stage orchestration:
#'
#' 1. **Plan** (strong model, 1 call): decompose the problem into
#'    `n_approaches` genuinely different lines of attack.
#' 2. **Work** (cheap model, `n_approaches` parallel calls): each line is
#'    pursued independently, blind to the others.
#' 3. **Synthesize** (strong model, 1 call): weigh the drafts against one
#'    another, resolve contradictions, and write the answer.
#' 4. **Verify** (strong model, 1 call, optional): attack the synthesized
#'    answer; if a real flaw is found, one revision pass runs.
#'
#' The economics: two to three strong-model calls regardless of how wide the
#' fan-out is, with the bulk of tokens billed at the cheap model's rate. The
#' returned object keeps every intermediate product, so you can audit what
#' each worker contributed and what the verifier objected to.
#'
#' @param problem The problem statement (character scalar). Be complete:
#'   workers see nothing but this and their assigned approach.
#' @param strong_config An `LLMR::llm_config()` for the strong (planner /
#'   synthesizer / verifier) model.
#' @param cheap_config An `LLMR::llm_config()` for the cheap worker model.
#' @param n_approaches Number of independent lines of attack (default 4).
#' @param verify If TRUE (default), run the verification stage and one
#'   revision round when the verifier finds a substantive flaw.
#' @param quiet FALSE prints stage progress.
#' @param ... Passed to `LLMR::call_llm_par()` for the worker stage
#'   (e.g. `tries`, `progress`).
#' @return A list of class `super_brain`:
#'   \describe{
#'     \item{`answer`}{The final answer (character).}
#'     \item{`plan`}{Tibble of approaches: `title`, `instructions`.}
#'     \item{`workers`}{Tibble: `approach`, `output`, `success`, plus token
#'       diagnostics from `LLMR::call_llm_par()`.}
#'     \item{`verification`}{The verifier's structured critique (or NULL).}
#'     \item{`revised`}{TRUE when the revision pass ran.}
#'   }
#' @examples
#' \dontrun{
#' strong <- LLMR::llm_config("deepseek", "deepseek-reasoner")
#' cheap  <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.8)
#' out <- think_harder(
#'   "A city of 1M residents wants to halve traffic deaths in 5 years
#'    with a budget of $40M. Propose the most cost-effective portfolio
#'    of interventions, with rough numbers.",
#'   strong_config = strong, cheap_config = cheap, n_approaches = 5
#' )
#' cat(out$answer)
#' out$workers[, c("approach", "success")]
#' }
#' @export
think_harder <- function(problem, strong_config, cheap_config,
                         n_approaches = 4L, verify = TRUE,
                         quiet = FALSE, ...) {
  stopifnot(is.character(problem), length(problem) == 1L, nzchar(problem))
  .check_config(strong_config, "strong_config")
  .check_config(cheap_config, "cheap_config")
  n_approaches <- max(2L, as.integer(n_approaches))

  # ---- 1. plan ----------------------------------------------------------------
  if (!quiet) cli::cli_text("{.strong [1/4]} planning with {strong_config$model} ...")
  plan_schema <- list(
    type = "object",
    properties = list(
      approaches = list(
        type = "array",
        items = list(
          type = "object",
          properties = list(
            title = list(type = "string"),
            instructions = list(type = "string")
          ),
          required = list("title", "instructions")
        )
      )
    ),
    required = list("approaches")
  )
  plan_resp <- LLMR::call_llm_robust(
    LLMR::enable_structured_output(strong_config, schema = plan_schema),
    c(system = paste(
        "You decompose hard problems for a team of independent solvers.",
        sprintf("Produce exactly %d approaches that are genuinely DIFFERENT:", n_approaches),
        "different framings, methods, or bodies of evidence, not paraphrases.",
        "Each instructions field must be self-contained: the solver sees only",
        "the problem text and that field."),
      user = problem))
  plan_parsed <- LLMR::llm_parse_structured(plan_resp)
  approaches <- plan_parsed$approaches %||% list()
  if (!length(approaches)) {
    stop("The planner returned no approaches; inspect the model/config.", call. = FALSE)
  }
  plan_tbl <- tibble::tibble(
    title = vapply(approaches, function(a) as.character(a$title %||% ""), character(1)),
    instructions = vapply(approaches, function(a) as.character(a$instructions %||% ""), character(1))
  )

  # ---- 2. work (parallel, cheap) ------------------------------------------------
  if (!quiet) cli::cli_text("{.strong [2/4]} {nrow(plan_tbl)} workers on {cheap_config$model} ...")
  experiments <- tibble::tibble(
    approach = plan_tbl$title,
    config = rep(list(cheap_config), nrow(plan_tbl)),
    messages = lapply(seq_len(nrow(plan_tbl)), function(i) {
      c(system = paste(
          "You are one of several independent solvers attacking the same problem",
          "from different angles. Pursue ONLY your assigned approach, thoroughly.",
          "End with your best concrete answer under the heading FINDINGS."),
        user = paste0("PROBLEM:\n", problem,
                      "\n\nYOUR ASSIGNED APPROACH: ", plan_tbl$title[i],
                      "\n", plan_tbl$instructions[i]))
    })
  )
  workers <- LLMR::call_llm_par(experiments, ...)

  ok <- workers$success %in% TRUE
  if (!any(ok)) stop("All workers failed; inspect the cheap_config.", call. = FALSE)
  drafts <- paste(sprintf("### Worker %d: %s\n\n%s",
                          seq_len(sum(ok)),
                          workers$approach[ok],
                          workers$response_text[ok]),
                  collapse = "\n\n---\n\n")

  # ---- 3. synthesize (strong) ----------------------------------------------------
  if (!quiet) cli::cli_text("{.strong [3/4]} synthesizing ...")
  synth <- LLMR::call_llm_robust(
    strong_config,
    c(system = paste(
        "You are the lead analyst. Several independent workers attacked the",
        "problem from different angles. Weigh their drafts against each other,",
        "discard what does not survive scrutiny, resolve contradictions",
        "explicitly, and write the best final answer. Do not mention the",
        "workers; just answer the problem, completely and concretely."),
      user = paste0("PROBLEM:\n", problem, "\n\nWORKER DRAFTS:\n\n", drafts)))
  answer <- as.character(synth)

  # ---- 4. verify + one revision (strong) ----------------------------------------
  verification <- NULL
  revised <- FALSE
  if (isTRUE(verify)) {
    if (!quiet) cli::cli_text("{.strong [4/4]} verifying ...")
    verdict_schema <- list(
      type = "object",
      properties = list(
        sound = list(type = "boolean"),
        flaws = list(type = "array", items = list(type = "string"))
      ),
      required = list("sound", "flaws")
    )
    vresp <- LLMR::call_llm_robust(
      LLMR::enable_structured_output(strong_config, schema = verdict_schema),
      c(system = paste(
          "You are a hostile reviewer. Find substantive flaws in the answer:",
          "errors of fact or arithmetic, unsupported leaps, ignored constraints",
          "from the problem statement. Style is not a flaw. If the answer is",
          "sound, say so."),
        user = paste0("PROBLEM:\n", problem, "\n\nANSWER UNDER REVIEW:\n", answer)))
    verification <- LLMR::llm_parse_structured(vresp)
    flaws <- verification$flaws %||% list()
    if (!isTRUE(verification$sound) && length(flaws)) {
      rresp <- LLMR::call_llm_robust(
        strong_config,
        c(system = "Revise the answer to repair the listed flaws. Keep everything that was right.",
          user = paste0("PROBLEM:\n", problem,
                        "\n\nCURRENT ANSWER:\n", answer,
                        "\n\nFLAWS TO REPAIR:\n- ",
                        paste(vapply(flaws, as.character, character(1)),
                              collapse = "\n- "))))
      answer <- as.character(rresp)
      revised <- TRUE
    }
  }

  structure(
    list(answer = answer, plan = plan_tbl, workers = workers,
         verification = verification, revised = revised),
    class = "super_brain"
  )
}

#' @export
print.super_brain <- function(x, ...) {
  cat(sprintf("<super_brain | %d approaches | %d/%d workers ok | revised: %s>\n\n",
              nrow(x$plan), sum(x$workers$success %in% TRUE), nrow(x$workers),
              x$revised))
  cat(x$answer, "\n")
  invisible(x)
}
