# superbrain.R ------------------------------------------------------------------
# One strong model plans and synthesizes; many cheap models draft candidate answers in
# parallel. For hard questions this keeps much of the strong model's quality while
# making far fewer strong-model calls, and the intermediate products stay inspectable.

#' Work a hard problem with one strong model and many cheap ones
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
  # Accumulate the strong-model responses with their request context so the run
  # object can build a faithful per-call provenance trail (the workers frame
  # already carries the cheap-model records).
  strong_calls <- list()
  record_strong <- function(resp, request, stage) {
    rr <- tryCatch(LLMR::llm_response_record(resp, request = request,
                                             config = strong_config),
                   error = function(e) NULL)
    if (!is.null(rr)) {
      rr$stage <- stage
      # Keep the request messages and reply metadata alongside the record so the
      # run object can emit a TRUE LLMR audit log (request body included), not
      # just a flat record. (Mirrors what Agent$account() stashes per call.)
      strong_calls[[length(strong_calls) + 1L]] <<- list(
        record = rr, stage = stage, request = request,
        provider = strong_config$provider %||% NA_character_,
        model = strong_config$model %||% NA_character_,
        model_version = if (inherits(resp, "llmr_response")) resp$model_version else NA_character_,
        finish_reason = if (inherits(resp, "llmr_response")) resp$finish_reason else NA_character_,
        text = if (inherits(resp, "llmr_response")) resp$text else NA_character_,
        usage = if (inherits(resp, "llmr_response")) resp$usage else NULL)
    }
    resp
  }

  plan_msg <- c(system = paste(
        "You decompose hard problems for a team of independent solvers.",
        sprintf("Produce exactly %d approaches that are genuinely DIFFERENT:", n_approaches),
        "different framings, methods, or bodies of evidence, not paraphrases.",
        "Each instructions field must be self-contained: the solver sees only",
        "the problem text and that field."),
      user = problem)
  plan_resp <- record_strong(LLMR::call_llm_robust(
    LLMR::enable_structured_output(strong_config, schema = plan_schema),
    plan_msg), plan_msg, "plan")
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
  synth_msg <- c(system = paste(
        "You are the lead analyst. Several independent workers attacked the",
        "problem from different angles. Weigh their drafts against each other,",
        "discard what does not survive scrutiny, resolve contradictions",
        "explicitly, and write the best final answer. Do not mention the",
        "workers; just answer the problem, completely and concretely."),
      user = paste0("PROBLEM:\n", problem, "\n\nWORKER DRAFTS:\n\n", drafts))
  synth <- record_strong(LLMR::call_llm_robust(strong_config, synth_msg),
                         synth_msg, "synthesize")
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
    vmsg <- c(system = paste(
          "You are a hostile reviewer. Find substantive flaws in the answer:",
          "errors of fact or arithmetic, unsupported leaps, ignored constraints",
          "from the problem statement. Style is not a flaw. If the answer is",
          "sound, say so."),
        user = paste0("PROBLEM:\n", problem, "\n\nANSWER UNDER REVIEW:\n", answer))
    vresp <- record_strong(LLMR::call_llm_robust(
      LLMR::enable_structured_output(strong_config, schema = verdict_schema),
      vmsg), vmsg, "verify")
    verification <- LLMR::llm_parse_structured(vresp)
    flaws <- verification$flaws %||% list()
    if (!isTRUE(verification$sound) && length(flaws)) {
      rmsg <- c(system = "Revise the answer to repair the listed flaws. Keep everything that was right.",
          user = paste0("PROBLEM:\n", problem,
                        "\n\nCURRENT ANSWER:\n", answer,
                        "\n\nFLAWS TO REPAIR:\n- ",
                        paste(vapply(flaws, as.character, character(1)),
                              collapse = "\n- ")))
      rresp <- record_strong(LLMR::call_llm_robust(strong_config, rmsg),
                             rmsg, "revise")
      answer <- as.character(rresp)
      revised <- TRUE
    }
  }

  structure(
    list(answer = answer, plan = plan_tbl, workers = workers,
         verification = verification, revised = revised,
         provenance = list(
           run_id = .llmragent_id("run"),
           kind = "think_harder",
           design = list(n_approaches = n_approaches, verify = verify,
                         strong_model = strong_config$model,
                         cheap_model = cheap_config$model),
           strong_calls = strong_calls,
           created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
           llmr_log = .llmragent_active_log())),
    class = "super_brain"
  )
}

#' @exportS3Method as_agent_run super_brain
as_agent_run.super_brain <- function(x, ...) {
  prov <- x$provenance
  # Strong-model calls: build spans from the captured records, carrying the
  # request body + reply metadata so the archive can emit a true audit log.
  spans <- lapply(prov$strong_calls, function(sc) {
    rr <- sc$record
    list(span_id = .llmragent_id("span"), parent_id = NA_character_,
         run_id = prov$run_id, agent_id = "strong",
         event_type = "call", status = if (isTRUE(rr$success[[1]])) "ok" else "error",
         started_at = Sys.time(), ended_at = Sys.time(),
         duration_s = rr$duration_s[[1]] %||% NA_real_,
         tokens_sent = rr$sent_tokens[[1]], tokens_received = rr$rec_tokens[[1]],
         tool = NA_character_, request_hash = rr$request_hash[[1]],
         response_id = rr$response_id[[1]], note = sc$stage %||% NA_character_,
         meta = list(record = rr[, setdiff(names(rr), "stage"), drop = FALSE],
                     stage = sc$stage, request = sc$request,
                     provider = sc$provider, model = sc$model,
                     model_version = sc$model_version, finish_reason = sc$finish_reason,
                     text = sc$text, usage = sc$usage))
  })
  # Cheap workers: convert each response object in the workers frame.
  w <- x$workers
  if (is.data.frame(w) && "response" %in% names(w)) {
    for (i in seq_len(nrow(w))) {
      resp <- w$response[[i]]
      rr <- if (inherits(resp, "llmr_response"))
        tryCatch(LLMR::llm_response_record(resp), error = function(e) NULL) else NULL
      spans[[length(spans) + 1L]] <- list(
        span_id = .llmragent_id("span"), parent_id = NA_character_,
        run_id = prov$run_id, agent_id = "worker",
        event_type = "call", status = if (isTRUE(w$success[i])) "ok" else "error",
        started_at = Sys.time(), ended_at = Sys.time(),
        duration_s = if ("duration" %in% names(w)) w$duration[i] else NA_real_,
        tokens_sent = if ("sent_tokens" %in% names(w)) w$sent_tokens[i] else NA_integer_,
        tokens_received = if ("rec_tokens" %in% names(w)) w$rec_tokens[i] else NA_integer_,
        tool = NA_character_,
        request_hash = if (!is.null(rr)) rr$request_hash[[1]] else NA_character_,
        response_id = if (!is.null(rr)) rr$response_id[[1]] else NA_character_,
        note = paste0("worker:", w$approach[i]),
        meta = list(record = rr))
    }
  }
  prov$spans <- spans
  prov$participants <- tibble::tibble(
    agent_id = c("strong", "worker"),
    name = c(prov$design$strong_model %||% "strong", prov$design$cheap_model %||% "worker"),
    provider = NA_character_, model = c(prov$design$strong_model, prov$design$cheap_model),
    persona_hash = NA_character_)
  prov$agents <- list()
  .run_from_provenance(prov, artifacts = list(plan = x$plan, workers = x$workers))
}

#' @export
print.super_brain <- function(x, ...) {
  cat(sprintf("<super_brain | %d approaches | %d/%d workers ok | revised: %s>\n\n",
              nrow(x$plan), sum(x$workers$success %in% TRUE), nrow(x$workers),
              x$revised))
  cat(x$answer, "\n")
  invisible(x)
}
