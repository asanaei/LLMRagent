# tool_governance.R -----------------------------------------------------------
# Governed tools: an llm_tool() carrying a declared policy (side effects,
# approval requirement, call/time/size limits). A governed tool IS an llmr_tool
# (same class, same fields), so agent(tools=) and LLMR::call_llm_tools() consume
# it unchanged; a plain llm_tool() degrades to an unguarded external tool. The
# policy is part of a study's identity (hash_tool_spec reads it), and every
# invocation is enforced and recorded.

#' Define a governed tool
#'
#' Like [LLMR::llm_tool()], but the tool also declares how it behaves as a
#' research instrument: whether it reads, writes, or reaches outside the
#' session; whether a human must approve each call; and hard per-tool limits on
#' the number of calls, wall-clock time, and result size. The returned object is
#' an ordinary `llmr_tool`, so it passes to [agent()] and the tool loop exactly
#' as a plain tool does; the governance is carried alongside and enforced on
#' every call.
#'
#' Governance is provenance. A tool's policy is folded into the study manifest
#' via [hash_tool_spec()], so tightening a limit (a different apparatus) changes
#' the manifest hash. Each invocation is recorded with the hashes of its
#' arguments and result, its status, and its duration, surfaced at the `"tool"`
#' level of [as_agent_run()].
#'
#' @param fn The R function to expose. Called with the model's arguments by
#'   name, exactly as in [LLMR::llm_tool()].
#' @param name Tool name shown to the model.
#' @param description One or two sentences for the model.
#' @param parameters A named list of JSON-Schema properties, or a full schema
#'   object (as in [LLMR::llm_tool()]).
#' @param required Character vector of required argument names.
#' @param side_effects What the tool does in the world: `"read"` (default),
#'   `"write"`, `"external"` (reaches a network or service), or `"none"`.
#' @param requires_approval If `TRUE`, each call pauses for human sign-off (see
#'   [human_gate()]); the agent then runs a controllable tool loop so the pause
#'   is possible. Default `FALSE`.
#' @param timeout_s Optional wall-clock limit per call (seconds). Timed calls
#'   run in a `callr` child process, which is terminated at the limit.
#' @param max_calls Maximum times this **tool object** may run (`Inf` by
#'   default). A call beyond the limit is refused (the model is told, not the
#'   tool executed). The counter lives in the tool object, so it is shared if
#'   the same `agent_tool()` object is reused across agents or experiment cells;
#'   build a fresh tool per independent cell (as you would a fresh agent) when
#'   each cell should get its own budget.
#' @param max_bytes Maximum result size in bytes (as measured by
#'   `nchar(type = "bytes")`); a larger result is truncated at a character
#'   boundary within the cap and flagged.
#' @return An object of class `agent_tool` and `llmr_tool`, with its governance
#'   policy in the ordinary `governance` field.
#' @seealso [LLMR::llm_tool()], [hash_tool_spec()], [guardrail()], [human_gate()]
#' @examples
#' lookup <- agent_tool(
#'   function(city) paste0("22C in ", city),
#'   name = "get_weather", description = "Current weather for a city.",
#'   parameters = list(city = list(type = "string")),
#'   side_effects = "external", max_calls = 5
#' )
#' @export
agent_tool <- function(fn, name, description, parameters = NULL, required = NULL,
                       side_effects = c("read", "write", "external", "none"),
                       requires_approval = FALSE,
                       timeout_s = NULL, max_calls = Inf, max_bytes = Inf) {
  stopifnot(is.function(fn))
  side_effects <- match.arg(side_effects)
  if (!is.null(timeout_s) && is.null(.agent_tool_timeout_backend())) {
    stop("`timeout_s` requires the 'callr' package.", call. = FALSE)
  }
  state <- new.env(parent = emptyenv())
  state$n_calls <- 0L
  gov <- list(side_effects = side_effects,
              requires_approval = isTRUE(requires_approval),
              timeout_s = timeout_s, max_calls = max_calls,
              max_bytes = max_bytes, state = state, raw_fn = fn)

  truncate_bytes <- function(x, limit) {
    limit <- max(0L, as.integer(floor(limit)))
    if (!limit || !nzchar(x)) return("")
    keep <- substr(x, 1L, min(nchar(x), limit))
    while (nchar(keep, type = "bytes") > limit) {
      chars <- nchar(keep)
      next_chars <- floor(chars * limit / nchar(keep, type = "bytes"))
      if (next_chars >= chars) next_chars <- chars - 1L
      keep <- if (next_chars > 0L) substr(keep, 1L, next_chars) else ""
    }
    keep
  }

  wrapped <- function(...) {
    args <- list(...)
    state$n_calls <- state$n_calls + 1L
    if (state$n_calls > max_calls) {
      return(sprintf("BLOCKED: tool '%s' exceeded max_calls = %s.", name,
                     format(max_calls)))
    }
    run_one <- function() do.call(fn, args)
    res <- if (!is.null(timeout_s)) {
      tryCatch(
        callr::r(
          function(fn, args) do.call(fn, args),
          args = list(fn = fn, args = args), timeout = timeout_s,
          spinner = FALSE),
        callr_timeout_error = function(e)
          sprintf("BLOCKED: tool '%s' timed out after %ss.", name, timeout_s),
        error = function(e) paste0("ERROR: ", conditionMessage(e)))
    } else {
      tryCatch(run_one(), error = function(e) paste0("ERROR: ", conditionMessage(e)))
    }
    out <- if (is.character(res) && length(res) == 1L) res else
      tryCatch(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")),
               error = function(e) paste(utils::capture.output(print(res)), collapse = "\n"))
    if (is.finite(max_bytes) && nchar(out, type = "bytes") > max_bytes) {
      limit <- max(0L, as.integer(floor(max_bytes)))
      marker <- truncate_bytes(" [truncated]", limit)
      keep <- truncate_bytes(out, limit - nchar(marker, type = "bytes"))
      out <- paste0(keep, marker)
    }
    out
  }

  tool <- LLMR::llm_tool(wrapped, name = name, description = description,
                         parameters = parameters, required = required)
  tool$governance <- gov
  class(tool) <- unique(c("agent_tool", class(tool)))
  tool
}

#' @keywords internal
#' @noRd
.agent_tool_timeout_backend <- function() {
  if (requireNamespace("callr", quietly = TRUE)) "callr" else NULL
}

# Internal: the governance record for a tool (or a default for a plain
# llm_tool, which is treated as an unguarded external tool).
#' @keywords internal
#' @noRd
.tool_governance <- function(tool) {
  gov <- tool$governance %||% attr(tool, "governance")
  if (is.null(gov)) {
    return(list(side_effects = "external", requires_approval = FALSE,
                timeout_s = NULL, max_calls = Inf, max_bytes = Inf))
  }
  gov
}

# Internal: does an agent (or a tool list) carry any approval-gated tool? When
# it does, the Agent must run a controllable tool loop so a pause is possible.
#' @keywords internal
#' @noRd
.has_gated_tool <- function(tools) {
  if (inherits(tools, "llmr_tool")) tools <- list(tools)
  any(vapply(tools, function(t) isTRUE(.tool_governance(t)$requires_approval),
             logical(1)))
}
