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
#' @param timeout_s Optional wall-clock limit per call (seconds); needs the
#'   `R.utils` package to enforce, otherwise it is recorded but not enforced.
#' @param max_calls Maximum times this **tool object** may run (`Inf` by
#'   default). A call beyond the limit is refused (the model is told, not the
#'   tool executed). The counter lives in the tool object, so it is shared if
#'   the same `agent_tool()` object is reused across agents or experiment cells;
#'   build a fresh tool per independent cell (as you would a fresh agent) when
#'   each cell should get its own budget.
#' @param max_bytes Maximum result size in bytes (as measured by
#'   `nchar(type = "bytes")`); a larger result is truncated at a character
#'   boundary within the cap and flagged.
#' @return An `llmr_tool` carrying a `"governance"` attribute.
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
  state <- new.env(parent = emptyenv())
  state$n_calls <- 0L
  gov <- list(side_effects = side_effects,
              requires_approval = isTRUE(requires_approval),
              timeout_s = timeout_s, max_calls = max_calls,
              max_bytes = max_bytes, state = state, raw_fn = fn)

  wrapped <- function(...) {
    args <- list(...)
    state$n_calls <- state$n_calls + 1L
    if (state$n_calls > max_calls) {
      return(sprintf("BLOCKED: tool '%s' exceeded max_calls = %s.", name,
                     format(max_calls)))
    }
    run_one <- function() do.call(fn, args)
    res <- if (!is.null(timeout_s) && requireNamespace("R.utils", quietly = TRUE)) {
      tryCatch(
        R.utils::withTimeout(run_one(), timeout = timeout_s, onTimeout = "error"),
        TimeoutException = function(e)
          sprintf("BLOCKED: tool '%s' timed out after %ss.", name, timeout_s),
        error = function(e) paste0("ERROR: ", conditionMessage(e)))
    } else {
      tryCatch(run_one(), error = function(e) paste0("ERROR: ", conditionMessage(e)))
    }
    out <- if (is.character(res) && length(res) == 1L) res else
      tryCatch(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")),
               error = function(e) paste(utils::capture.output(print(res)), collapse = "\n"))
    if (is.finite(max_bytes) && nchar(out, type = "bytes") > max_bytes) {
      # The cap is in BYTES; substr() counts characters, so a multibyte result
      # is shrunk proportionally until the kept text fits within the byte cap
      # (converging in a few steps, always on a character boundary).
      keep <- substr(out, 1L, max_bytes)
      while (nchar(keep, type = "bytes") > max_bytes) {
        keep <- substr(keep, 1L,
                       floor(nchar(keep) * max_bytes / nchar(keep, type = "bytes")))
      }
      out <- paste0(keep, sprintf(" ...[truncated to %s bytes]", format(max_bytes)))
    }
    out
  }

  tool <- LLMR::llm_tool(wrapped, name = name, description = description,
                         parameters = parameters, required = required)
  attr(tool, "governance") <- gov
  tool
}

# Internal: the governance record for a tool (or a default for a plain
# llm_tool, which is treated as an unguarded external tool).
#' @keywords internal
#' @noRd
.tool_governance <- function(tool) {
  gov <- attr(tool, "governance")
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
