# delegation.R ------------------------------------------------------------------
# Agents calling agents. agent_as_tool() turns an agent into an LLMR tool, so
# any other agent can consult it the way it would call an R function. This is
# the primitive behind supervisor/specialist hierarchies.

#' Expose an agent as a tool for other agents
#'
#' Turns an [Agent] into an `LLMR::llm_tool()`, so other agents can delegate
#' to it. A supervisor with specialist sub-agents in its `tools` decides for
#' itself when to consult whom; the specialist's reply comes back as the tool
#' result, and the supervisor continues with it.
#'
#' Three properties make delegation safe and auditable:
#'
#' - **Spend is attributed.** A consultation runs through the specialist's own
#'   machinery, so its `usage()` and `trace()` record the work it did, while
#'   the supervisor's trace records the tool call.
#' - **Budgets nest.** Give the specialist its own [budget()]; when it is
#'   exhausted, the supervisor receives the budget error as the tool result
#'   (an `"ERROR: ..."` string) and can carry on without it.
#' - **Consultations are stateless.** Each delegated question goes through
#'   `reply()`: the specialist's persona applies, but nothing is written to
#'   its memory, so concurrent supervisors cannot contaminate each other.
#'
#' @param x The [Agent] to expose.
#' @param name Tool name shown to the calling model. Default `ask_<name>`,
#'   lower-cased and sanitized.
#' @param description What the calling model is told about this specialist.
#'   Defaults to the first sentence of the persona, prefixed by the agent's
#'   name. Write this the way you would brief a colleague: what the
#'   specialist knows and when to consult it.
#' @return An `LLMR::llm_tool()` object, ready for `agent(tools = ...)`.
#' @examples
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b", temperature = 0.3)
#'
#' statistician <- agent("Stat", cfg,
#'   persona = "A PhD statistician. Precise about assumptions and pitfalls.",
#'   budget  = budget(max_calls = 5))   # the specialist has its own ceiling
#' historian <- agent("Hist", cfg,
#'   persona = "An economic historian. Strong on institutional context.")
#'
#' supervisor <- agent("Lead", cfg,
#'   persona = "A research lead. Consult your specialists, then synthesize.",
#'   tools = list(agent_as_tool(statistician), agent_as_tool(historian)))
#'
#' supervisor$chat(
#'   "We observe falling crime and rising policing budgets across cities.
#'    What would it take to argue causality here?")
#' statistician$usage()   # the consultation is on the specialist's meter
#' }
#' @seealso [agent()], [agent_pipeline()], [agent_fanout_synthesis()]
#' @export
agent_as_tool <- function(x, name = NULL, description = NULL) {
  stopifnot(inherits(x, "Agent"))
  nm <- name %||% paste0("ask_", gsub("[^a-z0-9_]+", "_", tolower(x$name)))
  desc <- description %||% {
    brief <- if (is.null(x$persona)) "" else {
      first <- sub("([.!?]).*$", "\\1", trimws(x$persona))
      paste0(" ", first)
    }
    paste0("Consult ", x$name, ".", brief,
           " Send one complete, self-contained question;",
           " it sees nothing but your message.")
  }
  tool <- LLMR::llm_tool(
    function(question) x$reply(as.character(question)[1]),
    name = nm,
    description = desc,
    parameters = list(question = list(
      type = "string",
      description = "A complete, self-contained question or task."))
  )
  # Link the specialist so a run that includes the supervisor also binds the
  # specialist: its consultation spans then nest in the same run's event graph.
  attr(tool, "delegate_agent") <- x
  tool
}

# Internal: collect the specialist agents reachable through an agent's
# delegate-tools (one level), so a run can bind them alongside the supervisor.
#' @keywords internal
#' @noRd
.delegate_agents <- function(agent) {
  tls <- tryCatch(agent$tools, error = function(e) list())
  out <- list()
  for (t in tls) {
    da <- attr(t, "delegate_agent")
    if (inherits(da, "Agent")) out[[length(out) + 1L]] <- da
  }
  out
}
