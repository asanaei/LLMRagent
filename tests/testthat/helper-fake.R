# Offline test doubles: fake llmr_response objects, a scripted caller for the
# Agent seam, and a namespace stubber for LLMR functions used directly.

fake_response <- function(text = "ok", sent = 10L, rec = 5L,
                          finish = "stop", tool_history = NULL) {
  out <- structure(list(
    text = text, provider = "fake", model = "fake-1",
    model_version = "fake-1-v", finish_reason = finish,
    usage = list(sent = sent, rec = rec, total = sent + rec,
                 reasoning = NA_integer_, cached = NA_integer_),
    thinking = NA_character_, response_id = "fr-1", duration_s = 0.01,
    raw = list(), raw_json = "{}"
  ), class = "llmr_response")
  if (!is.null(tool_history)) attr(out, "tool_history") <- tool_history
  out
}

# A caller that returns scripted replies in order (recycling the last).
scripted_caller <- function(replies) {
  i <- 0L
  function(config, messages, tools, ...) {
    i <<- i + 1L
    r <- replies[[min(i, length(replies))]]
    if (is.character(r)) fake_response(r) else r
  }
}

fake_agent <- function(name, replies, persona = NULL, ...) {
  cfg <- LLMR::llm_config("groq", "fake-model")
  Agent$new(name = name, config = cfg, persona = persona,
            caller = scripted_caller(replies), quiet = TRUE, ...)
}

# Temporarily replace a function inside the LLMR namespace.
with_stub_llmr <- function(name, stub, expr) {
  ns <- asNamespace("LLMR")
  orig <- get(name, ns)
  unlockBinding(name, ns)
  assign(name, stub, ns)
  on.exit({
    assign(name, orig, ns)
    lockBinding(name, ns)
  }, add = TRUE)
  force(expr)
}
