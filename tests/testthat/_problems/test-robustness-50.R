# Extracted from test-robustness.R:50

# test -------------------------------------------------------------------------
captured <- NULL
run_fn <- function(cond, rep, perturb) {
    captured <<- list(
      model = perturb$config$model,
      temp = perturb$config$model_params$temperature,
      persona = perturb$persona("BASE"),
      reordered = perturb$reorder(c("a", "b", "c")))
    "x"
  }
agent_robustness(run_fn,
    vary = list(model = c("mZ"), temperature = c("0.5"),
                persona = c("is cautious"), option_order = c("reverse")),
    measure = function(r) r,
    .config = LLMR::llm_config("groq", "fake"))
