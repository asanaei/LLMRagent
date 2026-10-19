# Stage 3: the robustness battery. Offline via fake_agent cells.

test_that("single-config public helpers use the config formal", {
  helpers <- list(persona_variants, persona_audit, vary_prompt,
                  agent_robustness)
  expect_true(all(vapply(helpers, function(f) "config" %in% names(formals(f)),
                         logical(1))))
  expect_false(any(vapply(helpers, function(f) ".config" %in% names(formals(f)),
                          logical(1))))
})

test_that("agent_robustness varies an axis and summarizes by axis", {
  # a run_fn that returns a value depending on the temperature axis, so we can
  # see instability appear deterministically. perturb$config carries the temp.
  run_fn <- function(cond, rep, perturb) {
    # measure depends on the cell's temperature level
    if (identical(as.character(cond$temperature), "0")) "yes" else "no"
  }
  batt <- agent_robustness(
    run_fn = run_fn,
    vary = list(temperature = c("0", "1")),
    measure = function(r) r,
    config = LLMR::llm_config("groq", "fake")
  )
  expect_s3_class(batt, "agent_robustness")
  expect_true(all(c("axis", "level", "instability", "dispersion",
                    "agreement_alpha", "flips_vs_baseline") %in% names(batt$by_axis)))
  # two temperature levels -> two by_axis rows for the temperature axis
  expect_equal(sum(batt$by_axis$axis == "temperature"), 2L)
  # the non-baseline level differs from baseline -> instability > 0 there
  nonbase <- batt$by_axis[batt$by_axis$level == "1", ]
  expect_gt(nonbase$instability, 0)
})

test_that("a 2-argument run_fn still runs (perturb optional)", {
  run_fn <- function(cond, rep) "stable"   # ignores perturbation
  batt <- agent_robustness(run_fn, vary = list(model = c("m1", "m2")),
                           measure = function(r) r,
                           config = LLMR::llm_config("groq", "fake"))
  # identical measure across all cells -> zero instability
  expect_equal(max(batt$by_axis$instability), 0)
  expect_false(batt$overall$fragile)
})

test_that("the perturb closure applies model/temperature/persona/order", {
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
    config = LLMR::llm_config("groq", "fake"))
  expect_identical(captured$model, "mZ")
  expect_equal(captured$temp, 0.5)
  expect_match(captured$persona, "BASE")
  expect_match(captured$persona, "is cautious")
  expect_identical(captured$reordered, c("c", "b", "a"))   # reversed
})

test_that("a random option order without a seed follows the session's RNG stream", {
  opts <- letters[1:10]
  random_orders <- function(reps = 1L) {
    got <- list()
    agent_robustness(function(cond, rep, perturb) {
      got[[length(got) + 1L]] <<- perturb$reorder(opts)
      "x"
    },
    reps = reps,
    vary = list(option_order = vary_option_order("random")),
    measure = function(r) r,
    config = LLMR::llm_config("groq", "fake"))
    got
  }
  expect_null(vary_option_order("random")$seed)

  set.seed(4242)
  first <- random_orders()[[1]]
  set.seed(4242)
  expect_identical(random_orders()[[1]], first)
  expect_setequal(first, opts)
  # The old default seeded every random order with 110, so the session's seed
  # made no difference; now a different session seed gives a different order.
  set.seed(4243)
  expect_false(identical(random_orders()[[1]], first))
  # Each draw is fresh, so two replicates see different permutations.
  set.seed(4242)
  two <- random_orders(reps = 2L)
  expect_length(two, 2L)
  expect_false(identical(two[[1]], two[[2]]))

  # The permutation is the session's own draw, and it advances the stream.
  inside <- NULL
  agent_robustness(function(cond, rep, perturb) {
    set.seed(4242)
    before <- get(".Random.seed", envir = globalenv())
    o <- perturb$reorder(opts)
    inside <<- list(order = o, advanced = !identical(
      before, get(".Random.seed", envir = globalenv())))
    "x"
  },
  vary = list(option_order = vary_option_order("random")),
  measure = function(r) r,
  config = LLMR::llm_config("groq", "fake"))
  set.seed(4242)
  expect_identical(inside$order, sample(opts))
  expect_true(inside$advanced)
})

test_that("a supplied seed fixes the random order and leaves .Random.seed untouched", {
  opts <- letters[1:10]
  captured <- list()
  run_fn <- function(cond, rep, perturb) {
    before <- get(".Random.seed", envir = globalenv())
    o <- perturb$reorder(opts)
    captured[[length(captured) + 1L]] <<- list(
      order = o,
      untouched = identical(before, get(".Random.seed", envir = globalenv())))
    "x"
  }
  for (session_seed in c(1, 2)) {
    set.seed(session_seed)
    agent_robustness(run_fn, reps = 2L,
      vary = list(option_order = vary_option_order("random", seed = 110)),
      measure = function(r) r,
      config = LLMR::llm_config("groq", "fake"))
  }
  set.seed(110)
  expected <- sample(opts)
  expect_length(captured, 4L)
  expect_true(all(vapply(captured, `[[`, logical(1), "untouched")))
  expect_true(all(vapply(captured, function(x) identical(x$order, expected),
                         logical(1))))
})

test_that("diagnostics(agent_robustness) returns the overall summary", {
  run_fn <- function(cond, rep) "z"
  batt <- agent_robustness(run_fn, vary = list(model = c("a", "b")),
                           measure = function(r) r,
                           config = LLMR::llm_config("groq", "fake"))
  d <- diagnostics(batt)
  expect_true(all(c("n_cells", "failure_rate", "worst_axis", "fragile") %in% names(d)))
  expect_equal(nrow(d), 1L)
})

test_that("vary_persona accepts a persona_set", {
  base <- persona_frame("A voter.", source = "synthetic")
  pset <- persona_variants(base, vary = list(mood = c("calm", "angry")))
  run_fn <- function(cond, rep, perturb) {
    p <- perturb$persona("BASE")
    # the persona variant should be a persona_frame from the set
    if (inherits(p, "persona_frame")) "frame" else "string"
  }
  batt <- agent_robustness(run_fn, vary = list(persona = vary_persona(pset)),
                           measure = function(r) r,
                           config = LLMR::llm_config("groq", "fake"))
  expect_true(all(batt$cells$measure_value == "frame"))
})

test_that("a numeric measure gets an interval agreement alpha (LLMR >= 0.8.9)", {
  # measure depends on temperature, paired by 'block'; two levels per axis so a
  # cross-level alpha is defined. The numeric measure routes to interval alpha.
  run_fn <- function(cond, rep) if (as.character(cond$temperature) == "0") 0.9 else 0.2
  batt <- agent_robustness(run_fn,
    vary = list(temperature = c("0", "1"), block = c("a", "b")),
    measure = function(r) r, config = LLMR::llm_config("groq", "fake"))
  ta <- batt$by_axis$agreement_alpha[batt$by_axis$axis == "temperature"]
  expect_false(any(is.na(ta)))          # interval alpha computed, not skipped
  expect_true(is.numeric(ta))
})
