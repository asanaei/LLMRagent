# robustness.R ----------------------------------------------------------------
# A robustness battery: run a procedure across perturbations of prompt wording,
# persona, option order, model, and temperature, and summarize how stable the
# result is by axis. Robustness is treated as a standard validity diagnostic,
# not an afterthought. The battery does not reimplement an engine: it builds the
# perturbed design and dispatches to agent_experiment() (whole-procedure cells)
# while reusing LLMR's reliability statistics. The researcher writes the
# procedure once; the axes are applied to the inputs through a single `perturb`
# argument, so no per-perturbation hand-coding is needed.

# ---- axis constructors ------------------------------------------------------

#' Robustness perturbation axes
#'
#' Helpers that declare a perturbation axis for [agent_robustness()]. Each
#' returns a small spec the battery expands into design cells and applies to the
#' inputs through the `perturb` argument of your `run_fn`. Bare vectors work too
#' (`vary = list(model = c("a","b"))`).
#'
#' @param ... For `vary_models`/`vary_temperature`, the levels (model names or
#'   temperatures). For `vary_prompt`, either named template strings or
#'   `paraphrase = n` with `config =` to generate `n` paraphrases. For
#'   `vary_persona`, persona variants (strings or a `persona_set`). For
#'   `vary_option_order`, the orders (`"as_is"`, `"reverse"`, `"random"`).
#' @param paraphrase For `vary_prompt`: number of paraphrases to generate.
#' @param config For `vary_prompt(paraphrase=)`: a generative config used once
#'   to draft the paraphrases (hashed into the manifest).
#' @param seed For `vary_option_order`: RNG seed for the random permutation.
#' @return An `agent_axis` object.
#' @name robustness-axes
#' @seealso [agent_robustness()]
NULL

#' @rdname robustness-axes
#' @export
vary_models <- function(...) .axis("model", c(...))

#' @rdname robustness-axes
#' @export
vary_temperature <- function(...) .axis("temperature", c(...))

#' @rdname robustness-axes
#' @param prompt For `vary_prompt(paraphrase=)`: the actual prompt text to
#'   paraphrase. The paraphrase set is generated once at build time and is the
#'   axis's levels (including the original as the baseline), so `run_fn`'s
#'   `perturb$prompt(x)` returns the cell's paraphrase, not a placeholder.
#' @export
vary_prompt <- function(..., prompt = NULL, paraphrase = NULL, config = NULL) {
  if (!is.null(paraphrase)) {
    if (is.null(prompt) || !nzchar(prompt)) {
      stop("vary_prompt(paraphrase=) needs `prompt =` (the actual prompt text ",
           "to paraphrase) and `config =` (a generative llm_config).",
           call. = FALSE)
    }
    return(structure(list(axis = "prompt", mode = "paraphrase",
                          n = as.integer(paraphrase), prompt = prompt,
                          config = config),
                     class = "agent_axis"))
  }
  templates <- c(...)
  structure(list(axis = "prompt", mode = "templates", levels = templates),
            class = "agent_axis")
}

#' @rdname robustness-axes
#' @export
vary_persona <- function(...) {
  v <- list(...)
  if (length(v) == 1L && inherits(v[[1]], "persona_set")) {
    return(structure(list(axis = "persona", mode = "set", set = v[[1]]),
                     class = "agent_axis"))
  }
  structure(list(axis = "persona", mode = "levels", levels = unlist(v)),
            class = "agent_axis")
}

#' @rdname robustness-axes
#' @export
vary_option_order <- function(..., seed = 110) {
  orders <- c(...)
  if (!length(orders)) orders <- c("as_is", "reverse")
  structure(list(axis = "option_order", levels = orders, seed = seed),
            class = "agent_axis")
}

#' @keywords internal
#' @noRd
.axis <- function(name, levels) {
  structure(list(axis = name, mode = "levels", levels = levels),
            class = "agent_axis")
}

# Normalize a vary= list entry (a bare vector or an agent_axis) to an axis spec.
#' @keywords internal
#' @noRd
.as_axis <- function(name, value) {
  if (inherits(value, "agent_axis")) return(value)
  .axis(name, value)
}

# The levels of an axis as a simple labeled vector (for the design grid).
#' @keywords internal
#' @noRd
.axis_levels <- function(ax) {
  switch(ax$mode %||% "levels",
    levels    = ax$levels,
    templates = if (!is.null(names(ax$levels))) names(ax$levels) else ax$levels,
    set       = ax$set$id,
    paraphrase = paste0("paraphrase_", seq_len(ax$n)),
    ax$levels)
}

# ---- the battery ------------------------------------------------------------

#' Run a robustness battery
#'
#' Runs a procedure across the cross-product of perturbation axes and reports,
#' by axis, how much the result moves. The procedure is your `run_fn`; the
#' perturbations reach it through an optional third argument, `perturb`, so you
#' write the procedure once and the battery varies its inputs.
#'
#' `run_fn` may be `function(cond, rep)` (the existing [agent_experiment()]
#' contract) or `function(cond, rep, perturb)`. `perturb` is a list with
#' `config` (the base config with this cell's `model`/`temperature` applied),
#' `persona(x)` (apply this cell's persona variant to a base persona), `prompt(x)`
#' (apply this cell's prompt variant), and `reorder(options)` (apply this cell's
#' option permutation). A two-argument `run_fn` still runs; then only the
#' `model`/`temperature` axes affect the run.
#'
#' @param run_fn The procedure, `function(cond, rep[, perturb])`, returning a
#'   result whose stability is assessed via `measure`.
#' @param design Optional baseline conditions (a data frame); one empty
#'   condition if `NULL`.
#' @param reps Replications per cell.
#' @param vary A named list of axes: bare level vectors or axis specs from
#'   [vary_models()] and related helpers.
#' @param measure A function `result -> scalar` (or a field name) producing the
#'   quantity whose stability is assessed. If `NULL`, the result is used when it
#'   is already scalar.
#' @param baseline Which level of each axis is the reference (`"first"`).
#' @param config A base config (used to build `perturb$config`).
#' @param parallel Passed to [agent_experiment()].
#' @param quiet Passed to [agent_experiment()].
#' @param ... Passed to [agent_experiment()].
#' @return An object of class `agent_robustness`: a list with `cells` (the full
#'   perturbed design with `measure_value`), `by_axis` (one row per axis-level
#'   with `instability`, `dispersion`, `agreement_alpha`, `failure_rate`,
#'   `flips_vs_baseline`, `delta_mean`), and `overall` (a `fragile` flag).
#' @seealso [agent_experiment()], [LLMR::llm_replicate()], [LLMR::llm_agreement()]
#' @examples
#' \dontrun{
#' batt <- agent_robustness(
#'   run_fn = function(cond, rep, perturb) {
#'     a <- agent("S", perturb$config, persona = perturb$persona("A cautious voter."))
#'     a$reply(perturb$prompt("Do you support the policy? yes/no."))
#'   },
#'   vary = list(temperature = c(0, 1), model = c("openai/gpt-oss-20b")),
#'   measure = function(r) tolower(trimws(r))
#' )
#' batt$by_axis
#' }
#' @export
agent_robustness <- function(run_fn, design = NULL, reps = 1L, vary = list(),
                             measure = NULL, baseline = "first",
                             config = NULL, parallel = FALSE, quiet = TRUE, ...) {
  stopifnot(is.function(run_fn))
  if (!length(vary)) stop("Provide at least one axis in `vary`.", call. = FALSE)

  axes <- stats::setNames(
    lapply(names(vary), function(nm) .as_axis(nm, vary[[nm]])), names(vary))

  # Resolve paraphrase axes once (a single generative call), so the paraphrase
  # set is fixed and reproducible (and would be hashed into a run manifest).
  axes <- lapply(axes, function(ax) {
    if (identical(ax$axis, "prompt") && identical(ax$mode, "paraphrase")) {
      # Paraphrase the ACTUAL prompt, and keep the original as the baseline
      # level, so perturb$prompt() returns a real paraphrase of the real prompt.
      paras <- .generate_paraphrases(ax$prompt, ax$n, ax$config)
      lv <- c(original = ax$prompt,
              stats::setNames(paras, paste0("paraphrase_", seq_along(paras))))
      ax$mode <- "templates"
      ax$levels <- lv
    }
    ax
  })

  # Build the perturbation grid: one column per axis of its level labels.
  grid <- expand.grid(lapply(axes, .axis_levels), stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  names(grid) <- names(axes)

  # Cross the baseline design (if any) with the perturbation grid.
  base_design <- if (is.null(design)) data.frame(.unit = 1L) else design
  full <- merge(base_design, grid, by = character(0))  # cross join

  # Wrap run_fn so each cell receives a ready `perturb` built from its axis levels.
  wrapped <- function(cond, rep) {
    pert <- .build_perturb(cond, axes, config)
    if (length(formals(run_fn)) >= 3L) run_fn(cond, rep, pert)
    else run_fn(cond, rep)
  }

  exp <- agent_experiment(full, wrapped, reps = reps, parallel = parallel,
                          quiet = quiet, ...)

  # Reduce each cell's result to a scalar measure.
  mv <- vapply(exp$result, function(res) {
    if (is.null(res)) return(NA_character_)
    val <- if (is.function(measure)) tryCatch(measure(res), error = function(e) NA)
           else if (is.character(measure)) tryCatch(res[[measure]], error = function(e) NA)
           else res
    if (is.null(val) || length(val) != 1L) NA_character_ else as.character(val)
  }, character(1))
  exp$measure_value <- mv

  axis_names <- names(axes)
  by_axis <- .robustness_by_axis(exp, axis_names, baseline)
  overall <- .robustness_overall(exp, by_axis)

  structure(list(cells = tibble::as_tibble(exp), by_axis = by_axis,
                 overall = overall, axes = lapply(axes, .axis_levels)),
            class = "agent_robustness")
}

# Build the per-cell `perturb` helper bundle from the cell's axis levels.
#' @keywords internal
#' @noRd
.build_perturb <- function(cond, axes, base_config) {
  cfg <- base_config
  # model / temperature overrides
  if ("model" %in% names(cond) && !is.null(cfg)) cfg$model <- as.character(cond$model)
  if ("temperature" %in% names(cond) && !is.null(cfg)) {
    cfg$model_params <- cfg$model_params %||% list()
    cfg$model_params$temperature <- as.numeric(cond$temperature)
  }
  prompt_fn <- function(x) {
    ax <- axes$prompt
    if (is.null(ax)) return(x)
    lvl <- as.character(cond$prompt)
    tmpl <- if (!is.null(names(ax$levels))) ax$levels[[lvl]] else lvl
    if (is.null(tmpl) || is.na(tmpl)) x else tmpl
  }
  persona_fn <- function(x) {
    ax <- axes$persona
    if (is.null(ax)) return(x)
    lvl <- as.character(cond$persona)
    if (identical(ax$mode, "set")) {
      i <- match(lvl, ax$set$id)
      if (!is.na(i)) return(ax$set$persona[[i]])
      return(x)
    }
    # a level string: append it to the base persona, non-essentializing
    if (nzchar(lvl)) paste0(x, "\n\n", lvl) else x
  }
  reorder_fn <- function(options) {
    ax <- axes$option_order
    if (is.null(ax)) return(options)
    lvl <- as.character(cond$option_order)
    switch(lvl,
      reverse = rev(options),
      # Reproducible permutation WITHOUT disturbing the caller's RNG: snapshot
      # and restore .Random.seed around the local set.seed (set.seed otherwise
      # clobbers global RNG state, a documented footgun).
      random  = .with_local_seed(ax$seed %||% 110, function() sample(options)),
      options)
  }
  list(config = cfg, prompt = prompt_fn, persona = persona_fn, reorder = reorder_fn)
}

# One row per (axis, level): instability and dispersion of the measure.
#' @keywords internal
#' @noRd
.robustness_by_axis <- function(exp, axis_names, baseline) {
  rows <- list()
  ok <- is.na(exp$error)
  mv <- exp$measure_value
  is_num <- suppressWarnings(!all(is.na(as.numeric(mv[ok & !is.na(mv)]))))
  for (ax in axis_names) {
    levs <- unique(exp[[ax]])
    base_lev <- if (identical(baseline, "first")) levs[1] else baseline
    base_vals <- mv[exp[[ax]] == base_lev & ok]
    base_mode <- .mode_value(base_vals)
    for (lv in levs) {
      sel <- exp[[ax]] == lv
      vals <- mv[sel & ok]
      n <- sum(sel)
      n_fail <- sum(sel & !ok)
      disp <- if (is_num) stats::sd(suppressWarnings(as.numeric(vals)), na.rm = TRUE)
              else .norm_entropy(vals)
      flips <- sum(vals != base_mode & !is.na(vals))
      instab <- if (length(vals)) mean(vals != base_mode, na.rm = TRUE) else NA_real_
      delta <- if (is_num) mean(suppressWarnings(as.numeric(vals)), na.rm = TRUE) -
                 mean(suppressWarnings(as.numeric(base_vals)), na.rm = TRUE) else NA_real_
      rows[[length(rows) + 1L]] <- tibble::tibble(
        axis = ax, level = as.character(lv), n = n, n_failed = n_fail,
        failure_rate = if (n) n_fail / n else NA_real_,
        instability = instab, dispersion = disp,
        agreement_alpha = NA_real_,  # filled below across the axis
        flips_vs_baseline = flips, delta_mean = delta)
    }
    # axis-level agreement across its levels: lay each level's per-unit measure
    # into a column and compute Krippendorff's alpha across columns (interval
    # for a numeric measure, nominal otherwise).
    rows <- .fill_axis_alpha(rows, exp, ax, ok, mv, is_num = is_num)
  }
  do.call(rbind, rows)
}

# Compute one Krippendorff alpha for an axis (agreement of the measure across
# that axis's levels, paired by the other design columns) and write it into the
# axis's rows.
#' @keywords internal
#' @noRd
.fill_axis_alpha <- function(rows, exp, ax, ok, mv, is_num = FALSE) {
  alpha <- tryCatch({
    other <- setdiff(names(exp), c(ax, "result", "error", "duration", "rep",
                                   "measure_value", ".unit"))
    df <- exp[ok, , drop = FALSE]
    # A numeric measure gets an interval Krippendorff alpha (LLMR >= 0.8.9);
    # a categorical one stays nominal. Numeric values are kept as numbers so
    # interval distances are meaningful.
    df$.m <- if (isTRUE(is_num)) suppressWarnings(as.numeric(mv[ok])) else mv[ok]
    if (!length(other)) {
      # No pairing key beyond this axis (e.g. a single-axis design): the
      # within-axis-level replicate index is the unit, so each (unit, level)
      # cell is unique and reshape() does not warn.
      df$.unit_key <- as.character(stats::ave(seq_len(nrow(df)), df[[ax]],
                                              FUN = seq_along))
    } else {
      df$.unit_key <- apply(df[, other, drop = FALSE], 1L, paste, collapse = "|")
    }
    # Pivot to one column per axis level, one row per unit, by hand: stats::reshape
    # mangles a timevar whose values look numeric (e.g. "0"/"1" collapse into a
    # single malformed column). The first measure per (unit, level) wins.
    df <- df[!duplicated(df[, c(".unit_key", ax)]), c(".unit_key", ax, ".m"), drop = FALSE]
    levs <- unique(as.character(df[[ax]]))
    units <- unique(df$.unit_key)
    # A single level (or no units) gives no cross-level agreement to compute.
    # NB: do NOT return() here -- this runs inside tryCatch() and a return would
    # escape the whole .fill_axis_alpha() function (returning a scalar instead of
    # the rows list). Yield NA_real_ as the tryCatch value instead.
    if (length(levs) < 2L || !length(units)) {
      NA_real_
    } else {
      wide <- as.data.frame(lapply(levs, function(lv) {
        sub <- df[as.character(df[[ax]]) == lv, , drop = FALSE]
        sub$.m[match(units, sub$.unit_key)]
      }), stringsAsFactors = FALSE)
      names(wide) <- paste0("lvl_", seq_along(levs))
      ag <- LLMR::llm_agreement(wide, cols = names(wide),
                                metric = if (isTRUE(is_num)) "interval" else "nominal")
      ag$summary$krippendorff_alpha
    }
  }, error = function(e) NA_real_)
  for (i in seq_along(rows)) if (identical(rows[[i]]$axis, ax)) rows[[i]]$agreement_alpha <- alpha
  rows
}

#' @keywords internal
#' @noRd
.robustness_overall <- function(exp, by_axis) {
  worst <- if (nrow(by_axis)) by_axis$axis[which.max(by_axis$instability)] else NA_character_
  tibble::tibble(
    n_cells = nrow(exp),
    failure_rate = mean(!is.na(exp$error)),
    worst_axis = worst,
    max_instability = suppressWarnings(max(by_axis$instability, na.rm = TRUE)),
    min_alpha = suppressWarnings(min(by_axis$agreement_alpha, na.rm = TRUE)),
    n_axes = length(unique(by_axis$axis)),
    fragile = isTRUE(suppressWarnings(max(by_axis$instability, na.rm = TRUE)) > 0.1)
  )
}

# ---- small helpers ----------------------------------------------------------

# Run `f()` under a local seed, restoring the global RNG state afterward, so a
# reproducible permutation does not clobber the caller's random stream.
#' @keywords internal
#' @noRd
.with_local_seed <- function(seed, f) {
  has_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (has_seed) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (!is.null(old)) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE))
      rm(".Random.seed", envir = globalenv())
  }, add = TRUE)
  set.seed(seed)
  f()
}

#' @keywords internal
#' @noRd
.mode_value <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return(NA_character_)
  t <- sort(table(x), decreasing = TRUE)
  names(t)[1]
}

#' @keywords internal
#' @noRd
.norm_entropy <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 2L) return(0)
  p <- prop.table(table(x))
  h <- -sum(p * log(p))
  hmax <- log(length(p))
  if (hmax == 0) 0 else as.numeric(h / hmax)
}

# Generate prompt paraphrases via one structured strong-model call. Returns the
# paraphrase strings; falls back to the empty set on failure.
#' @keywords internal
#' @noRd
.generate_paraphrases <- function(prompt, n, config) {
  if (is.null(config)) {
    stop("vary_prompt(paraphrase=) needs config (a generative llm_config).",
         call. = FALSE)
  }
  schema <- list(type = "object", properties = list(
    paraphrases = list(type = "array", items = list(type = "string"))),
    required = list("paraphrases"))
  resp <- LLMR::call_llm_robust(
    LLMR::enable_structured_output(config, schema = schema),
    c(system = paste("Produce exactly", n, "faithful paraphrases of the prompt",
                     "the user gives. Preserve its meaning and any answer format",
                     "exactly; vary only the wording. Return JSON",
                     '{"paraphrases":[...]}.'),
      user = paste0("PROMPT TO PARAPHRASE:\n", prompt)))
  pp <- tryCatch(LLMR::llm_parse_structured(resp)$paraphrases, error = function(e) NULL)
  if (is.null(pp) || !length(pp)) character(0) else
    vapply(pp, as.character, character(1))[seq_len(min(n, length(pp)))]
}

#' @export
print.agent_robustness <- function(x, ...) {
  o <- x$overall
  cat(sprintf("<agent_robustness | %d cell(s) | %d axes | fragile: %s>\n",
              o$n_cells, o$n_axes, o$fragile))
  cat(sprintf("  worst axis: %s | max instability: %.3f | failure rate: %.1f%%\n",
              o$worst_axis %||% "-", o$max_instability %||% NA_real_,
              100 * (o$failure_rate %||% NA_real_)))
  cat("  per-axis detail in $by_axis\n")
  invisible(x)
}

#' @exportS3Method LLMR::diagnostics agent_robustness
diagnostics.agent_robustness <- function(x, ...) x$overall

#' @export
as.data.frame.agent_robustness <- function(x, ...) as.data.frame(x$by_axis, ...)
