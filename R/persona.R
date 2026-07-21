# persona.R -------------------------------------------------------------------
# A persona as a first-class research object. A persona_frame() carries the
# brief the model actually reads plus the provenance that makes it auditable:
# where it came from, what dimensions were varied to produce it, the scope it is
# meant to hold under, and a stable content hash. persona_variants() turns one
# frame into a designed set (an enumerated factorial, or a generated batch);
# persona_audit() reads those briefs back for essentializing language and, when
# a model is supplied, for caricature. The premise is that synthetic personas
# are an apparatus to be inspected, not ground truth to be trusted: a persona is
# a prompt with a paper trail, and a set of personas is a sampling design whose
# representational failures (stereotype, out-group homogeneity) we can measure
# rather than assume away.

# `as.character` is an internal generic; its method dispatch reads the package's
# S3 method table, which `pkgload::load_all()` does not populate from the roxygen
# `@export` tag alone (unlike `print`, which dispatches fine). Register the
# method at load so a persona_frame behaves as its brief string in every context
# (dev and installed alike). `print`/`diagnostics` are belt-and-suspenders here.
#' @keywords internal
#' @noRd
.onLoad <- function(libname, pkgname) {
  ns <- asNamespace(pkgname)
  reg <- function(generic, class, fn) {
    if (exists(fn, envir = ns, inherits = FALSE)) {
      registerS3method(generic, class, get(fn, envir = ns), envir = ns)
    }
  }
  reg("as.character", "persona_frame", "as.character.persona_frame")
  reg("print", "persona_frame", "print.persona_frame")
  reg("print", "persona_set", "print.persona_set")
  reg("print", "persona_audit", "print.persona_audit")
  # diagnostics() is LLMR's generic; register the method on it so a bare
  # diagnostics(persona_audit) dispatches under load_all as well as installed.
  if (requireNamespace("LLMR", quietly = TRUE) &&
      exists("diagnostics.persona_audit", envir = ns, inherits = FALSE)) {
    registerS3method("diagnostics", "persona_audit",
                     get("diagnostics.persona_audit", envir = ns),
                     envir = asNamespace("LLMR"))
  }
  invisible()
}

#' A persona as an auditable research object
#'
#' A `persona_frame` bundles the brief a model reads (`text`, the *only* thing
#' the model sees) with the provenance that makes a synthetic persona
#' inspectable: its `source`, the `scope` conditions it is meant to hold under,
#' the `attributes` that were varied to produce it, an optional `variant_of`
#' parent hash, and a stable content `hash` (via [hash_persona()]). A frame is a
#' drop-in replacement for a plain-string persona anywhere [agent()] accepts
#' one: [as.character()] returns its `text`, and the [Agent] reads `text`
#' directly while keeping the frame for provenance.
#'
#' Provenance, not authority: a persona is a prompt with a provenance record, never a
#' claim that the model speaks for the people it sketches. Pair with
#' [persona_audit()] before trusting a brief, and with [persona_variants()] to
#' turn one frame into a designed set.
#'
#' @param text The persona brief (character scalar): who this person is, what
#'   they want, how they speak. The only field the model sees.
#' @param source Where the brief came from, e.g. `"synthetic"`,
#'   `"interview-grounded"`, `"literature"`, or `NULL` if unrecorded.
#' @param scope Optional named list of scope conditions the persona is meant to
#'   hold under (e.g. `list(country = "US", year = 2024)`); recorded as
#'   provenance, not enforced.
#' @param attributes Optional named list of the dimensions varied to produce
#'   this brief (e.g. `list(age = "52", risk = "cautious")`).
#' @param variant_of Optional parent persona hash this frame was derived from.
#' @param id Optional explicit identifier; ignored for hashing (the `hash` is
#'   always content-derived) but kept on the object when supplied.
#' @return An object of class `persona_frame`: a list with `text`, `source`,
#'   `scope`, `attributes`, `variant_of`, `id`, and `hash`.
#' @seealso [persona_variants()], [persona_audit()], [hash_persona()], [agent()]
#' @examples
#' p <- persona_frame(
#'   "A retired schoolteacher who reads the local paper and distrusts polls.",
#'   source = "synthetic",
#'   scope = list(country = "US"))
#' print(p)
#' as.character(p)            # the brief, usable wherever a string persona is
#' \dontrun{
#' cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' a <- agent("Voter", cfg, persona = p)   # the frame is accepted directly
#' a$persona_frame()$hash                  # provenance is preserved
#' }
#' @export
persona_frame <- function(text, source = NULL, scope = NULL,
                          attributes = NULL, variant_of = NULL, id = NULL) {
  stopifnot(is.character(text), length(text) == 1L)
  if (!is.null(scope) && !is.list(scope)) {
    stop("`scope` must be a named list or NULL.", call. = FALSE)
  }
  if (!is.null(attributes) && !is.list(attributes)) {
    stop("`attributes` must be a named list or NULL.", call. = FALSE)
  }
  out <- list(
    text       = text,
    source     = source,
    scope      = scope,
    attributes = attributes,
    variant_of = variant_of,
    id         = id
  )
  class(out) <- "persona_frame"
  # The hash is content-derived and reuses the exported hashing convention, so a
  # frame and a bare string with the same brief sit in the same identity space.
  out$hash <- hash_persona(out)
  out
}

#' @rdname persona_frame
#' @param x A `persona_frame`.
#' @param ... Ignored.
#' @export
print.persona_frame <- function(x, ...) {
  short <- substr(x$hash %||% "", 1L, 12L)
  cat(sprintf("<persona_frame %s | source=%s>\n",
              short, x$source %||% "?"))
  brief <- x$text %||% ""
  if (nchar(brief) > 80L) brief <- paste0(substr(brief, 1L, 80L), "...")
  cat("  ", brief, "\n", sep = "")
  if (length(x$scope)) {
    cond <- vapply(seq_along(x$scope), function(i) {
      paste0(names(x$scope)[i], "=", paste(as.character(x$scope[[i]]), collapse = "/"))
    }, character(1))
    cat("  scope: ", paste(cond, collapse = ", "), "\n", sep = "")
  }
  invisible(x)
}

#' @rdname persona_frame
#' @export
as.character.persona_frame <- function(x, ...) {
  x$text %||% ""
}

#' Vary a persona along named dimensions
#'
#' Turn one [persona_frame()] into a designed *set* of personas. Two modes:
#'
#' - **Enumerated** (default, `config = NULL`): `vary` is a named list of level
#'   vectors and the result is their full Cartesian product. Each combination is
#'   rendered by appending the varied attributes to the base brief in plain,
#'   factual language (no stereotyping copula), so the design is legible and the
#'   base text is never rewritten.
#' - **Generative** (`config` is a generative `LLMR::llm_config()` and `n` is
#'   given): one structured call asks the model for `n` individuated briefs that
#'   vary `names(vary)`, under a hard-coded anti-essentialism instruction. Use
#'   this when you want fluent, non-templated briefs; audit the result with
#'   [persona_audit()].
#'
#' Varying a demographic attribute does not license writing a stereotype. The
#' enumerated renderer states attributes flatly; the generative prompt forbids
#' caricature. Neither mode certifies the output: it produces briefs to be
#' inspected, not a population to be trusted.
#'
#' @param p A [persona_frame()] (the base).
#' @param vary A named list of the dimensions to vary. Enumerated mode reads the
#'   level vectors (e.g. `list(age = c("28", "52"), risk = c("cautious",
#'   "tolerant"))`); generative mode reads only the names.
#' @param n Number of briefs to generate (generative mode only). Ignored when
#'   enumerating.
#' @param config Optional generative `LLMR::llm_config()`. When `NULL`
#'   (default), the set is enumerated offline.
#' @return An object of class `persona_set`: a tibble with a `persona` list
#'   column of [persona_frame()] objects, an `id` column (each frame's hash), a
#'   `variant_of` column (the base hash), and one column per varied attribute.
#' @seealso [persona_frame()], [persona_audit()]
#' @examples
#' base <- persona_frame("A first-time voter in a swing district.",
#'                       source = "synthetic")
#' set <- persona_variants(base, vary = list(age = c("19", "24"),
#'                                           leaning = c("undecided", "left")))
#' set
#' set$persona[[1]]$attributes
#' \dontrun{
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' gen <- persona_variants(base, vary = list(age = NA, occupation = NA),
#'                         n = 5, config = cfg)
#' persona_audit(gen)
#' }
#' @export
persona_variants <- function(p, vary, n = NULL, config = NULL) {
  if (!inherits(p, "persona_frame")) {
    stop("`p` must be a persona_frame() (the base persona).", call. = FALSE)
  }
  if (!is.list(vary) || is.null(names(vary)) || any(!nzchar(names(vary)))) {
    stop("`vary` must be a named list of the dimensions to vary.", call. = FALSE)
  }
  attrs <- names(vary)

  if (!is.null(config)) {
    if (is.null(n) || !is.numeric(n) || length(n) != 1L || n < 1L) {
      stop("Generative mode (a non-NULL `config`) needs `n` >= 1.", call. = FALSE)
    }
    return(.persona_variants_generative(p, attrs, as.integer(n), config))
  }
  .persona_variants_enumerated(p, vary)
}

# Enumerated mode: the full Cartesian product of the supplied levels, each
# rendered by appending attributes flatly to the base brief.
#' @keywords internal
#' @noRd
.persona_variants_enumerated <- function(p, vary) {
  grid <- expand.grid(vary, stringsAsFactors = FALSE,
                      KEEP.OUT.ATTRS = FALSE)
  attrs <- names(vary)
  frames <- vector("list", nrow(grid))
  for (i in seq_len(nrow(grid))) {
    combo <- as.list(grid[i, , drop = FALSE])
    names(combo) <- attrs
    combo <- lapply(combo, as.character)
    frames[[i]] <- persona_frame(
      text       = .render_variant_text(p$text %||% "", combo),
      source     = p$source,
      scope      = p$scope,
      attributes = combo,
      variant_of = p$hash)
  }
  .as_persona_set(frames, attrs, p$hash)
}

# Render a variant brief by stating the varied attributes plainly after the base
# text. Deliberately flat ("This person is ...; their ... is ...") so a varied
# demographic carries no narrative stereotype.
#' @keywords internal
#' @noRd
.render_variant_text <- function(base, combo) {
  if (!length(combo)) return(base)
  clause <- paste0(names(combo), ": ", vapply(combo, function(v)
    paste(as.character(v), collapse = "/"), character(1)))
  paste0(base,
         "\n\nThis person is described by the following attributes (stated as ",
         "plain facts, not as a basis for assuming how they think or behave): ",
         paste(clause, collapse = "; "), ".")
}

# Generative mode: one structured call returns `n` briefs that vary `attrs`,
# under a hard-coded anti-essentialism instruction.
#' @keywords internal
#' @noRd
.persona_variants_generative <- function(p, attrs, n, config) {
  item_props <- c(
    list(text = list(type = "string",
                     description = "The persona brief: an individuated character sketch.")),
    stats::setNames(
      lapply(attrs, function(a) list(type = "string",
                                     description = sprintf("This persona's %s.", a))),
      attrs))
  schema <- list(
    type = "object",
    properties = list(
      personas = list(
        type = "array",
        items = list(
          type = "object",
          properties = item_props,
          required = c("text", attrs)))),
    required = "personas")

  sys <- paste0(
    "You write persona briefs for a social-science simulation. Produce ", n,
    " briefs that vary these attributes across the set: ",
    paste(attrs, collapse = ", "), ". ",
    "Hard rule: vary these attributes, but DO NOT reduce people to demographic ",
    "stereotypes. Write individuated, non-caricatured briefs. Two people who ",
    "share an attribute must not share a personality, opinion, or speech style ",
    "because of it. Avoid 'all', 'naturally', 'inherently', 'typical of their', ",
    "and any claim that a demographic determines how a person thinks or behaves. ",
    "Ground each brief in specifics (habits, history, circumstances), not in ",
    "the attribute itself.")
  usr <- paste0(
    "Base persona to build from:\n\n", p$text %||% "",
    "\n\nReturn ", n, " briefs as JSON matching the schema. For each, fill the ",
    "attribute fields (", paste(attrs, collapse = ", "),
    ") with the value you chose for that brief.")

  cfg <- LLMR::enable_structured_output(config, schema = schema)
  resp <- LLMR::call_llm_robust(cfg, c(system = sys, user = usr))
  parsed <- LLMR::llm_parse_structured(resp)
  rows <- parsed$personas %||% list()
  if (is.data.frame(rows)) {
    rows <- lapply(seq_len(nrow(rows)), function(i) as.list(rows[i, , drop = FALSE]))
  }

  frames <- lapply(rows, function(r) {
    combo <- r[intersect(attrs, names(r))]
    combo <- lapply(combo, function(v) as.character(v)[1])
    persona_frame(
      text       = as.character(r$text %||% "")[1],
      source     = p$source,
      scope      = p$scope,
      attributes = if (length(combo)) combo else NULL,
      variant_of = p$hash)
  })
  .as_persona_set(frames, attrs, p$hash)
}

# Assemble a list of persona_frame objects into the persona_set tibble: a
# persona list column, the per-frame hash as id, the parent hash, and one
# column per varied attribute.
#' @keywords internal
#' @noRd
.as_persona_set <- function(frames, attrs, parent_hash) {
  if (!length(frames)) {
    base <- tibble::tibble(
      persona    = list(),
      id         = character(0),
      variant_of = character(0))
    for (a in attrs) base[[a]] <- character(0)
    class(base) <- c("persona_set", class(base))
    return(base)
  }
  out <- tibble::tibble(
    persona    = frames,
    id         = vapply(frames, function(f) f$hash %||% NA_character_, character(1)),
    variant_of = vapply(frames, function(f) f$variant_of %||% NA_character_, character(1)))
  for (a in attrs) {
    out[[a]] <- vapply(frames, function(f) {
      v <- f$attributes[[a]]
      if (is.null(v)) NA_character_ else as.character(v)[1]
    }, character(1))
  }
  class(out) <- c("persona_set", class(out))
  out
}

#' @rdname persona_variants
#' @param x A `persona_set`.
#' @param ... Ignored.
#' @export
print.persona_set <- function(x, ...) {
  attrs <- setdiff(names(x), c("persona", "id", "variant_of"))
  cat(sprintf("<persona_set: %d persona(s) | varied: %s>\n",
              nrow(x), if (length(attrs)) paste(attrs, collapse = ", ") else "(none)"))
  show_cols <- intersect(c("id", attrs), names(x))
  if (nrow(x) && length(show_cols)) {
    tbl <- x[, show_cols, drop = FALSE]
    if ("id" %in% names(tbl)) tbl$id <- substr(tbl$id, 1L, 10L)
    print(tibble::as_tibble(tbl), n = min(nrow(tbl), 10L))
  }
  invisible(x)
}

#' Audit persona briefs for essentializing language and caricature
#'
#' Read persona briefs back as text and flag the ways synthetic personas fail
#' representationally. Two layers:
#'
#' - **Lexical** (always, no model): each brief is scanned against a small
#'   built-in lexicon of essentializing and demographic-as-destiny patterns.
#'   A brief that says a demographic *naturally* or *always* thinks something
#'   is flagged.
#' - **Model** (optional, when `config` is a generative `LLMR::llm_config()`):
#'   each brief is scored on caricature and essentialism on a 0--1 scale via
#'   [LLMR::llm_judge()]. Without a config these scores are `NA`.
#'
#' The lexical layer is a screening pass, not a proof: a clean scan does not certify a
#' brief is unbiased, and a hit may be a false positive in quoted speech. Treat
#' the audit as evidence to read, alongside the briefs themselves.
#'
#' @param p_or_set A [persona_frame()], a [persona_variants()] result
#'   (`persona_set`), or a list of `persona_frame` objects.
#' @param config Optional generative `LLMR::llm_config()` for model scoring.
#'   When `NULL` (default), only the lexical layer runs and model scores are
#'   `NA`.
#' @param dimensions Optional character vector naming the qualities to score
#'   (model layer); defaults to caricature, out-group homogeneity, and
#'   essentialism. Recorded in the judge prompt.
#' @return A tibble of class `persona_audit`, one row per persona, with columns
#'   `id`, `flag_lexical` (any lexical hit), `n_lexical_hits`, `caricature_score`
#'   (0--1 or `NA`), `essentialism_score` (0--1 or `NA`), and `notes`.
#' @seealso [persona_frame()], [persona_variants()], [diagnostics()]
#' @examples
#' set <- persona_variants(
#'   persona_frame("A small-business owner.", source = "synthetic"),
#'   vary = list(age = c("35", "60")))
#' persona_audit(set)
#' diagnostics(persona_audit(set))
#' \dontrun{
#' cfg <- LLMR::llm_config("openai", "gpt-4o-mini")
#' persona_audit(set, config = cfg)   # adds model caricature scores
#' }
#' @export
persona_audit <- function(p_or_set, config = NULL, dimensions = NULL) {
  frames <- .collect_personas(p_or_set)
  ids   <- vapply(frames, function(f) f$hash %||% NA_character_, character(1))
  texts <- vapply(frames, function(f) f$text %||% "", character(1))

  lexicon <- .essentialism_lexicon()
  hit_lists <- lapply(texts, function(t) .lexical_hits(t, lexicon))
  n_hits <- vapply(hit_lists, length, integer(1))
  flag   <- n_hits > 0L
  notes  <- vapply(hit_lists, function(h) {
    if (!length(h)) "" else paste0("matched: ", paste(unique(h), collapse = "; "))
  }, character(1))

  caricature   <- rep(NA_real_, length(frames))
  essentialism <- rep(NA_real_, length(frames))
  if (!is.null(config) && length(frames)) {
    scored <- .persona_model_scores(texts, config, dimensions)
    caricature   <- scored$caricature
    essentialism <- scored$essentialism
  }

  out <- tibble::tibble(
    id                 = ids,
    flag_lexical       = flag,
    n_lexical_hits     = as.integer(n_hits),
    caricature_score   = caricature,
    essentialism_score = essentialism,
    notes              = notes)
  class(out) <- c("persona_audit", class(out))
  out
}

# Normalize the accepted inputs (a single frame, a persona_set, or a bare list
# of frames) to a flat list of persona_frame objects.
#' @keywords internal
#' @noRd
.collect_personas <- function(p_or_set) {
  if (inherits(p_or_set, "persona_frame")) return(list(p_or_set))
  if (inherits(p_or_set, "persona_set")) {
    return(as.list(p_or_set$persona))
  }
  if (is.list(p_or_set) && length(p_or_set) &&
      all(vapply(p_or_set, inherits, logical(1), "persona_frame"))) {
    return(p_or_set)
  }
  stop("`p_or_set` must be a persona_frame, a persona_set, or a list of ",
       "persona_frame objects.", call. = FALSE)
}

#' Built-in lexicon of essentializing patterns
#'
#' The regex patterns the lexical layer of [persona_audit()] scans for:
#' universal-quantifier claims over a group, nature/inherence language,
#' genetic determinism, never/always behavioral claims, "typical of their
#' \[demographic\]" constructions, and "essentially a/an" framings. ASCII only,
#' matched case-insensitively. Exported for inspection and extension.
#'
#' @return A named character vector of regular expressions.
#' @seealso [persona_audit()]
#' @keywords internal
#' @noRd
.essentialism_lexicon <- function() {
  c(
    universal_group   = "\\ball (men|women|people|members)\\b",
    by_nature         = "\\b(naturally|inherently|by nature)\\b",
    genetic           = "\\bgenetically\\b",
    always_behavior   = "\\b(always|never) (think|believe|behave|act)\\b",
    typical_of_their  = "typical (of|for) (their|his|her) (race|ethnicity|gender|religion|culture)",
    essentially_a     = "\\bessentially (a|an)\\b")
}

# Count and name the lexicon patterns a single brief matches (case-insensitive).
#' @keywords internal
#' @noRd
.lexical_hits <- function(text, lexicon) {
  if (!nzchar(text %||% "")) return(character(0))
  hit <- vapply(lexicon, function(rx) {
    isTRUE(grepl(rx, text, ignore.case = TRUE, perl = TRUE))
  }, logical(1))
  names(lexicon)[hit]
}

# Model layer: score each brief on caricature and essentialism (0-1) with
# LLMR::llm_judge over a one-column data frame. Robust to a judge that returns
# nothing parseable (leaves NA).
#' @keywords internal
#' @noRd
.persona_model_scores <- function(texts, config, dimensions = NULL) {
  dims <- dimensions %||% c("caricature", "out-group homogeneity", "essentialism")
  dim_txt <- paste(dims, collapse = ", ")
  na_out <- list(caricature   = rep(NA_real_, length(texts)),
                 essentialism = rep(NA_real_, length(texts)))

  score_one <- function(quality, instruction) {
    df <- data.frame(text = texts, stringsAsFactors = FALSE)
    prompt <- paste0(
      "Rate this persona brief for ", quality, " on a 0-1 scale, where 0 means ",
      "no ", quality, " (an individuated, fair sketch) and 1 means severe ",
      quality, " (a flattening, stereotyped caricature). ", instruction,
      " Consider these failure modes: ", dim_txt,
      ". Return your reasoning and a single numeric score in [0, 1].")
    judged <- tryCatch(
      LLMR::llm_judge(df, .target = text, .config = config,
                      prompt = prompt, .tags = c("reasoning", "score")),
      error = function(e) NULL)
    if (is.null(judged)) return(rep(NA_real_, length(texts)))
    col <- intersect(c("judge_res_score", "score"), names(judged))
    if (!length(col)) return(rep(NA_real_, length(texts)))
    s <- suppressWarnings(as.numeric(judged[[col[1]]]))
    if (length(s) != length(texts)) return(rep(NA_real_, length(texts)))
    s
  }

  caricature <- tryCatch(
    score_one("caricature", "Judge how much the brief reduces a person to a type."),
    error = function(e) na_out$caricature)
  essentialism <- tryCatch(
    score_one("essentialism",
              "Judge how much the brief treats a demographic as destiny."),
    error = function(e) na_out$essentialism)
  list(caricature = caricature, essentialism = essentialism)
}

#' @rdname persona_audit
#' @param x A `persona_audit`.
#' @param ... Ignored.
#' @export
print.persona_audit <- function(x, ...) {
  n_flag <- sum(x$flag_lexical %||% logical(0), na.rm = TRUE)
  cat(sprintf("<persona_audit: %d persona(s) | %d lexically flagged>\n",
              nrow(x), n_flag))
  if (nrow(x)) {
    ord <- order(-(x$n_lexical_hits %||% 0L),
                 -(x$caricature_score %||% rep(0, nrow(x))),
                 na.last = TRUE)
    tbl <- x[ord, , drop = FALSE]
    show <- tibble::tibble(
      id               = substr(tbl$id, 1L, 10L),
      flag             = tbl$flag_lexical,
      hits             = tbl$n_lexical_hits,
      caricature       = round(tbl$caricature_score, 2L),
      notes            = ifelse(nzchar(tbl$notes), substr(tbl$notes, 1L, 50L), ""))
    print(show, n = min(nrow(show), 10L))
  }
  invisible(x)
}

#' Machine-readable diagnostics for a persona audit
#'
#' A one-row summary of a [persona_audit()]: the number of personas, how many
#' tripped the lexical scan, the worst single hit count, and the mean model
#' caricature score (`NaN` when no model layer ran).
#'
#' @param x A [persona_audit()] result.
#' @param ... Unused.
#' @return A one-row tibble with `n_personas`, `n_flagged`, `max_hits`, and
#'   `mean_caricature`.
#' @seealso [persona_audit()]
#' @importFrom LLMR diagnostics
#' @exportS3Method LLMR::diagnostics persona_audit
diagnostics.persona_audit <- function(x, ...) {
  n <- nrow(x)
  tibble::tibble(
    n_personas      = as.integer(n),
    n_flagged       = as.integer(sum(x$flag_lexical %||% logical(0), na.rm = TRUE)),
    max_hits        = if (n) max(x$n_lexical_hits, na.rm = TRUE) else 0L,
    mean_caricature = if (n) mean(x$caricature_score, na.rm = TRUE) else NA_real_)
}
