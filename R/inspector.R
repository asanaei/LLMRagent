# inspector.R ----------------------------------------------------------------
# A pure read over the run substrate. view_run() projects an agent_run to a
# self-contained HTML inspector that links transcript rows to their span
# events, model calls, and tool outputs. It computes nothing the run does not
# already carry: it only arranges the five level tibbles (utterance, event,
# call, tool, state) into a cross-referenced page. When htmltools is available
# the page is built with htmltools::tags; otherwise a hand-rolled HTML string
# (zero optional dependencies) produces the same visible content. If neither a
# rich nor a minimal HTML page can be built, a structured text summary is the
# floor.

#' View a run as a self-contained HTML inspector
#'
#' `view_run()` renders an [agent_run] (or anything [as_agent_run()] accepts)
#' into a single self-contained HTML file: a header (kind, run id, short
#' manifest hash, agents, creation time, total calls and tokens) followed by one
#' table per grain (utterance, event, call, tool). Event rows carry an HTML
#' anchor keyed by their `span_id`, and transcript and call cells that name a
#' span link to it, so a reader can trace an utterance to the model call, span
#' event, and tool output that produced it.
#'
#' The inspector is a read-only view: it visualizes the substrate already captured by
#' the run and derives nothing. It prefers `htmltools` when that package is
#' installed; otherwise it builds the same content by hand with no optional
#' dependencies. If no HTML page can be produced at all, it writes a structured
#' text summary so the call still yields a file.
#'
#' @param run An [Agent] or any object accepted by [as_agent_run()].
#' @param output Path to write. Defaults to a temp file with extension `.html`.
#'   A missing parent directory is created.
#' @param open If `TRUE` (the default in an interactive session) the file is
#'   opened with [utils::browseURL()] after writing.
#' @return The output path, invisibly, with class `agent_inspector_path` (its
#'   print method reports where the file was written).
#' @seealso [as_agent_run()], [agent_manifest()], [report()]
#' @examples
#' \dontrun{
#' a <- agent("Ada", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' a$chat("Tell me something true.")
#' view_run(a)
#' }
#' @export
view_run <- function(run, output = NULL, open = interactive()) {
  r <- as_agent_run(run)

  if (is.null(output)) output <- tempfile(fileext = ".html")
  dir <- dirname(output)
  if (nzchar(dir) && !dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Each level is guarded: a malformed substrate still yields a valid file with
  # the levels that did project.
  levels <- list(
    utterance = .view_level(r, "utterance"),
    event     = .view_level(r, "event"),
    call      = .view_level(r, "call"),
    tool      = .view_level(r, "tool")
  )
  head <- .view_header(r)

  html <- .view_build_html(head, levels)
  if (is.null(html)) {
    html <- .view_build_text(head, levels)
    writeLines(html, output)
  } else if (isTRUE(attr(html, "saved"))) {
    # htmltools::save_html() already wrote `output`; nothing more to do.
  } else {
    writeLines(html, output)
  }

  if (isTRUE(open) && interactive()) {
    tryCatch(utils::browseURL(output), error = function(e) NULL)
  }
  invisible(structure(output, class = "agent_inspector_path"))
}

#' @export
print.agent_inspector_path <- function(x, ...) {
  cat("Run inspector written to: ", unclass(x)[1], "\n", sep = "")
  invisible(x)
}

# ---- level projection -------------------------------------------------------

# Project one grain to a tibble, guarded so a broken level cannot abort the
# whole render. Returns a zero-row tibble on failure.
#' @keywords internal
#' @noRd
.view_level <- function(r, level) {
  tryCatch(
    tibble::as_tibble(as_tibble(r, level)),
    error = function(e) tibble::tibble()
  )
}

# Header facts pulled from the run and its manifest. Token and call totals come
# from the call level; the manifest hash is best-effort (a partial substrate may
# not hash).
#' @keywords internal
#' @noRd
.view_header <- function(r) {
  calls <- .view_level(r, "call")
  n_calls <- nrow(calls)
  tok_sent <- if ("sent_tokens" %in% names(calls)) sum(calls$sent_tokens, na.rm = TRUE) else NA_real_
  tok_rec  <- if ("rec_tokens"  %in% names(calls)) sum(calls$rec_tokens,  na.rm = TRUE) else NA_real_

  mh <- tryCatch(agent_manifest(r)$manifest_hash, error = function(e) NA_character_)
  mh_short <- if (is.character(mh) && length(mh) == 1L && !is.na(mh)) substr(mh, 1L, 12L) else NA_character_

  agents <- if (nrow(r$participants)) paste(r$participants$name, collapse = ", ") else "(none)"

  list(
    run_id        = r$run_id %||% NA_character_,
    kind          = r$kind %||% "run",
    manifest_hash = mh_short,
    agents        = agents,
    created_at    = r$created_at %||% NA_character_,
    n_calls       = n_calls,
    tokens_sent   = tok_sent,
    tokens_received = tok_rec
  )
}

# ---- HTML escaping and minimal table rendering ------------------------------

# Escape the five characters that matter inside HTML text and attribute values.
# Used by the fallback path; the htmltools path escapes for us.
#' @keywords internal
#' @noRd
.html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  x
}

# Coerce a cell of any type to a short display string.
#' @keywords internal
#' @noRd
.view_cell <- function(v) {
  if (length(v) != 1L) v <- paste(format(v), collapse = ", ")
  if (is.na(v %||% NA)) return("")
  s <- if (inherits(v, "POSIXt")) format(v, "%Y-%m-%dT%H:%M:%S") else as.character(v)
  if (length(s) != 1L) s <- paste(s, collapse = " ")
  if (nchar(s) > 400L) s <- paste0(substr(s, 1L, 400L), "...")
  s
}

# Render a data frame as a minimal HTML <table>, escaping every cell. The
# `anchor_col` column, when present, gets an id="span-<value>" on its <td> so
# other tables can link to it; `link_cols` columns render their value as an
# <a href="#span-<value>"> when non-empty. Returns a single HTML string.
#' @keywords internal
#' @noRd
.df_to_html_table <- function(df, anchor_col = NULL, link_cols = character(0)) {
  df <- tibble::as_tibble(df)
  if (!ncol(df)) return("<p class=\"empty\">(none)</p>")
  if (!nrow(df)) {
    return(sprintf(
      "<table><thead><tr>%s</tr></thead><tbody><tr><td class=\"empty\" colspan=\"%d\">(none)</td></tr></tbody></table>",
      paste0("<th>", .html_escape(names(df)), "</th>", collapse = ""),
      ncol(df)))
  }
  nms <- names(df)
  header <- paste0("<th>", .html_escape(nms), "</th>", collapse = "")
  body <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    cells <- character(length(nms))
    for (j in seq_along(nms)) {
      raw <- .view_cell(df[[j]][i])
      esc <- .html_escape(raw)
      nm <- nms[j]
      attr_id <- ""
      if (!is.null(anchor_col) && identical(nm, anchor_col) && nzchar(raw)) {
        attr_id <- sprintf(" id=\"span-%s\"", .html_escape(raw))
      }
      content <- esc
      if (nm %in% link_cols && nzchar(raw)) {
        content <- sprintf("<a href=\"#span-%s\">%s</a>", .html_escape(raw), esc)
      }
      cells[j] <- sprintf("<td%s>%s</td>", attr_id, content)
    }
    body[i] <- paste0("<tr>", paste0(cells, collapse = ""), "</tr>")
  }
  paste0(
    "<table><thead><tr>", header, "</tr></thead><tbody>",
    paste0(body, collapse = ""), "</tbody></table>")
}

# ---- shared style -----------------------------------------------------------

# Minimal inline CSS so the page is self-contained.
#' @keywords internal
#' @noRd
.view_css <- function() {
  paste0(
    "body{font-family:-apple-system,Segoe UI,Roboto,Helvetica,Arial,sans-serif;",
    "margin:1.5rem;color:#1a1a1a;line-height:1.4}",
    "h1{font-size:1.4rem;margin:0 0 .3rem 0}",
    "h2{font-size:1.1rem;margin:1.6rem 0 .4rem 0;border-bottom:1px solid #ddd;padding-bottom:.2rem}",
    ".meta{color:#444;font-size:.9rem;margin:.1rem 0}",
    ".meta code{background:#f3f3f3;padding:.05rem .3rem;border-radius:3px}",
    "table{border-collapse:collapse;width:100%;margin:.3rem 0;font-size:.82rem}",
    "th,td{border:1px solid #ddd;padding:.25rem .45rem;text-align:left;vertical-align:top}",
    "th{background:#f5f5f5;position:sticky;top:0}",
    "tr:nth-child(even) td{background:#fafafa}",
    ".empty{color:#999;font-style:italic}",
    "a{color:#1558b0;text-decoration:none}a:hover{text-decoration:underline}",
    "td:target,tr:target td{background:#fff6c2 !important}")
}

# A short prose note describing the cross-references, so the linkage is legible
# even where a transcript row carries no call_id.
#' @keywords internal
#' @noRd
.view_note <- function() {
  paste0(
    "Event rows are anchored by their span id. A call_id in the utterance or ",
    "call table links to its span event above, so a transcript row can be ",
    "traced to the model call, span, and tool output that produced it.")
}

# ---- htmltools path ---------------------------------------------------------

# Build the page with htmltools when available. Returns an HTML string (the
# fully rendered document) or, when save_html() succeeds, a zero-length string
# carrying attr(.,"saved")=TRUE. Returns NULL when htmltools is not installed so
# the caller can fall back.
#' @keywords internal
#' @noRd
.view_build_html <- function(head, levels) {
  if (!requireNamespace("htmltools", quietly = TRUE)) return(NULL)
  tryCatch(.view_build_html_impl(head, levels), error = function(e) NULL)
}

#' @keywords internal
#' @noRd
.view_build_html_impl <- function(head, levels) {
  tags <- htmltools::tags
  HTML <- htmltools::HTML

  meta_line <- function(label, value) {
    tags$p(class = "meta", tags$strong(paste0(label, ": ")),
           tags$span(.view_cell(value)))
  }
  num <- function(v) if (is.na(v %||% NA)) "NA" else format(v)

  header <- htmltools::tagList(
    tags$h1(sprintf("Run inspector - %s", head$kind %||% "run")),
    meta_line("run id", head$run_id),
    meta_line("manifest", head$manifest_hash),
    meta_line("agents", head$agents),
    meta_line("created", head$created_at),
    tags$p(class = "meta",
           tags$strong("calls: "), tags$span(num(head$n_calls)),
           HTML(" &middot; "),
           tags$strong("tokens sent: "), tags$span(num(head$tokens_sent)),
           HTML(" &middot; "),
           tags$strong("tokens received: "), tags$span(num(head$tokens_received))),
    tags$p(class = "meta empty", .view_note())
  )

  # Build each section's table as raw HTML (reusing the hand-rolled renderer so
  # anchors and links are identical across both paths), wrapped in htmltools so
  # the document is one coherent tree.
  section <- function(title, df, anchor_col = NULL, link_cols = character(0)) {
    htmltools::tagList(
      tags$h2(title),
      HTML(.df_to_html_table(df, anchor_col = anchor_col, link_cols = link_cols))
    )
  }

  body <- htmltools::tagList(
    header,
    section("Utterances (analysis grain)", levels$utterance,
            link_cols = c("call_id")),
    section("Events (spans)", levels$event, anchor_col = "span_id"),
    section("Model calls", levels$call, link_cols = c("span_id")),
    section("Tool calls", levels$tool, link_cols = c("span_id"))
  )

  page <- tags$html(
    tags$head(
      tags$meta(charset = "utf-8"),
      tags$title(sprintf("Run inspector: %s", head$run_id %||% "run")),
      tags$style(HTML(.view_css()))
    ),
    tags$body(body)
  )

  out <- structure(htmltools::doRenderTags(page), saved = FALSE)
  out
}

# ---- text fallback ----------------------------------------------------------

# A structured plain-text summary, the floor when no HTML can be built. Wrapped
# in a minimal HTML shell so the written file is still openable and contains the
# tokens the rest of the toolchain expects.
#' @keywords internal
#' @noRd
.view_build_text <- function(head, levels) {
  num <- function(v) if (is.na(v %||% NA)) "NA" else format(v)
  body <- c(
    "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><style>",
    .view_css(),
    "</style></head><body><pre>",
    sprintf("Run inspector -- %s", head$kind %||% "run"),
    sprintf("run id:   %s", head$run_id %||% "NA"),
    sprintf("manifest: %s", head$manifest_hash %||% "NA"),
    sprintf("agents:   %s", head$agents %||% "NA"),
    sprintf("created:  %s", head$created_at %||% "NA"),
    sprintf("calls: %s | tokens sent: %s | tokens received: %s",
            num(head$n_calls), num(head$tokens_sent), num(head$tokens_received)),
    .view_note(),
    "</pre>")
  for (nm in names(levels)) {
    body <- c(body, sprintf("<h2>%s (span linked)</h2>", .html_escape(nm)),
              .df_to_html_table(levels[[nm]],
                                anchor_col = if (nm == "event") "span_id" else NULL,
                                link_cols = if (nm %in% c("utterance")) "call_id"
                                            else if (nm %in% c("call", "tool")) "span_id"
                                            else character(0)))
  }
  body <- c(body, "</body></html>")
  body
}
