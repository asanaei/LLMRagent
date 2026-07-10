# archive.R -------------------------------------------------------------------
# Seal an agent study to a directory: the manifest (the apparatus identity), the
# transcript and event/call/tool/state views, the per-call provenance (the LLMR
# audit log, copied verbatim when one was kept), the artifacts, a drafted
# methods note, and a SHA-256 manifest over every file written. This is the one
# Stage-1 surface that writes to disk.
#
# Two invariants govern the writing. First, hashes are identity, not outcome:
# the request_hash in calls.jsonl is computed over the *original* request, so a
# call read back from the archive still matches the same call issued live; that
# is why redaction never touches a hash, and why the verbatim audit log is
# copied byte-for-byte rather than reserialized. Second, an archive should
# survive a partial run: every level extraction is guarded, so a study that
# failed halfway still seals what it has.

#' Seal an agent study to a directory
#'
#' Writes a self-contained, hash-sealed archive of a run: the study
#' [agent_manifest()] (`manifest.json`), the transcript (`transcript.csv`), the
#' event / call / tool / state views, the per-call provenance (`calls.jsonl`,
#' the LLMR audit log copied verbatim when one was kept), any artifacts, a
#' drafted methods note (`README-methods.md`), and a `hashes.sha256` manifest
#' over every file written. The archive is the supplementary material a paper
#' can ship: it carries the apparatus's identity and the calls' provenance, not
#' just the prose.
#'
#' Hashes are identity, not outcome. The `request_hash` in `calls.jsonl` is
#' computed over the original request, so a call read back from the archive
#' still matches the same call issued live. Redaction therefore never touches a
#' hash or a request body, and when an LLMR audit log is present it is copied
#' byte-for-byte rather than reserialized -- unless a privacy lever is engaged:
#' with `include_messages = FALSE` each copied record's request body and reply
#' text are removed (its precomputed `request_hash` is kept, so the join
#' survives), and with `redact` the copied records' reply text is scrubbed.
#'
#' @param run An object accepted by [as_agent_run()] (an [Agent], a conversation
#'   or preset result, a pipeline, an experiment, or an `agent_run`).
#' @param path Directory to write into; created (recursively) if absent.
#' @param include_messages If `TRUE` (default), write the transcript and the
#'   free-text columns of the tool and state tables. If `FALSE`, those tables
#'   are still written but their free-text columns are blanked, keeping only
#'   structure, hashes, and metadata; the records in `calls.jsonl` likewise
#'   omit the request body and reply text (each record keeps its precomputed
#'   `request_hash`, so the join invariant holds).
#' @param redact Optional redaction applied to the free-text columns of the
#'   written transcript, tool, and state tables (`text`, `content`, `arguments`,
#'   `result`, `response_text`) and to the reply text of the records in
#'   `calls.jsonl` -- never to hashes, and never to a request body, which is
#'   the call's identity (omit request bodies with `include_messages = FALSE`).
#'   Either a `function(text) -> text`, or a character vector of regular
#'   expressions, each of which is replaced by `"[REDACTED]"`. Redaction is
#'   applied to the on-disk copy after hashing, so the join invariant holds.
#' @param formats Which optional formats to write, any of `"csv"` (the tabular
#'   views), `"jsonl"` (events and calls; always written), and `"rds"` (the
#'   `agent_run` object as `run.rds`). Defaults to all three.
#' @return Invisibly, an object of class `agent_archive`: a list with `path`,
#'   `files` (relative paths written), `manifest_hash`, and `n_calls`. Print
#'   lists what was written and confirms the seal.
#' @seealso [agent_manifest()], [as_agent_run()], [LLMR::llm_log_read()]
#' @examples
#' \dontrun{
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"))
#' a$chat("Hello")
#' archive_agent_study(a, tempfile("study_"))
#' }
#' @export
archive_agent_study <- function(run, path, include_messages = TRUE,
                                redact = NULL,
                                formats = c("csv", "jsonl", "rds")) {
  r <- as_agent_run(run)
  formats <- match.arg(formats, several.ok = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  redactor <- .make_redactor(redact)
  files <- character(0)

  # A guarded level extraction: a partial or malformed run still seals what it
  # has, with an empty frame standing in for a level that could not be built.
  lvl <- function(level) tryCatch(
    tibble::as_tibble(as_tibble(r, level = level)),
    error = function(e) tibble::tibble())

  # ---- manifest.json (apparatus identity; tibbles -> data.frames for rows) ---
  man <- tryCatch(agent_manifest(r), error = function(e) NULL)
  manifest_hash <- man$manifest_hash %||% NA_character_
  files <- c(files, tryCatch({
    body <- if (is.null(man)) list() else lapply(unclass(man), function(el)
      if (inherits(el, "tbl_df") || is.data.frame(el)) as.data.frame(el) else el)
    txt <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE,
                            null = "null", na = "null")
    .write_lines(as.character(txt), file.path(path, "manifest.json"))
    "manifest.json"
  }, error = function(e) character(0)))

  # ---- transcript.csv (analysis grain) --------------------------------------
  if ("csv" %in% formats) {
    files <- c(files, tryCatch({
      utt <- .redact_cols(lvl("utterance"), redactor, include_messages)
      utils::write.csv(utt, file.path(path, "transcript.csv"), row.names = FALSE)
      "transcript.csv"
    }, error = function(e) character(0)))
  }

  # ---- events.jsonl (every span, one JSON object per line) ------------------
  files <- c(files, tryCatch({
    .write_jsonl(lvl("event"), file.path(path, "events.jsonl"))
    "events.jsonl"
  }, error = function(e) character(0)))

  # ---- calls.jsonl (per-call provenance; a true LLMR audit log) -------------
  # Always a file LLMR::llm_log_read() can parse. When a live audit log was
  # active during the run it is copied verbatim (its request hashes must still
  # match a live call). Otherwise the per-call spans (which carry the rendered
  # request body, the served model id, usage, and the reply) are rendered into
  # the same audit-record schema, so the request body is real, not reconstructed.
  calls_source <- tryCatch({
    dest <- file.path(path, "calls.jsonl")
    log <- r$llmr_log
    if (!is.null(log) && length(log) == 1L && !is.na(log) && file.exists(log)) {
      # Filter the live session log to THIS run's calls, by request hash, so a
      # session with prior unrelated calls does not leak them into the archive.
      run_hashes <- tryCatch(
        stats::na.omit(tibble::as_tibble(lvl("call"))$request_hash),
        error = function(e) character(0))
      n_kept <- NA_integer_
      if (length(run_hashes)) {
        n_kept <- .filter_log_to_hashes(log, dest, run_hashes)
      } else {
        file.copy(log, dest, overwrite = TRUE)  # no hashes known: copy whole log
      }
      # The privacy levers apply to the copied lines too: include_messages =
      # FALSE strips each record's request body and reply text (the precomputed
      # request_hash is kept, so the join survives); a redactor scrubs the
      # reply text. Only with neither lever is the copy byte-for-byte.
      if (!isTRUE(include_messages) || !is.null(redact)) {
        .privacy_scrub_log(dest, include_messages = include_messages,
                           redactor = redactor)
      }
      if (identical(n_kept, 0L)) {
        # An honest flag: the live log had none of this run's calls (e.g. it
        # was rotated or belongs to another session), so calls.jsonl is empty
        # and the archive must not claim live-log provenance for its calls.
        warning("The active LLMR audit log contains no records matching this ",
                "run's request hashes; calls.jsonl is empty and the archive ",
                "is marked accordingly.", call. = FALSE)
        "live_llmr_log_empty"
      } else {
        "live_llmr_log"
      }
    } else {
      .write_audit_calls(r, dest, include_messages = include_messages,
                         redactor = redactor)
    }
  }, error = function(e) "none")
  if (!identical(calls_source, "none")) files <- c(files, "calls.jsonl")

  # ---- tools.csv / state.csv (governed tool calls; agent memory at end) -----
  if ("csv" %in% formats) {
    files <- c(files, tryCatch({
      tl <- .redact_cols(lvl("tool"), redactor, include_messages)
      utils::write.csv(tl, file.path(path, "tools.csv"), row.names = FALSE)
      "tools.csv"
    }, error = function(e) character(0)))
    files <- c(files, tryCatch({
      st <- .redact_cols(lvl("state"), redactor, include_messages)
      utils::write.csv(st, file.path(path, "state.csv"), row.names = FALSE)
      "state.csv"
    }, error = function(e) character(0)))
  }

  # ---- run.rds (the object itself) ------------------------------------------
  if ("rds" %in% formats) {
    files <- c(files, tryCatch({
      saveRDS(r, file.path(path, "run.rds"))
      "run.rds"
    }, error = function(e) character(0)))
  }

  # ---- artifacts/ (kind-specific products) ----------------------------------
  files <- c(files, tryCatch(.write_artifacts(r$artifacts %||% list(), path),
                             error = function(e) character(0)))

  # ---- README-methods.md (design header + drafted methods + file list) ------
  files <- c(files, tryCatch({
    .write_readme(r, man, files, file.path(path, "README-methods.md"))
    "README-methods.md"
  }, error = function(e) character(0)))

  # ---- hashes.sha256 (seal: one line per file, sha256sum format) ------------
  # Written last so it can hash every other file; it cannot hash itself.
  files <- unique(files)
  tryCatch({
    lines <- vapply(files, function(rel) {
      sprintf("%s  %s", .file_sha256(file.path(path, rel)), rel)
    }, character(1), USE.NAMES = FALSE)
    .write_lines(lines, file.path(path, "hashes.sha256"))
  }, error = function(e) NULL)
  files <- c(files, "hashes.sha256")

  n_calls <- tryCatch(nrow(lvl("call")), error = function(e) 0L)

  structure(
    list(path = path, files = files,
         manifest_hash = manifest_hash, n_calls = as.integer(n_calls),
         calls_source = calls_source,
         llm_log_compatible = calls_source %in% c("live_llmr_log", "audit_from_spans")),
    class = "agent_archive")
}

# ---- redaction --------------------------------------------------------------

# Build the function that scrubs free text, from NULL / a function / a vector of
# regexes. NULL is identity.
#' @keywords internal
#' @noRd
.make_redactor <- function(redact) {
  if (is.null(redact)) return(function(text) text)
  if (is.function(redact)) return(redact)
  if (is.character(redact)) {
    patterns <- redact
    return(function(text) {
      out <- as.character(text)
      for (p in patterns) out <- gsub(p, "[REDACTED]", out, perl = TRUE)
      out
    })
  }
  stop("`redact` must be NULL, a function, or a character vector of regexes.",
       call. = FALSE)
}

# Apply redaction (and, when include_messages is FALSE, blanking) to the
# free-text columns of a table copy. Hash columns are left untouched.
#' @keywords internal
#' @noRd
.redact_cols <- function(df, redactor, include_messages) {
  if (!is.data.frame(df) || !nrow(df)) return(df)
  text_cols <- intersect(
    c("text", "content", "arguments", "result", "response_text"), names(df))
  for (cn in text_cols) {
    v <- as.character(df[[cn]])
    if (!isTRUE(include_messages)) {
      v <- ifelse(is.na(v), v, "[OMITTED]")
    } else {
      v <- redactor(v)
    }
    df[[cn]] <- v
  }
  df
}

# ---- writers ----------------------------------------------------------------

# Write character lines as UTF-8, one per line.
#' @keywords internal
#' @noRd
.write_lines <- function(lines, file) {
  writeLines(enc2utf8(as.character(lines)), file, useBytes = TRUE)
  invisible(file)
}

# Stream a data frame to JSONL: one JSON object per row, optionally stamping a
# scalar field (e.g. schema_version) onto every line.
#' @keywords internal
#' @noRd
.write_jsonl <- function(df, file, schema_version = NULL) {
  con <- file(file, open = "wb")
  on.exit(close(con), add = TRUE)
  if (!is.data.frame(df) || !nrow(df)) {
    return(invisible(file))
  }
  for (i in seq_len(nrow(df))) {
    row <- as.list(df[i, , drop = FALSE])
    row <- lapply(row, function(v) {
      if (inherits(v, "POSIXct")) format(v, "%Y-%m-%dT%H:%M:%S%z") else v
    })
    if (!is.null(schema_version)) {
      row <- c(list(schema_version = schema_version), row)
    }
    line <- jsonlite::toJSON(row, auto_unbox = TRUE, null = "null", na = "null")
    cat(as.character(line), "\n", sep = "", file = con)
  }
  invisible(file)
}

# Copy a live LLMR audit log to `dest`, keeping only the records whose
# request_hash is in `keep_hashes` (this run's calls). Uses llm_log_read()'s
# manifest, which computes request_hash exactly as a live call would, so the
# match is on the same identity the run's call level carries. Falls back to a
# full copy if the manifest cannot be built. Returns the number of records
# written (NA for the full-copy fallback), so the caller can flag an archive
# whose live log turned out to contain none of the run's calls.
#' @keywords internal
#' @noRd
.filter_log_to_hashes <- function(log, dest, keep_hashes) {
  parsed <- tryCatch(LLMR::llm_log_read(log), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$manifest) || !nrow(parsed$manifest)) {
    file.copy(log, dest, overwrite = TRUE)
    return(invisible(NA_integer_))
  }
  man <- parsed$manifest
  keep_idx <- man$idx[man$request_hash %in% keep_hashes]
  raw_lines <- vapply(parsed$records, function(r) r$raw %||% "", character(1))
  out_lines <- raw_lines[keep_idx]
  con <- file(dest, open = "wb")
  on.exit(close(con), add = TRUE)
  if (length(out_lines)) writeLines(enc2utf8(out_lines), con, useBytes = TRUE)
  invisible(length(out_lines))
}

# Apply the privacy levers to an already-written calls.jsonl copied from a
# live LLMR audit log. include_messages = FALSE removes each record's request
# body and reply text, first stamping the precomputed request_hash into the
# record (the reader can no longer recompute it once the body is gone, and
# the hash is identity, not content). A redactor scrubs the reply text only:
# the request body is the call's identity and is never redacted -- omitting
# it wholesale (include_messages = FALSE) is the lever for private requests.
#' @keywords internal
#' @noRd
.privacy_scrub_log <- function(dest, include_messages = TRUE, redactor = NULL) {
  parsed <- tryCatch(LLMR::llm_log_read(dest), error = function(e) NULL)
  if (is.null(parsed) || !length(parsed$records)) return(invisible(dest))
  man <- parsed$manifest
  lines <- vapply(seq_along(parsed$records), function(i) {
    rec <- parsed$records[[i]]$rec
    if (!isTRUE(include_messages)) {
      if (!is.null(rec[["request"]]) && is.null(rec$request_hash)) {
        rec$request_hash <- man$request_hash[man$idx == i][1]
      }
      rec[["request"]] <- NULL
      rec$text <- NULL
    } else if (!is.null(redactor) && !is.null(rec$text)) {
      rec$text <- redactor(as.character(rec$text))
    }
    as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null",
                                  na = "null"))
  }, character(1))
  .write_lines(lines, dest)
  invisible(dest)
}

# Render a run's per-call spans into the LLMR audit-log record schema, one JSON
# object per line, so the file parses identically with LLMR::llm_log_read(). The
# spans carry the rendered request body, the served model id, usage, and the
# reply (stashed in account()), so the request body written here is the real one,
# not a reconstruction. Writing the full `request` object (rather than a bare
# `request_hash`) is what makes this a faithful audit log: a reader recomputes
# the hash over the same body the live call hashed, so the archive joins the run.
#' @keywords internal
#' @noRd
.write_audit_calls <- function(r, dest, include_messages = TRUE, redactor = NULL) {
  spans <- Filter(function(s) identical(s$event_type %||% "", "call"), r$spans %||% list())
  con <- file(dest, open = "wb")
  on.exit(close(con), add = TRUE)
  red <- function(x) if (is.null(redactor) || is.null(x)) x else redactor(x)

  for (s in spans) {
    m <- s$meta %||% list()
    # Reconstruct the provider request body as a list of role/content turns
    # (the shape llm_log_read()/.llmr_turns() canonicalizes). The request body is
    # the call's IDENTITY: it is written VERBATIM (not redacted), so the hash a
    # reader recomputes equals the live run's request_hash (the join invariant).
    # Privacy is the include_messages = FALSE lever, which omits the request and
    # the reply entirely; partial redaction of a hashable identity field would
    # make the archive misrepresent what was actually sent.
    req <- NULL
    if (isTRUE(include_messages) && !is.null(m$request)) {
      # The generation parameters are part of the request's identity: LLMR's
      # reader recomputes request_hash over the body's params exactly as the
      # live hash was keyed on the config's, so they are written at the top
      # level of the body, the same place LLMR's own audit log carries them.
      req <- c(list(messages = .messages_to_turns(m$request, redactor = NULL)),
               .archive_body_params(m$params))
    }
    usage <- m$usage
    rec <- list(
      ts             = format(s$started_at %||% Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
      schema_version = "1.0",
      llmr_version   = as.character(r$pkg_versions$LLMR %||% utils::packageVersion("LLMR")),
      kind           = "call",
      provider       = m$provider %||% NA_character_,
      model          = m$model %||% NA_character_,
      status         = if (identical(s$status, "ok")) 200L else NA_integer_,
      request        = req,
      # the precomputed identity, so an auditor can verify even when the body is
      # omitted for privacy (include_messages = FALSE)
      request_hash   = s$request_hash %||% NA_character_,
      model_version  = m$model_version %||% NA_character_,
      finish_reason  = m$finish_reason %||% NA_character_,
      usage          = usage,
      response_id    = s$response_id %||% NA_character_,
      duration_s     = s$duration_s %||% NA_real_,
      text           = if (isTRUE(include_messages)) red(m$text) else NULL
    )
    rec <- rec[!vapply(rec, is.null, logical(1))]
    line <- jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null")
    cat(as.character(line), "\n", sep = "", file = con)
  }
  "audit_from_spans"
}

# The generation parameters to write into an audit record's request body, so
# that a reader recomputes the same request_hash the live call was keyed on.
# Keeps exactly what LLMR's hash keeps: model params minus transport-only
# settings, minus empty/NA values, minus anything JSON cannot carry
# (functions). The reader's own normalization then reproduces the config-side
# param set. The drop list mirrors LLMR's transport list; a param dropped
# here AND by LLMR's hash is harmless either way.
#' @keywords internal
#' @noRd
.archive_body_params <- function(params) {
  if (!is.list(params) || !length(params)) return(list())
  drop <- c(
    # structural body fields (never params)
    "messages", "contents", "system", "systemInstruction", "generationConfig",
    "model", "tools",
    # transport-only settings LLMR's request hash drops
    "req_builder", "request_modifier", "response_modifier", "timeout",
    "api_url", "base_url", "max_tries", "verbose", "cache",
    "use_responses_api", "anthropic_beta", "vertex", "project", "location",
    "stream", "stream_options")
  keep <- params[setdiff(names(params), drop)]
  keep[vapply(keep, function(v) {
    !(is.null(v) || is.function(v) || length(v) == 0L ||
        (length(v) == 1L && is.atomic(v) && is.na(v)))
  }, logical(1))]
}

# Convert a messages object (named char vector, bare string, or list of
# role/content turns) into a list of list(role=, content=) for JSON output.
#' @keywords internal
#' @noRd
.messages_to_turns <- function(messages, redactor = NULL) {
  red <- function(x) if (is.null(redactor)) x else redactor(x)
  if (is.character(messages)) {
    roles <- names(messages)
    if (is.null(roles)) roles <- rep("user", length(messages))
    roles[!nzchar(roles)] <- "user"
    return(lapply(seq_along(messages), function(i)
      list(role = roles[i], content = red(as.character(messages[[i]])))))
  }
  if (is.list(messages)) {
    return(lapply(messages, function(mm) {
      if (is.list(mm) && !is.null(mm$role)) {
        ct <- mm$content
        if (is.character(ct)) ct <- red(ct)
        list(role = mm$role, content = ct)
      } else {
        list(role = "user", content = red(as.character(mm)))
      }
    }))
  }
  list()
}

# Write each artifact: a data frame -> <name>.csv, anything else -> <name>.json.
# Returns the relative paths written (under artifacts/).
#' @keywords internal
#' @noRd
.write_artifacts <- function(artifacts, path) {
  if (!length(artifacts)) return(character(0))
  adir <- file.path(path, "artifacts")
  dir.create(adir, recursive = TRUE, showWarnings = FALSE)
  nms <- names(artifacts)
  if (is.null(nms)) nms <- paste0("artifact_", seq_along(artifacts))
  written <- character(0)
  for (i in seq_along(artifacts)) {
    nm <- if (nzchar(nms[i])) nms[i] else paste0("artifact_", i)
    safe <- gsub("[^A-Za-z0-9._-]+", "_", nm)
    el <- artifacts[[i]]
    rel <- tryCatch({
      if (is.data.frame(el)) {
        utils::write.csv(el, file.path(adir, paste0(safe, ".csv")), row.names = FALSE)
        file.path("artifacts", paste0(safe, ".csv"))
      } else {
        txt <- jsonlite::toJSON(el, auto_unbox = TRUE, pretty = TRUE,
                                null = "null", na = "null")
        .write_lines(as.character(txt), file.path(adir, paste0(safe, ".json")))
        file.path("artifacts", paste0(safe, ".json"))
      }
    }, error = function(e) character(0))
    written <- c(written, rel)
  }
  written
}

# Compose README-methods.md: a design header, the drafted methods paragraph,
# and the list of files in the archive.
#' @keywords internal
#' @noRd
.write_readme <- function(r, man, files, file) {
  agents <- stats::na.omit(as.character(r$participants$name %||% character(0)))
  short_hash <- if (!is.null(man)) substr(man$manifest_hash, 1L, 12L) else "unknown"

  header <- c(
    "# Agent study archive",
    "",
    sprintf("- kind: %s", r$kind %||% "run"),
    sprintf("- run_id: %s", r$run_id %||% "?"),
    sprintf("- agents: %s",
            if (length(agents)) paste(agents, collapse = ", ") else "(none recorded)"),
    sprintf("- manifest_hash (short): %s", short_hash),
    sprintf("- created: %s", r$created_at %||% "?"),
    "",
    "## Methods",
    ""
  )

  calls <- tryCatch(tibble::as_tibble(as_tibble(r, level = "call")),
                    error = function(e) NULL)
  methods <- if (!is.null(calls)) {
    tryCatch(LLMR::llm_methods_text(calls),
             error = function(e) "Methods paragraph unavailable (no call records).")
  } else {
    "Methods paragraph unavailable (no call records)."
  }

  file_list <- c("", "## Files in this archive", "",
                 paste0("- ", c(files, "hashes.sha256")))

  .write_lines(c(header, methods, file_list), file)
  invisible(file)
}

# ---- hashing ----------------------------------------------------------------

# SHA-256 over a file's raw bytes, without R serialization (stable across R
# versions), matching the LLMR audit-log line-hash convention.
#' @keywords internal
#' @noRd
.file_sha256 <- function(f) {
  if (!file.exists(f)) return(NA_character_)
  sz <- file.size(f)
  raw <- if (is.na(sz) || sz == 0) raw(0) else readBin(f, "raw", n = sz)
  digest::digest(raw, algo = "sha256", serialize = FALSE)
}

# ---- print ------------------------------------------------------------------

#' @export
print.agent_archive <- function(x, ...) {
  cat(sprintf("<agent_archive | %d file(s) | %d call(s)>\n",
              length(x$files), x$n_calls %||% 0L))
  cat("  path: ", x$path, "\n", sep = "")
  if (!is.na(x$manifest_hash %||% NA_character_)) {
    cat("  manifest_hash: ", substr(x$manifest_hash, 1L, 12L), "\n", sep = "")
  }
  if (length(x$files)) {
    cat("  files:\n")
    for (f in x$files) cat("    - ", f, "\n", sep = "")
  }
  cat("  sealed by hashes.sha256 (SHA-256 over each file's bytes)\n")
  invisible(x)
}
