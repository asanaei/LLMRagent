# archive.R -------------------------------------------------------------------
# Seal an agent study to a directory: the manifest (the declared specification), the
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
# survive a partial run: every file is written in its own guarded step, so a
# study that failed halfway still seals what it has, and every file that could
# not be produced is removed and reported in `write_failures` rather than left
# in place or replaced by an empty stand-in.

#' Seal an agent study to a directory
#'
#' Writes a self-contained, hash-sealed archive of a run: the study
#' [agent_manifest()] (`manifest.json`), the transcript (`transcript.csv`), the
#' event / call / tool / state views, the per-call provenance (`calls.jsonl`:
#' this run's records from the LLMR audit log, copied verbatim, when one was
#' kept), any artifacts, a
#' drafted methods note (`README-methods.md`), and a `hashes.sha256` manifest
#' over every file written. The archive is the supplementary material a paper
#' can ship: it carries the declared specification and the calls' provenance, not
#' just the prose.
#'
#' The archive is privacy-preserving at its serialization boundary: `run.rds`
#' is a data-only snapshot of the same projected levels written to the text
#' formats. Live agents, callers, tool functions, model configurations, and
#' configuration secrets are never serialized into the archive. Message
#' omission and redaction apply to this snapshot as well as the text files.
#' They do not reach the manifest, artifacts, or error messages.
#'
#' Hashes are identity, not outcome. The `request_hash` in `calls.jsonl` is
#' computed over the original request, so a call read back from the archive
#' still matches the same call issued live. Redaction therefore never touches a
#' hash or a request body, and when an LLMR audit log is present this run's
#' records are copied byte-for-byte rather than reserialized -- unless a privacy
#' lever is engaged: with `include_messages = FALSE` each copied record's
#' request body and reply text are removed (its precomputed `request_hash` is
#' kept, so the join survives), and with `redact` the copied records' reply
#' text is scrubbed.
#'
#' A record is this run's when its request hash matches one of the run's calls
#' and, where both carry one, so does its response id, which keeps an earlier
#' identical request out. When a response id is missing on either side, or
#' more records match than the run made calls, the function warns that an
#' earlier identical request can be among the copied records. The rest of the
#' log is never copied. When the log
#' cannot be read, or the run's calls carry no request hashes, `calls.jsonl` is
#' rendered from the run's own call records instead, with a warning. When a
#' privacy lever cannot be applied, the copied records are removed and
#' `calls.jsonl` is not written.
#'
#' Each file is written in its own step. A file that cannot be produced is
#' removed, recorded in `write_failures`, and reported in one warning at the
#' end; the rest of the archive is still written. A level of the run that
#' cannot be read is not written as an empty table (a level that is readable
#' and empty is), `run.rds` is omitted when any of its levels cannot be read,
#' and no `manifest.json` is written when the manifest cannot be built. The
#' seal is written last, over the final bytes of every other listed file, and
#' is listed only when it was written; the methods note lists it only then.
#' A partial archive is still sealed. A checksum identifies bytes; it does not
#' establish that the run's record is complete. A destination directory that
#' cannot be created, or a failed file that cannot be removed, is an error.
#'
#' @param run An object accepted by [as_agent_run()] (an [Agent], a conversation
#'   or preset result, a pipeline, an experiment, or an `agent_run`).
#' @param path Directory to write into; created (recursively) if absent.
#' @param overwrite If `FALSE` (default), refuse a non-empty `path`. Set to
#'   `TRUE` to replace archive files in an existing directory; other files in
#'   it are left alone.
#' @param include_messages If `TRUE` (default), write the transcript and the
#'   free-text columns of the tool and state tables. If `FALSE`, those tables
#'   are still written but their free-text columns are replaced by
#'   `"[OMITTED]"`, keeping only structure, hashes, and metadata; the records in
#'   `calls.jsonl` likewise omit the request body and reply text (each record
#'   keeps its precomputed `request_hash`, so the join invariant holds).
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
#'   privacy-filtered, data-only run snapshot as `run.rds`). Defaults to all
#'   three.
#' @return Invisibly, an object of class `agent_archive`: a list with `path`,
#'   `files` (relative paths written), `manifest_hash` (`NA` when the manifest
#'   could not be built), `n_calls` (`NA` when the call records could not be
#'   read), `calls_source` (where `calls.jsonl` came from, or `"none"`),
#'   `llm_log_compatible` (whether `calls.jsonl` is an LLMR audit log), and
#'   `write_failures` (a tibble with columns `file` and `error`, with no rows
#'   when every file was written). Print lists what was written and whether
#'   it was sealed.
#' @seealso [agent_manifest()], [as_agent_run()], [LLMR::llm_log_read()]
#' @examples
#' a <- agent("Aria", LLMR::llm_config("groq", "openai/gpt-oss-20b"), quiet = TRUE)
#' a$memory$add("user", "Hello")
#' dir <- file.path(tempdir(), "aria-study")
#' arch <- archive_agent_study(a, dir, include_messages = FALSE)
#' arch
#' arch$write_failures
#' unlink(dir, recursive = TRUE)
#'
#' \dontrun{
#' a$chat("Hello")
#' archive_agent_study(a, tempfile("study_"))
#' }
#' @export
archive_agent_study <- function(run, path, include_messages = TRUE,
                                redact = NULL,
                                formats = c("csv", "jsonl", "rds"),
                                overwrite = FALSE) {
  if (dir.exists(path) &&
      length(list.files(path, all.files = TRUE, no.. = TRUE)) &&
      !isTRUE(overwrite)) {
    stop("`path` is not empty; set `overwrite = TRUE` to replace archive files.",
         call. = FALSE)
  }
  formats <- match.arg(formats, several.ok = TRUE)
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE, showWarnings = FALSE)) {
    stop("The archive directory could not be created: ", path, call. = FALSE)
  }
  r <- as_agent_run(run)

  redactor <- .make_redactor(redact)
  files <- character(0)

  write_failures <- tibble::tibble(file = character(), error = character())
  write_step <- function(file, expr) {
    dest <- file.path(path, file)
    # Remove an older destination before writing so success describes this write.
    .remove_archive_output(dest)
    out <- tryCatch({
      value <- force(expr)
      if (!file.exists(dest) || dir.exists(dest)) stop("The writer did not produce a file.")
      list(ok = TRUE, value = value)
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
    if (out$ok) {
      files <<- unique(c(files, file))
      return(out$value)
    }
    .remove_archive_output(dest)
    files <<- setdiff(files, file)
    write_failures <<- rbind(write_failures,
      tibble::tibble(file = file, error = out$error))
    NULL
  }
  extracted <- lapply(c("utterance", "event", "call", "tool", "state"), function(level)
    .read_run_level(r, level, warn = FALSE))
  names(extracted) <- c("utterance", "event", "call", "tool", "state")
  archive_levels <- lapply(names(extracted), function(level) {
    out <- extracted[[level]]
    if (!is.null(out$error)) return(out)
    tryCatch(list(data = if (level == "event") out$data else
      .redact_cols(out$data, redactor, include_messages), error = NULL),
      error = function(e) list(data = NULL, error = conditionMessage(e)))
  })
  names(archive_levels) <- names(extracted)
  lvl <- function(level) {
    out <- archive_levels[[level]]
    if (!is.null(out$error)) stop(out$error, call. = FALSE)
    out$data
  }

  man <- NULL
  write_step("manifest.json", {
    man <- agent_manifest(r)
    body <- lapply(unclass(man), function(el)
      if (is.data.frame(el)) as.data.frame(el) else el)
    txt <- jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE,
                            null = "null", na = "null")
    .write_lines(as.character(txt), file.path(path, "manifest.json"))
  })
  manifest_hash <- man$manifest_hash %||% NA_character_
  if ("csv" %in% formats) {
    write_step("transcript.csv", utils::write.csv(lvl("utterance"),
      file.path(path, "transcript.csv"), row.names = FALSE))
  }
  write_step("events.jsonl", .write_jsonl(lvl("event"), file.path(path, "events.jsonl")))

  # ---- calls.jsonl (per-call provenance; a true LLMR audit log) -------------
  # Always a file LLMR::llm_log_read() can parse. When a live audit log was
  # active during the run, this run's records are copied from it verbatim
  # (their request hashes must still match a live call). Otherwise, and
  # whenever the run's records cannot be picked out of the live log, the
  # per-call spans (which carry the rendered request body, the served model id,
  # usage, and the reply) are rendered into the same audit-record schema, so
  # the request body is real, not reconstructed. The whole live log is never
  # copied: it can hold other studies' calls.
  calls_source <- write_step("calls.jsonl", {
    calls <- lvl("call")
    dest <- file.path(path, "calls.jsonl")
    log <- r$llmr_log
    if (!is.null(log) && length(log) == 1L && !is.na(log) && file.exists(log)) {
      run_hashes <- tryCatch(
        stats::na.omit(calls$request_hash),
        error = function(e) character(0))
      run_ids <- tryCatch(
        stats::na.omit(calls$response_id),
        error = function(e) character(0))
      n_kept <- if (length(run_hashes)) {
        .filter_log_to_hashes(log, dest, run_hashes, run_ids)
      } else NA_integer_
      if (is.na(n_kept)) {
        warning(if (length(run_hashes)) {
          "The active LLMR audit log could not be read, so calls.jsonl is rendered from the run's own call records instead of copied from the log."
        } else {
          "This run's calls carry no request hashes to match in the active LLMR audit log, so calls.jsonl is rendered from the run's own call records instead of copied from the log."
        }, call. = FALSE)
        .write_audit_calls(r, dest, include_messages = include_messages,
                           redactor = redactor)
      } else {
        # The privacy levers apply to the copied lines too: include_messages =
        # FALSE strips each record's request body and reply text (the
        # precomputed request_hash is kept, so the join survives); a redactor
        # scrubs the reply text. Only with neither lever is the copy
        # byte-for-byte.
        if (!isTRUE(include_messages) || !is.null(redact)) {
          .privacy_scrub_log(dest, include_messages = include_messages,
                             redactor = redactor)
        }
        .warn_log_selection(n_kept, length(run_hashes))
        if (identical(as.vector(n_kept), 0L)) {
          # The live log had none of this run's calls (e.g. it was rotated or
          # belongs to another session), so calls.jsonl is empty and the
          # archive must not claim live-log provenance for its calls.
          warning("The active LLMR audit log contains no records matching this ",
                  "run's request hashes; calls.jsonl is empty and the archive ",
                  "is marked accordingly.", call. = FALSE)
          "live_llmr_log_empty"
        } else {
          "live_llmr_log"
        }
      }
    } else {
      .write_audit_calls(r, dest, include_messages = include_messages,
                         redactor = redactor)
    }
  }) %||% "none"

  if ("csv" %in% formats) {
    write_step("tools.csv", utils::write.csv(lvl("tool"),
      file.path(path, "tools.csv"), row.names = FALSE))
    write_step("state.csv", utils::write.csv(lvl("state"),
      file.path(path, "state.csv"), row.names = FALSE))
  }
  if ("rds" %in% formats) {
    write_step("run.rds", {
      views <- lapply(names(archive_levels), lvl)
      names(views) <- names(archive_levels)
      snapshot <- structure(c(list(
        schema = "llmragent_archive_run/1", run_id = r$run_id, kind = r$kind,
        created_at = r$created_at, manifest_hash = manifest_hash,
        participants = tibble::as_tibble(r$participants), pkg_versions = r$pkg_versions),
        views), class = "agent_archive_run")
      saveRDS(snapshot, file.path(path, "run.rds"))
    })
  }
  .write_artifacts(r$artifacts %||% list(), path, write_step)
  readme <- "README-methods.md"
  write_step(readme, .write_readme(r, man, unique(c(files, readme, "hashes.sha256")),
                                  file.path(path, readme), extracted$call))
  write_step("hashes.sha256", {
    lines <- vapply(files, function(rel) {
      hash <- .file_sha256(file.path(path, rel))
      if (length(hash) != 1L || is.na(hash) || !grepl("^[0-9a-f]{64}$", hash)) {
        stop("Invalid checksum for '", rel, "'.", call. = FALSE)
      }
      sprintf("%s  %s", hash, rel)
    }, character(1), USE.NAMES = FALSE)
    .write_lines(lines, file.path(path, "hashes.sha256"))
  })
  if (!"hashes.sha256" %in% files && readme %in% files) {
    write_step(readme, .write_readme(r, man, files, file.path(path, readme), extracted$call))
  }
  if (nrow(write_failures)) warning(
    "Some archive files could not be produced.\n",
    paste0("  ", write_failures$file, ": ", write_failures$error, collapse = "\n"),
    call. = FALSE)
  invisible(structure(
    list(path = path, files = files, manifest_hash = manifest_hash,
         n_calls = if (is.null(extracted$call$error)) as.integer(nrow(extracted$call$data)) else NA_integer_,
         calls_source = calls_source,
         llm_log_compatible = calls_source %in% c("live_llmr_log", "audit_from_spans"),
         write_failures = write_failures),
    class = "agent_archive"))
}

.remove_archive_output <- function(file) {
  if (file.exists(file) && (unlink(file) != 0L || file.exists(file))) {
    stop("The failed archive output could not be removed: ", file, call. = FALSE)
  }
  invisible(NULL)
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
# match is on the same identity the run's call level carries. An identical
# request sent earlier (a pilot, another study) has the same hash, so a record
# that carries a response_id is kept only when that id is one of `keep_ids`.
# Returns the number of records written, or NA without writing anything when
# the log cannot be read, so the caller never copies an unfiltered log.
#' @keywords internal
#' @noRd
.filter_log_to_hashes <- function(log, dest, keep_hashes, keep_ids = character(0)) {
  parsed <- tryCatch(LLMR::llm_log_read(log), error = function(e) NULL)
  if (is.null(parsed) || is.null(parsed$manifest) || !nrow(parsed$manifest)) {
    return(invisible(NA_integer_))
  }
  man <- parsed$manifest
  rec_ids <- vapply(parsed$records, function(r) {
    id <- r$rec$response_id
    if (is.character(id) && length(id) == 1L) id else NA_character_
  }, character(1))
  id_ok <- if (length(keep_ids)) is.na(rec_ids) | rec_ids %in% keep_ids else TRUE
  keep_idx <- man$idx[man$request_hash %in% keep_hashes & id_ok]
  raw_lines <- vapply(parsed$records, function(r) r$raw %||% "", character(1))
  out_lines <- raw_lines[keep_idx]
  con <- file(dest, open = "wb")
  on.exit(close(con), add = TRUE)
  if (length(out_lines)) writeLines(enc2utf8(out_lines), con, useBytes = TRUE)
  # A record kept on its request hash alone (it, or every one of the run's
  # calls, lacks a response id) cannot be told apart from an earlier identical
  # request.
  hash_only <- if (length(keep_ids)) sum(is.na(rec_ids[keep_idx])) else length(keep_idx)
  invisible(structure(length(out_lines), n_hash_only = as.integer(hash_only)))
}

# Warn when the records copied from a live audit log may not be this run's
# alone: more of them than the run has hashed calls, or records that could be
# matched on request hash only.
.warn_log_selection <- function(n_kept, n_run_calls) {
  hash_only <- attr(n_kept, "n_hash_only") %||% 0L
  parts <- character(0)
  if (n_kept > n_run_calls) {
    parts <- c(parts, sprintf(
      "calls.jsonl keeps %d record(s) from the live LLMR audit log for %d call(s) of this run",
      as.integer(n_kept), as.integer(n_run_calls)))
  }
  if (hash_only > 0L) {
    parts <- c(parts, sprintf(paste0(
      "%d of the copied record(s) were matched on request hash alone because a ",
      "response id is missing, so an earlier identical request can be among them"),
      hash_only))
  }
  if (length(parts)) warning(paste0(paste(parts, collapse = "; "), "."), call. = FALSE)
  invisible(NULL)
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
  completed <- FALSE
  on.exit(if (!completed) .remove_archive_output(dest), add = TRUE)
  tryCatch({
  parsed <- LLMR::llm_log_read(dest)
  if (!length(parsed$records)) { completed <- TRUE; return(invisible(dest)) }
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
  completed <- TRUE
  invisible(dest)
  }, error = function(e) stop("the privacy options could not be applied (",
      conditionMessage(e), "), so the copied call records were removed", call. = FALSE))
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
.write_artifacts <- function(artifacts, path, write_step) {
  nms <- names(artifacts) %||% rep("", length(artifacts))
  for (i in seq_along(artifacts)) {
    nm <- if (!is.na(nms[i]) && nzchar(nms[i])) nms[i] else paste0("artifact_", i)
    safe <- gsub("[^A-Za-z0-9._-]+", "_", nm)
    el <- artifacts[[i]]
    rel <- file.path("artifacts", paste0(safe, if (is.data.frame(el)) ".csv" else ".json"))
    write_step(rel, {
      dir.create(file.path(path, "artifacts"), recursive = TRUE, showWarnings = FALSE)
      if (is.data.frame(el)) {
        utils::write.csv(el, file.path(path, rel), row.names = FALSE)
      } else {
        txt <- jsonlite::toJSON(el, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
        .write_lines(as.character(txt), file.path(path, rel))
      }
    })
  }
  invisible(NULL)
}

# Compose README-methods.md: a design header, the drafted methods paragraph,
# and the list of files in the archive.
#' @keywords internal
#' @noRd
.write_readme <- function(r, man, files, file, extracted = .read_run_level(r, "call")) {
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

  methods <- .run_methods(extracted)

  file_list <- c("", "## Files in this archive", "",
                 paste0("- ", files))

  .write_lines(c(header, methods, file_list), file)
  invisible(file)
}

# ---- hashing ----------------------------------------------------------------

# SHA-256 over a file's raw bytes, without R serialization (stable across R
# versions), matching the LLMR audit-log line-hash convention.
#' @keywords internal
#' @noRd
.file_sha256 <- function(f) {
  tryCatch({
    if (!file.exists(f) || dir.exists(f)) stop("file is missing or is not a regular file")
    if (file.access(f, 4L) != 0L) stop("file is not readable")
    sz <- file.size(f)
    if (is.na(sz)) stop("file size is unavailable")
    bytes <- readBin(f, "raw", n = sz)
    if (length(bytes) != sz) stop("file could not be read completely")
    digest::digest(bytes, algo = "sha256", serialize = FALSE)
  }, error = function(e) stop(sprintf("The checksum for '%s' could not be computed: %s",
                                     f, conditionMessage(e)), call. = FALSE))
}

# ---- print ------------------------------------------------------------------

#' @rdname archive_agent_study
#' @param x An `agent_archive`.
#' @param ... Unused.
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
  if ("hashes.sha256" %in% x$files) {
    cat("  sealed by hashes.sha256 (SHA-256 over each file's bytes)\n")
  } else cat("  No checksum file was produced.\n")
  invisible(x)
}
