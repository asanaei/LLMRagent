# sandbox.R --------------------------------------------------------------------
# Confined tools: an llm_tool() whose execution is bounded in wall-clock time,
# result size, and filesystem reach, and whose input and output files are
# hashed for provenance. A confined tool IS an llmr_tool (same class, same
# fields), so agent(tools=) and LLMR::call_llm_tools() consume it unchanged;
# it also carries a "governance" attribute, like agent_tool().
#
# The confinement is performed by an EXECUTOR: a function that runs the user's
# function somewhere bounded (a child R process, a container, a remote host)
# and reports back what it produced and which files it touched. The default
# executor spawns a child R process via callr; an injected executor= lets the
# whole apparatus be exercised offline, with nothing spawned, mirroring the
# caller=/transport= seams used elsewhere in this package.

#' Define a confined (sandboxed) tool
#'
#' Like [LLMR::llm_tool()], but the tool runs under confinement: a wall-clock
#' timeout, a cap on result size, and auditing of where it writes. Each
#' invocation hashes the input files it was handed and the output files it
#' produced, so a tool that touches the filesystem leaves an auditable trail.
#' The returned object is an ordinary `llmr_tool`, so it passes to [agent()]
#' and the tool loop exactly as a plain tool does.
#'
#' Confinement is carried out by an *executor*, a function that runs the user
#' function in a bounded place and reports what it produced. The default
#' executor runs the function in a child R process (via the `callr` package),
#' which is killed when the timeout elapses; `mode = "container"` requires an
#' executor to be supplied, because the package does not assume any particular
#' container runtime. Supplying `executor` directly is also how the tool is
#' tested offline: a fake executor returns a canned result and file list, so
#' the size, timeout, and path checks can be exercised without spawning
#' anything.
#'
#' **What the default executor actually guarantees.** The default child-process
#' executor runs the user function with its working directory set to a fresh
#' scratch directory, then snapshots that directory before and after the call to
#' hash every file written *under it*. Relative writes that stay under the
#' working directory therefore land in the scratch directory and are hashed and
#' checked against `allow_paths`. The default executor does **not** establish a
#' hard filesystem boundary: a write that leaves the scratch directory -- an
#' absolute path (e.g. `/tmp/out`, `$HOME/out`) or an upward-traversing relative
#' path (e.g. `../out`) -- happens in the same operating-system namespace as the
#' parent and is *not* intercepted or snapshotted, so
#' `llmragent_sandbox_violation` cannot fire for a write the executor never
#' sees. The guarantee is therefore "audit and check the writes the executor
#' *reports* (with the default executor: writes remaining under the scratch
#' working directory)", not "block writes elsewhere". As a best-effort flag,
#' any *reported* write, and any `allow_paths` entry, that resolves outside the
#' scratch working directory is recorded in the result's `sandbox` attribute
#' (field `outside_workdir`), so a call that sanctioned or performed an
#' out-of-scratch write is visible in the provenance rather than silent. To
#' enforce a real boundary against arbitrary paths, supply a
#' `mode = "container"` executor (or an OS-level sandbox) that runs the function
#' in a confined namespace and reports the files it wrote; the `allow_paths`
#' check then applies to whatever that executor reports.
#'
#' The executor contract is `executor(fn, args, workdir, timeout_s)` returning
#' a list with elements `stdout` (character), `result` (the value, or `NULL`),
#' `files` (a named character vector mapping written paths to content hashes,
#' or a bare character vector of written paths), `status` (one of `"ok"`,
#' `"timeout"`, `"error"`), and `error` (a message, or `NA`).
#'
#' @param fn The function to expose, or an existing `llmr_tool` to confine. When
#'   an `llmr_tool` is given, its own function, name, description, and schema
#'   are reused and the remaining `name`/`description`/`parameters`/`required`
#'   arguments are ignored.
#' @param name Tool name shown to the model. Defaults to `"sandboxed_tool"`.
#' @param description One or two sentences for the model. Defaults to
#'   `"A sandboxed tool."`.
#' @param parameters A named list of JSON-Schema properties, or a full schema
#'   object (as in [LLMR::llm_tool()]).
#' @param required Character vector of required argument names.
#' @param mode The confinement regime. `"read_only"`: the child runs with its
#'   working directory set to a scratch directory; relative writes that stay
#'   under it land there and
#'   are hashed and checked against `allow_paths`, and any *reported* write
#'   outside `allow_paths` is a violation (the default executor cannot intercept
#'   writes that leave the scratch directory; see Details). `"tempdir"`: the scratch directory is
#'   the sanctioned writable location and is always permitted; reported writes
#'   elsewhere are violations. `"container"`: confinement is delegated entirely to
#'   a supplied `executor`, which is the way to obtain a hard filesystem boundary.
#' @param timeout_s Wall-clock limit per call, in seconds (default `30`). The
#'   default executor kills the child process when it elapses; the call then
#'   reports a `"timeout"` status.
#' @param max_bytes Maximum result size in bytes; a larger result (or captured
#'   output) is truncated and flagged. Default `1e6`.
#' @param allow_paths Character vector of directories outside which a *reported*
#'   written file is a violation. The scratch working directory is always
#'   permitted (in `"tempdir"` mode). The check applies only to files the
#'   executor reports; with the default executor that means files written under
#'   the scratch directory, since writes that leave it (absolute or
#'   `../`-traversing paths) are not seen (see Details). Default `NULL` (only
#'   the scratch directory is allowed).
#' @param env How much of the ambient environment the child sees. Recorded for
#'   provenance and passed to the executor; the default executor treats
#'   `"minimal"` as a hint and does not inherit the parent's global objects.
#' @param executor A function `(fn, args, workdir, timeout_s)` implementing the
#'   contract above. When `NULL`, a default child-process executor is built for
#'   `"read_only"`/`"tempdir"` (requires `callr`; without it the function runs
#'   in-process with a one-time warning that confinement is weak), and
#'   `"container"` mode raises an error asking for an executor.
#' @return An `llmr_tool` carrying a `"governance"` attribute whose `sandbox`
#'   element records the mode and `allow_paths`. Each call returns the tool's
#'   result string with a `"sandbox"` attribute recording the mode, duration,
#'   byte count, input and output file hashes, status, and `outside_workdir`
#'   (reported writes and `allow_paths` entries resolving outside the scratch
#'   working directory; see Details).
#' @seealso [agent_tool()], [LLMR::llm_tool()], [agent()]
#' @examples
#' \dontrun{
#' # A tool that runs in a killed-on-timeout child R process.
#' slow <- sandbox_tool(
#'   function(n) { Sys.sleep(n); "done" },
#'   name = "wait", description = "Sleeps for n seconds, then returns.",
#'   parameters = list(n = list(type = "number")),
#'   mode = "tempdir", timeout_s = 2
#' )
#' slow$fn(n = 10)  # reports a timeout rather than blocking
#'
#' # Offline: an injected executor needs no process at all.
#' double <- sandbox_tool(
#'   function(x) x * 2, name = "double", description = "Doubles x.",
#'   parameters = list(x = list(type = "number")), mode = "tempdir",
#'   executor = function(fn, args, workdir, timeout_s)
#'     list(stdout = "", result = do.call(fn, args),
#'          files = character(0), status = "ok", error = NA)
#' )
#' double$fn(x = 21)
#' }
#' @export
sandbox_tool <- function(fn, name = NULL, description = NULL,
                         parameters = NULL, required = NULL,
                         mode = c("read_only", "tempdir", "container"),
                         timeout_s = 30, max_bytes = 1e6,
                         allow_paths = NULL, env = "minimal",
                         executor = NULL) {
  mode <- match.arg(mode)

  # Resolve the user function and its tool metadata. An existing llmr_tool is
  # re-confined: we wrap its own $fn and keep its name/description/schema.
  if (inherits(fn, "llmr_tool")) {
    user_fn <- fn$fn
    tool_name <- fn$name
    tool_desc <- fn$description
    tool_schema <- fn$schema
    have_schema <- TRUE
  } else {
    stopifnot(is.function(fn))
    user_fn <- fn
    tool_name <- name %||% "sandboxed_tool"
    tool_desc <- description %||% "A sandboxed tool."
    have_schema <- FALSE
  }

  # The default executor. For container mode there is no sane default, because
  # the package does not assume Docker (or any runtime); the caller must say
  # how to run things. For the file-system modes, prefer a real child process.
  if (is.null(executor)) {
    if (identical(mode, "container")) {
      rlang::abort(
        message = paste0(
          "sandbox_tool(mode = \"container\") needs an `executor=` function: ",
          "the package does not assume a container runtime (e.g. Docker). ",
          "Supply executor(fn, args, workdir, timeout_s) that runs `fn` in ",
          "your container and returns list(stdout, result, files, status, error)."),
        class = c("llmragent_sandbox_error", "error", "condition"))
    }
    executor <- .sandbox_default_executor(env = env)
  }
  stopifnot(is.function(executor))

  side_effects <- if (identical(mode, "read_only")) "read" else "write"
  warned <- new.env(parent = emptyenv())   # one-time weak-confinement warning
  warned$done <- FALSE

  wrapped <- function(...) {
    args <- list(...)
    workdir <- .sandbox_workdir(mode)
    on.exit(.sandbox_cleanup(workdir), add = TRUE)

    # Hash any argument that looks like an existing input file, before running.
    in_hashes <- .sandbox_hash_inputs(args)

    t0 <- Sys.time()
    res <- tryCatch(
      executor(user_fn, args, workdir, timeout_s),
      error = function(e)
        list(stdout = "", result = NULL, files = character(0),
             status = "error", error = conditionMessage(e)))
    duration <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

    res <- .sandbox_normalize(res)
    if (isTRUE(res$weak_confinement) && !warned$done) {
      warned$done <- TRUE
      warning(sprintf(
        "sandbox_tool('%s'): 'callr' is not installed; running in-process. ",
        tool_name),
        "Time and filesystem confinement are limited without a child process.",
        call. = FALSE)
    }

    status <- res$status
    out_hashes <- .sandbox_files_to_hashes(res$files, workdir)

    # Enforce the write policy on the files the executor REPORTS: any reported
    # write outside the permitted set is a violation. The scratch workdir is
    # always permitted in tempdir mode. With the default executor this covers
    # writes under the workdir; an absolute-path write the executor never saw
    # cannot be caught here -- a container-mode executor is required for that.
    written <- names(out_hashes)
    bad <- .sandbox_path_violations(written, allow_paths = allow_paths,
                                    workdir = workdir, mode = mode)
    if (length(bad)) {
      rlang::abort(
        message = sprintf(
          "sandbox_tool('%s') wrote outside the permitted paths: %s",
          tool_name, paste(bad, collapse = ", ")),
        class = c("llmragent_sandbox_violation", "error", "condition"),
        tool = tool_name, paths = bad, allow_paths = allow_paths)
    }

    # Best-effort escape flag: any reported write, and any allow_paths entry,
    # that resolves outside the scratch workdir is recorded in the provenance.
    # This is a flag, not a fence -- the default executor cannot see writes
    # that left the workdir -- but a call that sanctioned or performed an
    # out-of-scratch write must not read as contained.
    outside <- .sandbox_outside_workdir(c(written, allow_paths), workdir)

    # Stringify the result, then enforce the byte cap on it (and on stdout).
    out <- .sandbox_stringify(res$result, res$stdout, status, res$error,
                              tool_name, timeout_s)
    bytes <- nchar(out, type = "bytes")
    if (is.finite(max_bytes) && bytes > max_bytes) {
      keep <- max(0L, as.integer(max_bytes))
      out <- paste0(substr(out, 1L, keep),
                    sprintf(" ...[truncated to %s bytes]", format(max_bytes)))
      bytes <- nchar(out, type = "bytes")
    }

    attr(out, "sandbox") <- list(
      mode = mode, duration = duration, bytes = bytes,
      in_hashes = in_hashes, out_hashes = out_hashes, status = status,
      outside_workdir = outside)
    out
  }

  tool <- if (have_schema) {
    # Reuse the original schema verbatim by rebuilding through llm_tool with the
    # schema's properties/required, so the class and fields stay canonical.
    props <- tool_schema$properties %||% tool_schema[["function"]]$parameters$properties
    req   <- tool_schema$required %||% tool_schema[["function"]]$parameters$required
    LLMR::llm_tool(wrapped, name = tool_name, description = tool_desc,
                   parameters = props, required = req)
  } else {
    LLMR::llm_tool(wrapped, name = tool_name, description = tool_desc,
                   parameters = parameters, required = required)
  }

  attr(tool, "governance") <- list(
    side_effects = side_effects, requires_approval = FALSE,
    timeout_s = timeout_s, max_calls = Inf, max_bytes = max_bytes,
    sandbox = list(mode = mode, allow_paths = allow_paths))
  tool
}

# Build a default executor for read_only/tempdir modes. Prefers a child R
# process (callr), which can be killed on timeout and runs with its working
# directory set to the scratch workdir so that RELATIVE writes land there and
# become visible to the before/after snapshot. This executor confines and audits
# writes WITHIN the workdir only; it cannot intercept ABSOLUTE-path writes
# elsewhere (those happen in the same OS namespace and are never snapshotted) --
# a hard boundary requires a container-mode executor. Falls back to running
# in-process and flags that confinement is weak so the wrapper can warn once.
#' @keywords internal
#' @noRd
.sandbox_default_executor <- function(env = "minimal") {
  has_callr <- requireNamespace("callr", quietly = TRUE)
  function(fn, args, workdir, timeout_s) {
    before <- .sandbox_snapshot(workdir)
    if (has_callr) {
      out <- tryCatch(
        {
          val <- callr::r(
            func = function(fn, args, workdir) {
              old <- setwd(workdir); on.exit(setwd(old), add = TRUE)
              utils::capture.output(value <- do.call(fn, args))
              value
            },
            args = list(fn = fn, args = args, workdir = workdir),
            timeout = timeout_s, spinner = FALSE)
          list(value = val, status = "ok", error = NA_character_)
        },
        error = function(e) {
          msg <- conditionMessage(e)
          is_to <- inherits(e, "callr_timeout_error") ||
            grepl("timed?[ _]?out|timeout", msg, ignore.case = TRUE)
          list(value = NULL,
               status = if (is_to) "timeout" else "error",
               error = msg)
        })
      written <- .sandbox_new_files(workdir, before)
      list(stdout = "", result = out$value, files = written,
           status = out$status, error = out$error)
    } else {
      out <- tryCatch(
        {
          old <- setwd(workdir); on.exit(setwd(old), add = TRUE)
          txt <- utils::capture.output(value <- do.call(fn, args))
          list(value = value, stdout = paste(txt, collapse = "\n"),
               status = "ok", error = NA_character_)
        },
        error = function(e)
          list(value = NULL, stdout = "",
               status = "error", error = conditionMessage(e)))
      written <- .sandbox_new_files(workdir, before)
      list(stdout = out$stdout, result = out$value, files = written,
           status = out$status, error = out$error,
           weak_confinement = TRUE)
    }
  }
}

# A scratch directory for one call. In tempdir mode it is the only writable
# place; in read_only mode it is a place the executor may use but writes
# elsewhere are violations.
#' @keywords internal
#' @noRd
.sandbox_workdir <- function(mode) {
  d <- tempfile(pattern = "llmragent_sandbox_")
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  normalizePath(d, winslash = "/", mustWork = FALSE)
}

#' @keywords internal
#' @noRd
.sandbox_cleanup <- function(workdir) {
  if (!is.null(workdir) && dir.exists(workdir))
    unlink(workdir, recursive = TRUE, force = TRUE)
  invisible(NULL)
}

# Snapshot of files under a directory (for detecting what an executor wrote).
#' @keywords internal
#' @noRd
.sandbox_snapshot <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(character(0))
  list.files(dir, recursive = TRUE, all.files = TRUE, no.. = TRUE,
             full.names = TRUE)
}

#' @keywords internal
#' @noRd
.sandbox_new_files <- function(dir, before) {
  after <- .sandbox_snapshot(dir)
  setdiff(after, before)
}

# Normalize an executor's return into the contract shape, tolerating missing
# fields and carrying a weak-confinement flag through.
#' @keywords internal
#' @noRd
.sandbox_normalize <- function(res) {
  if (!is.list(res)) {
    return(list(stdout = "", result = res, files = character(0),
                status = "ok", error = NA_character_,
                weak_confinement = FALSE))
  }
  status <- res$status %||% "ok"
  if (!status %in% c("ok", "timeout", "error")) status <- "ok"
  list(
    stdout = res$stdout %||% "",
    result = if ("result" %in% names(res)) res$result else NULL,
    files  = res$files %||% character(0),
    status = status,
    error  = res$error %||% NA_character_,
    weak_confinement = isTRUE(res$weak_confinement))
}

# Coerce the executor's `files` (named path->hash, or a bare path vector) into a
# named character vector of path -> content hash, hashing any unhashed files
# that still exist on disk.
#' @keywords internal
#' @noRd
.sandbox_files_to_hashes <- function(files, workdir = NULL) {
  if (is.null(files) || length(files) == 0L) return(character(0))
  nms <- names(files)               # capture before coercion strips them
  files <- as.character(files)
  if (is.null(nms)) {
    # Bare vector of paths: treat each entry as a path, hash if it exists.
    paths <- files
    hashes <- vapply(paths, .sandbox_hash_file, character(1))
    names(hashes) <- paths
    return(hashes)
  }
  # Named: keep provided hashes; fill blanks by hashing the path when possible.
  out <- files
  blank <- !nzchar(out) | is.na(out)
  if (any(blank))
    out[blank] <- vapply(nms[blank], .sandbox_hash_file, character(1))
  stats::setNames(out, nms)
}

# Hash a single file's bytes (NA if it does not exist or cannot be read).
#' @keywords internal
#' @noRd
.sandbox_hash_file <- function(path) {
  if (is.na(path) || !nzchar(path) || !file.exists(path) || dir.exists(path))
    return(NA_character_)
  tryCatch({
    bytes <- readBin(path, "raw", n = file.info(path)$size %||% 0L)
    .sandbox_hash_bytes(bytes)
  }, error = function(e) NA_character_)
}

#' @keywords internal
#' @noRd
.sandbox_hash_bytes <- function(bytes) {
  if (exists("llm_hash", where = asNamespace("LLMR"), inherits = FALSE)) {
    return(tryCatch(as.character(LLMR::llm_hash(bytes)),
                    error = function(e) digest::digest(bytes, serialize = FALSE)))
  }
  digest::digest(bytes, serialize = FALSE)
}

# Hash any argument that is a length-1 string naming an existing file. These are
# the tool's input files; their hashes go into the provenance record.
#' @keywords internal
#' @noRd
.sandbox_hash_inputs <- function(args) {
  out <- character(0)
  flat <- unlist(args, use.names = FALSE)
  if (length(flat) == 0L) return(out)
  cand <- flat[is.character(flat)]
  cand <- unique(cand[nzchar(cand)])
  for (p in cand) {
    if (file.exists(p) && !dir.exists(p)) {
      out[p] <- .sandbox_hash_file(p)
    }
  }
  out
}

# Which of these paths resolve OUTSIDE the scratch workdir? Used for the
# best-effort provenance flag: a reported write, or a sanctioned allow_paths
# entry, outside the workdir means the call was not contained to the scratch
# directory, and the result must say so rather than staying silent.
#' @keywords internal
#' @noRd
.sandbox_outside_workdir <- function(paths, workdir) {
  paths <- unique(paths[!is.na(paths) & nzchar(paths)])
  if (!length(paths) || is.null(workdir) || !nzchar(workdir)) return(character(0))
  wd <- .sandbox_realize(workdir)
  norm <- .sandbox_norm(paths)
  paths[!vapply(norm, .sandbox_under, logical(1), root = wd)]
}

# Which written paths violate the policy? A path is permitted if it lies under
# the scratch workdir (tempdir mode only) or under any allow_paths entry.
#' @keywords internal
#' @noRd
.sandbox_path_violations <- function(written, allow_paths, workdir, mode) {
  if (length(written) == 0L) return(character(0))
  permitted <- character(0)
  if (identical(mode, "tempdir") && !is.null(workdir)) permitted <- workdir
  if (length(allow_paths)) permitted <- c(permitted, allow_paths)
  permitted <- .sandbox_norm(permitted)
  written_n <- .sandbox_norm(written)
  ok <- vapply(written_n, function(w) any(vapply(permitted, function(root)
    .sandbox_under(w, root), logical(1))), logical(1))
  written[!ok]
}

#' @keywords internal
#' @noRd
.sandbox_norm <- function(paths) {
  if (length(paths) == 0L) return(character(0))
  vapply(paths, .sandbox_realize, character(1))
}

# Canonicalize a single path so that an existing directory and a (possibly
# non-existent) file beneath it agree. normalizePath() only resolves symlinks
# (e.g. macOS /var -> /private/var) and collapses "//" for paths that exist; an
# unwritten target keeps its raw form and would never compare equal to its
# resolved parent. So resolve the longest existing ancestor, then re-attach the
# remainder.
#' @keywords internal
#' @noRd
.sandbox_realize <- function(p) {
  if (is.na(p) || !nzchar(p)) return(p)
  resolved <- tryCatch(normalizePath(p, winslash = "/", mustWork = TRUE),
                       error = function(e) NA_character_)
  if (!is.na(resolved)) return(resolved)
  # Walk up to the nearest existing ancestor, resolve it, re-append the tail.
  parts <- character(0)
  cur <- p
  repeat {
    parent <- dirname(cur)
    if (identical(parent, cur)) break          # reached the root
    parts <- c(basename(cur), parts)
    if (file.exists(parent)) {
      base <- tryCatch(normalizePath(parent, winslash = "/", mustWork = TRUE),
                       error = function(e) parent)
      return(paste(c(base, parts), collapse = "/"))
    }
    cur <- parent
  }
  # No existing ancestor: fall back to a best-effort lexical normalization.
  tryCatch(normalizePath(p, winslash = "/", mustWork = FALSE),
           error = function(e) p)
}

# Is path `w` inside directory `root` (or equal to it)?
#' @keywords internal
#' @noRd
.sandbox_under <- function(w, root) {
  if (is.na(w) || is.na(root) || !nzchar(root)) return(FALSE)
  root_slash <- if (endsWith(root, "/")) root else paste0(root, "/")
  identical(w, root) || startsWith(w, root_slash)
}

# Turn the result (plus status and captured output) into the single string the
# tool loop expects. Timeouts and errors become explicit, model-readable lines.
#' @keywords internal
#' @noRd
.sandbox_stringify <- function(result, stdout, status, error,
                               tool_name, timeout_s) {
  if (identical(status, "timeout")) {
    return(sprintf("TIMEOUT: tool '%s' exceeded the %ss sandbox limit.",
                   tool_name, format(timeout_s)))
  }
  if (identical(status, "error")) {
    msg <- if (is.na(error) || !nzchar(error)) "unknown error" else error
    return(sprintf("ERROR: tool '%s' failed in the sandbox: %s",
                   tool_name, msg))
  }
  body <- if (is.character(result) && length(result) == 1L) {
    result
  } else if (is.null(result)) {
    ""
  } else {
    tryCatch(
      as.character(jsonlite::toJSON(result, auto_unbox = TRUE, null = "null")),
      error = function(e)
        paste(utils::capture.output(print(result)), collapse = "\n"))
  }
  if (nzchar(stdout) && !nzchar(body)) body <- stdout
  body
}
