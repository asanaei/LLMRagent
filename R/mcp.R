# mcp.R -----------------------------------------------------------------------
# An optional, governed Model Context Protocol (MCP) client. MCP lets an agent
# reach external tools and data, but the 2026 security record (tool poisoning,
# line jumping, rug pulls, confused-deputy) makes governance the whole point.
# So this client is conservative by construction: read-only by default, writes
# pass a human gate, tool schemas are pinned and re-checked (rug-pull defense),
# server-supplied descriptions are treated as untrusted data and scanned for
# injection (line-jumping defense), and every schema/call/result is auditable.
# A `transport` seam makes the client fully testable without a network.

#' Expose MCP server tools to an agent, under governance
#'
#' Connects to a Model Context Protocol server and returns its tools as
#' [LLMR::llm_tool()] objects ready for [agent()], wrapped so the agent's use of
#' them is safe and recorded. The defaults are strict
#' because MCP's documented attack surface (tool/schema poisoning, line jumping,
#' rug pulls, confused-deputy) lives in exactly the trust an agent places in a
#' server's tool descriptions.
#'
#' Defenses applied:
#' - **Read-only floor** (`policy = "read_only"`): the floor is an allowlist, not
#'   a denylist. A tool is exposed only when it is *positively* known to be
#'   read-only (its `annotations$readOnlyHint` is `TRUE`). Any tool that is not
#'   positively read-only (one with no annotations, or one that looks
#'   write-like) is refused unless `policy = "read_write"`. A malicious server
#'   cannot slip a writing tool past the floor by giving it a benign name and no
#'   annotations.
#' - **Human gate for writes** (`approve_writes = TRUE`): any write/external call
#'   pauses for sign-off (see [human_gate()]).
#' - **Schema pinning** (`pin_schemas = TRUE`): each tool's full advertised
#'   signature (input schema, description, and annotations) is hashed at first
#'   listing and re-verified before every call. A later change, or a
#'   server that refuses to let us re-verify (a `tools/get` that errors or
#'   returns no schema), raises `llmragent_mcp_schema_drift` rather than
#'   trusting the new definition or failing open (the schema-drift defense). The
#'   re-check *fails closed*: if it cannot confirm the tool is unchanged, it
#'   refuses.
#' - **Description sanitation**: server descriptions are treated as untrusted,
#'   never spliced into a system prompt, and scanned for injection patterns;
#'   a flagged tool is downgraded to require approval (the line-jumping defense).
#' - **Audit** (`audit = TRUE`): tool schemas, call argument hashes, and result
#'   hashes are recorded.
#'
#' @param config A connection spec: a list with `url` (HTTP) or `command`
#'   (stdio), or anything your `transport` understands.
#' @param policy `"read_only"` (default) or `"read_write"`.
#' @param approve_writes If `TRUE` (default), write/external calls pass a human
#'   gate.
#' @param audit If `TRUE` (default), schemas/calls/results are recorded.
#' @param allow Optional character vector of tool names to expose (others are
#'   dropped and logged).
#' @param pin_schemas If `TRUE` (default), pin and re-check tool schemas.
#' @param transport Test/extension seam: a function `(method, params) -> result`
#'   speaking JSON-RPC to the server. Default builds a real HTTP/stdio client.
#' @param timeout_s,max_bytes Per-call limits.
#' @return A list of `llmr_tool` objects, each carrying MCP governance metadata.
#' @seealso [agent()], [agent_tool()], [human_gate()]
#' @examples
#' \dontrun{
#' # offline, via a fake transport returning canned JSON-RPC
#' fake <- function(method, params) {
#'   if (method == "tools/list") list(tools = list(list(
#'     name = "search", description = "Search docs.",
#'     inputSchema = list(type = "object",
#'                        properties = list(q = list(type = "string"))))))
#'   else list(content = list(list(type = "text", text = "result")))
#' }
#' tools <- mcp_tools(list(url = "http://localhost:9000"), transport = fake)
#' }
#' @export
mcp_tools <- function(config,
                      policy = c("read_only", "read_write"),
                      approve_writes = TRUE,
                      audit = TRUE,
                      allow = NULL,
                      pin_schemas = TRUE,
                      transport = NULL,
                      timeout_s = 30,
                      max_bytes = 1e6) {
  policy <- match.arg(policy)
  tr <- transport %||% .mcp_default_transport(config, timeout_s = timeout_s)

  listed <- tryCatch(tr("tools/list", list()), error = function(e)
    stop("MCP tools/list failed: ", conditionMessage(e), call. = FALSE))
  raw_tools <- listed$tools %||% list()

  # a session-local pin store: tool name -> schema hash at first listing
  pins <- new.env(parent = emptyenv())

  out <- list()
  for (rt in raw_tools) {
    nm <- rt$name %||% next
    if (!is.null(allow) && !(nm %in% allow)) next  # allowlist: drop + (audit)
    schema <- rt$inputSchema %||% rt$input_schema %||% list(type = "object")
    desc_raw <- as.character(rt$description %||% "")

    # rug-pull defence: pin the FULL advertised signature now (schema +
    # description + annotations), re-checked on each call. Pinning the schema
    # alone would let a server flip readOnlyHint -> destructive, or rewrite the
    # description into an injection, after listing without tripping the check.
    # The originally-listed description/annotations are kept so the re-check can
    # rebuild the signature even when tools/get returns only the schema.
    if (isTRUE(pin_schemas))
      assign(nm, list(sig = .mcp_signature(schema, desc_raw, rt$annotations),
                      desc = desc_raw, annotations = rt$annotations),
             envir = pins)

    # line-jumping defence: scan the (untrusted) description; downgrade if dirty.
    inj <- .mcp_scan_description(desc_raw)
    write_like <- .mcp_is_write(rt)
    requires_approval <- (write_like && isTRUE(approve_writes)) || length(inj) > 0L

    # read-only floor: ALLOWLIST, not denylist. Under read_only, a tool is
    # exposed only if it is positively known to be read-only; anything not
    # positively read-only (no annotations, or write-like) is refused.
    refuse <- identical(policy, "read_only") && !.mcp_is_readonly(rt)

    tool <- .mcp_wrap_tool(nm, desc_raw, schema, tr = tr, pins = pins,
                           pin_schemas = pin_schemas, refuse = refuse,
                           requires_approval = requires_approval, audit = audit,
                           max_bytes = max_bytes, write_like = write_like,
                           injection_flags = inj)
    out[[length(out) + 1L]] <- tool
  }
  out
}

# Build one governed llm_tool wrapping an MCP tools/call.
#' @keywords internal
#' @noRd
.mcp_wrap_tool <- function(name, description, schema, tr, pins, pin_schemas,
                           refuse, requires_approval, audit, max_bytes,
                           write_like, injection_flags) {
  # Force every captured argument so each tool's closure binds its OWN policy
  # (R promises in a loop would otherwise all resolve to the last iteration).
  force(name); force(description); force(schema); force(refuse)
  force(requires_approval); force(audit); force(max_bytes); force(write_like)
  fn <- function(...) {
    args <- list(...)
    if (isTRUE(refuse)) {
      return(sprintf("BLOCKED: tool '%s' is not positively read-only and policy is read_only.", name))
    }
    # rug-pull re-check: the server must not have changed this tool's signature
    # since it was listed. This FAILS CLOSED -- a server that errors on
    # tools/get, or returns no schema, is treated as drift, because we cannot
    # re-verify what we are about to call.
    if (isTRUE(pin_schemas) && exists(name, envir = pins)) {
      pinned <- get(name, envir = pins)
      got <- tryCatch(tr("tools/get", list(name = name)), error = function(e) NULL)
      cur_schema <- got$inputSchema %||% got$input_schema %||% NULL
      if (is.null(cur_schema)) {
        rlang::abort(
          sprintf(paste0("MCP tool '%s' could not be re-verified (tools/get failed ",
                         "or returned no schema); refusing (possible rug pull)."), name),
          class = c("llmragent_mcp_schema_drift", "error", "condition"), tool = name)
      }
      # Rebuild the signature. Use whatever tools/get reports for description /
      # annotations; fall back to the originally-listed values when it omits
      # them (a schema-only tools/get still pins+checks the schema).
      cur_desc <- if (!is.null(got$description)) as.character(got$description) else pinned$desc
      cur_ann  <- if (!is.null(got$annotations)) got$annotations else pinned$annotations
      cur_sig  <- .mcp_signature(cur_schema, cur_desc, cur_ann)
      if (!identical(cur_sig, pinned$sig)) {
        rlang::abort(
          sprintf("MCP tool '%s' changed its schema/description/annotations since it was listed (possible rug pull); refusing.", name),
          class = c("llmragent_mcp_schema_drift", "error", "condition"), tool = name)
      }
    }
    res <- tryCatch(tr("tools/call", list(name = name, arguments = args)),
                    error = function(e) paste0("ERROR: ", conditionMessage(e)))
    out <- .mcp_result_text(res)
    if (is.finite(max_bytes) && nchar(out, type = "bytes") > max_bytes) {
      out <- paste0(substr(out, 1L, max_bytes), " ...[truncated]")
    }
    attr(out, "mcp") <- list(tool = name,
                             arguments_hash = LLMR::llm_hash(args),
                             result_hash = LLMR::llm_hash(out),
                             audited = isTRUE(audit))
    out
  }
  tool <- LLMR::llm_tool(fn, name = name,
                         description = .mcp_safe_description(description),
                         parameters = schema)
  attr(tool, "governance") <- list(
    side_effects = if (write_like) "write" else "external",
    requires_approval = isTRUE(requires_approval),
    timeout_s = NULL, max_calls = Inf, max_bytes = max_bytes,
    mcp = list(server_tool = name, injection_flags = injection_flags,
               pinned = isTRUE(pin_schemas)))
  tool
}

# Pin a tool's FULL advertised signature, not just its input schema. Hashing
# the schema together with the description and annotations means a server that,
# after listing, rewrites the description into an injection or flips
# readOnlyHint -> destructiveHint trips the same rug-pull check as a schema swap.
#' @keywords internal
#' @noRd
.mcp_signature <- function(schema, description, annotations) {
  LLMR::llm_hash(list(
    schema = schema %||% list(type = "object"),
    description = as.character(description %||% ""),
    annotations = annotations %||% list()))
}

# Is a server tool POSITIVELY known to be read-only? The read-only floor is an
# allowlist: only a tool that the server itself marks `readOnlyHint = TRUE` is
# trusted as read-only. Absence of the hint is NOT read-only -- it is unknown,
# and the floor refuses the unknown. This is the conservative inverse of the
# write heuristic below: that heuristic guesses at writes for approval/side
# effects, but the floor never trusts a guess of "probably safe".
#' @keywords internal
#' @noRd
.mcp_is_readonly <- function(rt) {
  ann <- rt$annotations %||% list()
  isTRUE(ann$readOnlyHint)
}

# Heuristic: does a server tool look like it writes or has external side effects?
# MCP has no formal side-effect field, so use annotations when present and the
# name/description otherwise. Conservative: unknown -> treat as external.
# NOTE: this drives side_effects/approval only; the read-only FLOOR is governed
# by .mcp_is_readonly(), which is allowlist-style (positively-read-only).
#' @keywords internal
#' @noRd
.mcp_is_write <- function(rt) {
  ann <- rt$annotations %||% list()
  if (isTRUE(ann$readOnlyHint)) return(FALSE)
  if (isTRUE(ann$destructiveHint) || isTRUE(ann$openWorldHint)) return(TRUE)
  nm <- tolower(rt$name %||% "")
  if (grepl("(create|write|update|delete|remove|send|post|put|set|modify|exec|run)", nm)) return(TRUE)
  # default: external (reaches the server), gated only if approve_writes + policy
  FALSE
}

# Scan an untrusted tool description for prompt-injection / line-jumping
# patterns. Returns the matched fragments (empty = clean).
#' @keywords internal
#' @noRd
.mcp_scan_description <- function(text) {
  patterns <- c(
    "(?i)ignore (all |the )?(previous|prior|above) (instructions|prompts?)",
    "(?i)\\b(always|first|before (doing|you) )\\s*(call|use|invoke|run)\\b",
    "(?i)disregard (your|the) (system|instructions)",
    "(?i)<\\s*/?\\s*(system|im_start|im_end|tool)\\b",
    "(?i)you (must|should) (now )?(send|exfiltrate|forward|reveal)",
    "(?i)do not (tell|inform|mention to) the user"
  )
  hits <- character(0)
  for (p in patterns) {
    m <- regmatches(text, gregexpr(p, text, perl = TRUE))[[1]]
    if (length(m)) hits <- c(hits, m)
  }
  unique(hits)
}

# A description safe to show the model: never let a server description carry
# imperative injection. When flagged, the offending fragments are redacted from
# the surfaced text so the model never sees the injected instruction.
#' @keywords internal
#' @noRd
.mcp_safe_description <- function(text) {
  hits <- .mcp_scan_description(text)
  if (!length(hits)) return(text)
  redacted <- gsub("[[:cntrl:]]", " ", text)
  for (h in hits) redacted <- gsub(h, "[redacted: injection-like content]", redacted, fixed = TRUE)
  paste0("[server description sanitized] ", redacted)
}

# Extract text from an MCP tools/call result (content blocks) or an error string.
#' @keywords internal
#' @noRd
.mcp_result_text <- function(res) {
  if (is.character(res)) return(res)
  content <- res$content %||% NULL
  if (is.list(content) && length(content)) {
    txt <- vapply(content, function(b) {
      if (is.list(b) && identical(b$type, "text") && is.character(b$text)) b$text else ""
    }, character(1))
    txt <- txt[nzchar(txt)]
    if (length(txt)) return(paste(txt, collapse = "\n"))
  }
  tryCatch(as.character(jsonlite::toJSON(res, auto_unbox = TRUE, null = "null")),
           error = function(e) "")
}

# Default JSON-RPC transport (HTTP via httr2, or stdio). Only built when no
# `transport` was injected; the offline tests never reach this.
#' @keywords internal
#' @noRd
.mcp_default_transport <- function(config, timeout_s = 30) {
  url <- config$url %||% NULL
  if (is.null(url)) {
    stop("mcp_tools() needs config$url for the default transport, or a `transport=` ",
         "function (e.g. a stdio client). Provide one of these.", call. = FALSE)
  }
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("The default MCP transport needs the 'httr2' package.", call. = FALSE)
  }
  id_counter <- local({ n <- 0L; function() { n <<- n + 1L; n } })
  function(method, params) {
    body <- list(jsonrpc = "2.0", id = id_counter(), method = method, params = params)
    resp <- httr2::request(url) |>
      httr2::req_headers("Content-Type" = "application/json") |>
      httr2::req_body_json(body) |>
      httr2::req_timeout(timeout_s) |>
      httr2::req_perform()
    j <- httr2::resp_body_json(resp)
    if (!is.null(j$error)) stop("MCP error: ", j$error$message %||% "unknown", call. = FALSE)
    j$result
  }
}
