# memory.R --------------------------------------------------------------------
# Pluggable agent memory. Three implementations share one contract:
#   $add(role, content)         append a message
#   $get(query = NULL)          messages to place in the next context window
#   $needs_compaction()         should compact() run before the next call?
#   $compact(config)            shrink memory using a model (no-op by default);
#                               returns the llmr_response when a model call was
#                               made, so the agent can account for the spend
#   $state() / restore via memory_restore()   lossless persistence
#   $clear()
# Unlike a fixed message list, memory is consulted on every call, so what an
# agent "remembers" is an explicit, swappable policy.

MemoryBase <- R6::R6Class(
  "MemoryBase",
  public = list(
    # @description Append one message.
    # @param role "user", "assistant", or "system".
    # @param content Character scalar.
    add = function(role, content) {
      private$msgs[[length(private$msgs) + 1L]] <-
        list(role = as.character(role)[1], content = as.character(content)[1])
      invisible(self)
    },
    # @description Messages to include in the next context window.
    # @param query Optional current user text (used by retrieval memory).
    get = function(query = NULL) private$msgs,
    # @description Does memory want compacting before the next call?
    needs_compaction = function() FALSE,
    # @description Compact memory with a model; no-op in the base class.
    # @param config An LLMR config used for summarization.
    compact = function(config) invisible(self),
    # @description Remove all messages.
    clear = function() { private$msgs <- list(); invisible(self) },
    # @description Number of stored messages.
    size = function() length(private$msgs),
    # @description Serializable state (for save_agent()).
    state = function() {
      list(class = class(self)[1], msgs = private$msgs, params = private$params())
    }
  ),
  private = list(
    msgs = list(),
    params = function() list(),
    restore_msgs = function(msgs) { private$msgs <- msgs; invisible(self) }
  )
)

MemoryBuffer <- R6::R6Class(
  "MemoryBuffer", inherit = MemoryBase,
  public = list(
    # @param keep Number of most recent messages retained.
    initialize = function(keep = 40L) {
      private$keep <- max(2L, as.integer(keep))
    },
    get = function(query = NULL) {
      n <- length(private$msgs)
      if (n <= private$keep) return(private$msgs)
      private$msgs[(n - private$keep + 1L):n]
    }
  ),
  private = list(
    keep = 40L,
    params = function() list(keep = private$keep)
  )
)

MemorySummary <- R6::R6Class(
  "MemorySummary", inherit = MemoryBase,
  public = list(
    # @param threshold_chars Compact when stored text exceeds this many characters.
    # @param keep_last How many recent messages survive compaction verbatim.
    # @param config Optional dedicated summarizer config; NULL uses the
    #   agent's own model.
    initialize = function(threshold_chars = 12000L, keep_last = 10L,
                          config = NULL) {
      private$threshold <- as.integer(threshold_chars)
      private$keep_last <- max(2L, as.integer(keep_last))
      if (!is.null(config)) .check_config(config)
      private$cfg <- config
    },
    needs_compaction = function() {
      sum(nchar(vapply(private$msgs, `[[`, "", "content"))) > private$threshold
    },
    # @description Summarize everything but the most recent messages into one
    #   system note. Called automatically by the agent before a request when
    #   needs_compaction() is TRUE.
    compact = function(config) {
      n <- length(private$msgs)
      if (n <= private$keep_last) return(invisible(self))
      old  <- private$msgs[seq_len(n - private$keep_last)]
      tail <- private$msgs[(n - private$keep_last + 1L):n]
      dialogue <- paste(
        vapply(old, function(m) paste0(m$role, ": ", m$content), character(1)),
        collapse = "\n")
      resp <- LLMR::call_llm_robust(
        private$cfg %||% config,
        c(system = paste(
            "Summarize the following conversation faithfully and compactly.",
            "Keep named entities, decisions, numbers, and open questions.",
            "Write at most 200 words."),
          user = dialogue),
        tries = 3, wait_seconds = 2)
      note <- list(role = "system",
                   content = paste0("Summary of the earlier conversation: ",
                                    as.character(resp)))
      private$msgs <- c(list(note), tail)
      invisible(resp)
    }
  ),
  private = list(
    threshold = 12000L,
    keep_last = 10L,
    cfg = NULL,
    params = function() list(threshold_chars = private$threshold,
                             keep_last = private$keep_last,
                             config = private$cfg)
  )
)

MemoryRecall <- R6::R6Class(
  "MemoryRecall", inherit = MemoryBase,
  public = list(
    # @param embed_config An LLMR embedding config.
    # @param keep_recent Most recent messages always included.
    # @param k How many older messages to retrieve by similarity.
    initialize = function(embed_config, keep_recent = 6L, k = 4L) {
      .check_embed_config(embed_config, "embed_config")
      private$embed_config <- embed_config
      private$keep_recent <- max(0L, as.integer(keep_recent))
      private$k <- max(0L, as.integer(k))
    },
    get = function(query = NULL) {
      n <- length(private$msgs)
      if (n <= private$keep_recent || is.null(query) || private$k == 0L) {
        return(self$.recent())
      }
      older_idx <- seq_len(n - private$keep_recent)
      private$ensure_embeddings()
      qv <- LLMR::get_batched_embeddings(as.character(query)[1], private$embed_config)
      if (is.null(qv) || is.null(private$emb)) return(self$.recent())
      qv <- as.numeric(qv[1, ])
      sims <- vapply(older_idx, function(i) {
        if (i > length(private$emb)) return(-Inf)
        v <- private$emb[[i]]
        if (is.null(v)) return(-Inf)
        sum(v * qv) / (sqrt(sum(v^2)) * sqrt(sum(qv^2)))
      }, numeric(1))
      top <- older_idx[order(sims, decreasing = TRUE)][seq_len(min(private$k, length(older_idx)))]
      top <- sort(top)
      c(lapply(private$msgs[top], function(m) {
          list(role = "system",
               content = paste0("Recalled earlier exchange (", m$role, "): ", m$content))
        }),
        self$.recent())
    },
    .recent = function() {
      n <- length(private$msgs)
      if (!n || private$keep_recent <= 0L) return(list())
      private$msgs[max(1L, n - private$keep_recent + 1L):n]
    }
  ),
  private = list(
    embed_config = NULL,
    keep_recent = 6L,
    k = 4L,
    emb = NULL,
    ensure_embeddings = function() {
      n <- length(private$msgs)
      if (is.null(private$emb)) private$emb <- vector("list", 0L)
      done <- length(private$emb)
      if (done >= n) return(invisible(NULL))
      new_idx <- (done + 1L):n
      texts <- vapply(private$msgs[new_idx], `[[`, "", "content")
      mat <- LLMR::get_batched_embeddings(texts, private$embed_config)
      for (j in seq_along(new_idx)) {
        # single-bracket list assignment: storing NULL must keep a placeholder
        # at that index, not delete it and shift everything after it
        private$emb[new_idx[j]] <- list(
          if (!is.null(mat)) as.numeric(mat[j, ]) else NULL)
      }
      invisible(NULL)
    },
    params = function() list(keep_recent = private$keep_recent, k = private$k)
  )
)

#' Agent memory policies
#'
#' An agent's memory decides which past messages enter the next context
#' window. Three policies ship with the package; all are drop-in:
#'
#' - `memory_buffer(keep)`: the last `keep` messages, verbatim. Simple,
#'   predictable, and right for most short-lived agents.
#' - `memory_summary(threshold_chars, keep_last, config)`: unbounded
#'   conversations. When stored text exceeds the threshold, the agent
#'   automatically condenses everything but the most recent messages into one
#'   summary note before its next request, so context stays small without
#'   forgetting decisions. By default the agent's own model writes the
#'   summary; pass `config` to bill compaction to a cheaper model instead.
#' - `memory_recall(embed_config, keep_recent, k)`: long-horizon recall. Older
#'   messages are embedded (via LLMR); at each turn the `k` most similar to
#'   the current input are injected alongside the recent tail.
#'
#' @param keep,keep_last,keep_recent,k,threshold_chars Policy sizes; see above.
#' @param config Optional `LLMR::llm_config()` used only for compaction
#'   summaries; NULL (default) summarizes with the agent's own model.
#' @param embed_config An LLMR embedding config (e.g.
#'   `llm_config("gemini", "gemini-embedding-001", embedding = TRUE)`).
#' @return A memory object to pass as `agent(memory = ...)`.
#' @examples
#' m <- memory_buffer(keep = 10)
#' m$add("user", "hello")$add("assistant", "hi")
#' length(m$get())
#'
#' \dontrun{
#' cfg   <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
#' cheap <- LLMR::llm_config("groq", "llama-3.1-8b-instant")
#'
#' # an agent that summarizes its own past with the cheap model
#' scribe <- agent("Scribe", cfg,
#'                 memory = memory_summary(threshold_chars = 8000,
#'                                         config = cheap))
#'
#' # an agent that recalls relevant old exchanges by embedding similarity
#' emb <- LLMR::llm_config("gemini", "gemini-embedding-001", embedding = TRUE)
#' sage <- agent("Sage", cfg, memory = memory_recall(emb, k = 4))
#' }
#' @name memory
NULL

#' @rdname memory
#' @export
memory_buffer <- function(keep = 40L) MemoryBuffer$new(keep = keep)

#' @rdname memory
#' @export
memory_summary <- function(threshold_chars = 12000L, keep_last = 10L,
                           config = NULL) {
  MemorySummary$new(threshold_chars = threshold_chars, keep_last = keep_last,
                    config = config)
}

#' @rdname memory
#' @export
memory_recall <- function(embed_config, keep_recent = 6L, k = 4L) {
  MemoryRecall$new(embed_config = embed_config, keep_recent = keep_recent, k = k)
}

# Internal: rebuild a memory object from $state() (persistence). Retrieval
# memory cannot be restored without its embedding config, so it degrades to a
# buffer with a warning.
memory_restore <- function(state, embed_config = NULL) {
  out <- switch(state$class,
    MemoryBuffer  = do.call(memory_buffer, state$params),
    MemorySummary = do.call(memory_summary, state$params),
    MemoryRecall  = {
      if (is.null(embed_config)) {
        warning("Retrieval memory restored as a plain buffer (no embed_config supplied).",
                call. = FALSE)
        memory_buffer(keep = 1000L)
      } else {
        do.call(memory_recall, c(list(embed_config = embed_config), state$params))
      }
    },
    memory_buffer(keep = 1000L)
  )
  for (m in state$msgs) out$add(m$role, m$content)
  out
}
