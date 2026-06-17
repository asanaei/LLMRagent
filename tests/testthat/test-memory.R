test_that("buffer memory keeps the most recent messages", {
  m <- memory_buffer(keep = 4)
  for (i in 1:6) m$add("user", paste0("m", i))
  got <- m$get()
  expect_length(got, 4L)
  expect_identical(got[[1]]$content, "m3")
  expect_identical(got[[4]]$content, "m6")
  expect_equal(m$size(), 6L)   # storage is intact; the window is what shrinks
})

test_that("summary memory compacts automatically through the agent", {
  m <- memory_summary(threshold_chars = 50, keep_last = 2)
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    fake_response("SUMMARY-OF-EARLIER")
  }, {
    a <- fake_agent("S", list("r1", "r2", "r3"), memory = m)
    a$chat("a long opening message that easily exceeds fifty characters")
    a$chat("another long message to push us over the threshold")
    # the third chat must trigger compaction before its call
    a$chat("third")
    msgs <- m$get()
    expect_true(any(vapply(msgs, function(x)
      grepl("SUMMARY-OF-EARLIER", x$content), logical(1))))
    expect_true(any(a$trace()$event == "compact"))
  })
})

test_that("recall memory retrieves by similarity", {
  emb_cfg <- LLMR::llm_config("gemini", "gemini-embedding-001", embedding = TRUE)
  m <- memory_recall(emb_cfg, keep_recent = 2, k = 1)
  for (txt in c("the budget meeting is on tuesday",
                "my cat is orange",
                "we allocated 40 million dollars",
                "recent one", "recent two")) {
    m$add("user", txt)
  }
  fake_embed <- function(texts, embed_config, ...) {
    # crude deterministic embedding: money-related words load on dim 1
    t(vapply(texts, function(s) {
      c(as.numeric(grepl("allocat|million|dollar|money", s)),
        as.numeric(grepl("cat", s)),
        1)
    }, numeric(3)))
  }
  with_stub_llmr("get_batched_embeddings", fake_embed, {
    got <- m$get(query = "how much money did we set aside?")
    txts <- vapply(got, `[[`, "", "content")
    expect_true(any(grepl("40 million", txts)))    # retrieved by similarity
    expect_true(any(grepl("recent two", txts)))    # recent tail kept
    expect_false(any(grepl("orange", txts)))       # irrelevant memory left out
  })
})

test_that("memory state round-trips through save/load", {
  m <- memory_buffer(keep = 7)
  m$add("user", "alpha")$add("assistant", "beta")
  st <- m$state()
  m2 <- LLMRagent:::memory_restore(st)
  expect_identical(vapply(m2$get(), `[[`, "", "content"), c("alpha", "beta"))
})

test_that("recall memory tolerates keep_recent = 0 and failed embeddings", {
  emb_cfg <- LLMR::llm_config("gemini", "gemini-embedding-001", embedding = TRUE)

  m0 <- memory_recall(emb_cfg, keep_recent = 0, k = 1)
  m0$add("user", "alpha")$add("user", "beta")
  fake_embed <- function(texts, embed_config, ...) {
    t(vapply(texts, function(s) c(nchar(s), 1), numeric(2)))
  }
  with_stub_llmr("get_batched_embeddings", fake_embed, {
    got <- m0$get(query = "beta?")
    # no out-of-bounds NULL rows; only the retrieved memory comes back
    expect_true(all(vapply(got, function(x) is.list(x) && !is.null(x$content),
                           logical(1))))
  })

  # an embedding backend returning NULL must not corrupt the index
  m1 <- memory_recall(emb_cfg, keep_recent = 1, k = 1)
  for (t in c("one", "two", "three")) m1$add("user", t)
  with_stub_llmr("get_batched_embeddings", function(...) NULL, {
    expect_silent(got <- m1$get(query = "two?"))
    expect_identical(got[[length(got)]]$content, "three")  # recent tail intact
  })
})

test_that("memory_recall rejects non-embedding configs", {
  chat_cfg <- LLMR::llm_config("groq", "openai/gpt-oss-20b")
  expect_error(memory_recall(chat_cfg), "embedding")
})

test_that("summary memory can bill compaction to a dedicated cheap model", {
  seen_model <- NULL
  with_stub_llmr("call_llm_robust", function(config, messages, ...) {
    seen_model <<- config$model
    fake_response("the summary")
  }, {
    m <- memory_summary(threshold_chars = 10, keep_last = 2,
                        config = LLMR::llm_config("groq", "cheap-model"))
    for (i in 1:5) m$add("user", "a message long enough to trip the threshold")
    expect_true(m$needs_compaction())
    m$compact(LLMR::llm_config("groq", "main-model"))
    expect_identical(seen_model, "cheap-model")   # not the agent's model
    expect_equal(m$size(), 3L)                    # summary note + 2 kept

    # and the dedicated config survives a save/load round trip
    m2 <- LLMRagent:::memory_restore(m$state())
    m2$compact(LLMR::llm_config("groq", "main-model"))
    expect_identical(seen_model, "cheap-model")
  })
})
