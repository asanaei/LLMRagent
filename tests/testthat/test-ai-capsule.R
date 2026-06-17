# The AI usage capsule (inst/ai/LLMRagent.md) must never drift from the real
# API: every function it mentions in call position must exist, either as an
# export of this package, an LLMR export, or a base/stats/utils function.

test_that("the AI capsule mentions only real functions", {
  path <- system.file("ai", "LLMRagent.md", package = "LLMRagent")
  expect_true(nzchar(path))
  txt <- paste(readLines(path, warn = FALSE), collapse = "\n")
  hits <- regmatches(txt, gregexpr(
    "(?<![$:A-Za-z0-9_.])([A-Za-z_][A-Za-z0-9_.]*)\\(", txt, perl = TRUE))[[1]]
  fns <- unique(sub("\\($", "", hits))
  known <- function(f) {
    f %in% getNamespaceExports("LLMRagent") ||
      f %in% getNamespaceExports("LLMR") ||
      exists(f, envir = baseenv()) ||
      f %in% getNamespaceExports("stats") ||
      f %in% getNamespaceExports("utils")
  }
  unknown <- fns[!vapply(fns, known, logical(1))]
  expect_identical(unknown, character(0))
})
