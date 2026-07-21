# Personas as auditable research objects: construction, variation, and audit.
# Every test is offline (no generative config is passed, so no model call is made):
# enumerated variants and the lexical audit layer only.

test_that("persona_frame builds an object with text and a content hash", {
  p <- persona_frame(
    "A retired schoolteacher who reads the local paper.",
    source = "synthetic",
    scope  = list(country = "US"))

  expect_s3_class(p, "persona_frame")
  expect_equal(p$text, "A retired schoolteacher who reads the local paper.")
  expect_equal(p$source, "synthetic")
  expect_true(is.character(p$hash) && nchar(p$hash) > 0L)

  # A frame is drop-in usable as a string: as.character() returns the brief.
  expect_equal(as.character(p), p$text)
})

test_that("the persona hash is stable across construction and tracks the text", {
  txt <- "A first-time voter in a swing district."
  p1 <- persona_frame(txt, source = "synthetic")
  p2 <- persona_frame(txt, source = "synthetic")
  expect_equal(p1$hash, p2$hash)            # stable: same brief, same hash

  p3 <- persona_frame(paste0(txt, " They distrust polls."), source = "synthetic")
  expect_false(identical(p1$hash, p3$hash)) # sensitive: a wording change flips it

  # Matches the exported hashing convention.
  expect_equal(p1$hash, hash_persona(p1))
})

test_that("enumerated persona_variants builds a persona_set carrying the design", {
  base <- persona_frame("A small-business owner in a mid-size town.",
                        source = "synthetic")
  set <- persona_variants(base, vary = list(age = c("28", "52")),
                          config = NULL)

  expect_s3_class(set, "persona_set")
  expect_equal(nrow(set), 2L)

  # Each row's persona is a frame, derived from the base, carrying its attribute.
  expect_true(all(vapply(set$persona, inherits, logical(1), "persona_frame")))
  expect_true(all(vapply(set$persona,
                         function(f) identical(f$variant_of, base$hash),
                         logical(1))))
  ages <- vapply(set$persona, function(f) f$attributes$age, character(1))
  expect_equal(sort(ages), c("28", "52"))

  # The varied attribute surfaces as a column, and id is each frame's hash.
  expect_true("age" %in% names(set))
  expect_equal(sort(set$age), c("28", "52"))
  expect_equal(set$id, vapply(set$persona, function(f) f$hash, character(1)))
  expect_true(all(set$variant_of == base$hash))
})

test_that("the Cartesian product covers every level combination", {
  base <- persona_frame("A commuter.", source = "synthetic")
  set <- persona_variants(base, vary = list(age = c("28", "52"),
                                            risk = c("cautious", "tolerant")))
  expect_equal(nrow(set), 4L)
  combos <- paste(set$age, set$risk)
  expect_equal(sort(combos),
               sort(c("28 cautious", "28 tolerant",
                      "52 cautious", "52 tolerant")))
})

test_that("the lexical audit layer flags essentializing language", {
  flagged <- persona_frame(
    "A community member. All women naturally prefer comfort over risk.",
    source = "synthetic")
  neutral <- persona_frame(
    "A nurse who works nights, keeps a vegetable garden, and reads mysteries.",
    source = "synthetic")

  aud_flag <- persona_audit(flagged)
  expect_s3_class(aud_flag, "persona_audit")
  expect_true(aud_flag$flag_lexical)
  expect_true(aud_flag$n_lexical_hits >= 1L)
  # No model layer ran, so the model scores stay NA.
  expect_true(is.na(aud_flag$caricature_score))

  aud_ok <- persona_audit(neutral)
  expect_false(aud_ok$flag_lexical)
  expect_equal(aud_ok$n_lexical_hits, 0L)
})

test_that("auditing a persona_set scores every brief and diagnostics summarizes", {
  base <- persona_frame("A small-business owner.", source = "synthetic")
  set <- persona_variants(base, vary = list(age = c("35", "60")))
  aud <- persona_audit(set, config = NULL)

  expect_s3_class(aud, "persona_audit")
  expect_equal(nrow(aud), 2L)

  d <- diagnostics(aud)
  expect_equal(nrow(d), 1L)
  expect_true(all(c("n_personas", "n_flagged", "max_hits", "mean_caricature")
                  %in% names(d)))
  expect_equal(d$n_personas, 2L)
  expect_true(is.numeric(d$n_flagged) || is.integer(d$n_flagged))
})
