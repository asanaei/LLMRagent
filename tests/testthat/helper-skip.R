skip_if_no_api <- function() {
  testthat::skip_on_cran()
  testthat::skip_on_ci()
  testthat::skip_if_offline()
  testthat::skip_if(!nzchar(Sys.getenv("OPENAI_API_KEY")), "OPENAI_API_KEY not set")
}



