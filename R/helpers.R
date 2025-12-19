#' Internal helpers
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' Clamp probabilities to between 0 and 1
#' @keywords internal
clamp01 <- function(p) pmin(1, pmax(0, p))

#' Read example dataset shipped with the package
#'
#' This helper loads the example CSV shipped under \code{inst/extdata/heart3.csv}.
#' It is useful for quick testing and for examples that must run during \code{R CMD check}.
#'
#' @param include_outcome Logical. If TRUE, keep \code{HeartDisease} if present.
#' @param n Optional integer. If provided, return the first \code{n} rows.
#' @return A data.frame.
#' @export
heart_example_data <- function(include_outcome = TRUE, n = NULL) {
  p <- system.file("extdata", "heart3.csv", package = "heartPredictionR")
  if (p == "") stop("Example dataset not found in the installed package.")
  df <- utils::read.csv(p, stringsAsFactors = FALSE)

  if (!isTRUE(include_outcome) && "HeartDisease" %in% names(df)) {
    df$HeartDisease <- NULL
  }
  if (!is.null(n)) df <- utils::head(df, n)
  df
}

#' Get prediction schema (required columns)
#'
#' @return Character vector of required predictor columns.
#' @keywords internal
heart_required_cols_default <- function() {
  c(
    "Age","Sex","ChestPainType","RestingBP","Cholesterol",
    "FastingBS","RestingECG","MaxHR","ExerciseAngina","Oldpeak","ST_Slope"
  )
}
