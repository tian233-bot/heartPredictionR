#' Internal helpers
#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' Clamp probabilities to between 0 and 1
#' @keywords internal
clamp01 <- function(p) pmin(1, pmax(0, p))

#' Read example dataset shipped with the package
#'
#' Loads a small example CSV from inst/extdata/.
#' By default it looks for heart_example_input.csv (predictors only).
#' Falls back to heart3.csv if present.
#'
#' @param include_outcome Logical. If TRUE, keep HeartDisease if present.
#' @param n Optional integer. If provided, return the first n rows.
#' @return A data.frame.
#' @export
#' @examples
#' b <- heart_load_bundle()
#' newdata <- heart_example_data(include_outcome = FALSE, n = 5)
#' p <- heart_predict_proba(newdata, bundle = b, model = "topk")
#' head(p)
heart_example_data <- function(include_outcome = TRUE, n = NULL) {

  # Prefer a small predictors-only template for reliable examples
  candidates <- c("heart_example_input.csv", "heart3.csv")

  p <- ""
  for (fn in candidates) {
    pp <- system.file("extdata", fn, package = "heartPredictionR")
    if (nzchar(pp) && file.exists(pp)) { p <- pp; break }
  }

  if (!nzchar(p)) {
    stop(
      "Example dataset not found in the installed package.\n",
      "Expected one of: inst/extdata/heart_example_input.csv or inst/extdata/heart3.csv"
    )
  }

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

#' @keywords internal
hp_safe_predict_proba <- function(model_obj, newdata, positive_level) {

  # caret::train / train.recipe: predict(type="prob")
  if (inherits(model_obj, c("train", "train.recipe"))) {
    pr <- stats::predict(model_obj, newdata = newdata, type = "prob")
    pr <- as.data.frame(pr)
    if (positive_level %in% names(pr)) return(as.numeric(pr[[positive_level]]))
    return(as.numeric(pr[[1]]))
  }

  # ranger: must use ranger::predict(data=..., type="response")
  if (inherits(model_obj, "ranger")) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required to predict from a ranger model. Please install it.")
    }
    rp <- ranger::predict(model_obj, data = newdata, type = "response")
    pred <- rp$predictions

    # probability output is usually a matrix/data.frame with class columns
    if (is.matrix(pred) || is.data.frame(pred)) {
      pred <- as.data.frame(pred)
      if (positive_level %in% names(pred)) return(as.numeric(pred[[positive_level]]))
      return(as.numeric(pred[[1]]))
    }

    stop("This ranger model does not return probabilities. It must be trained with probability = TRUE.")
  }

  stop("Unsupported model class: ", paste(class(model_obj), collapse = ", "))
}
