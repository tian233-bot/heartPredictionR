#' Load the pre-trained model bundle shipped with the package
#'
#' This package ships with a pre-trained model bundle stored under
#' \code{inst/extdata/}. After installation, the file is available via
#' \code{system.file("extdata", ..., package="heartPredictionR")}.
#'
#' @param path Optional path to a bundle \code{.rds}. If \code{NULL}, the bundle
#'   is loaded from the installed package extdata directory.
#' @param filename Bundle filename inside package extdata (advanced).
#' @return A list (bundle) containing trained models and metadata.
#' @examples
#' # Locate the bundle inside the installed package
#' system.file("extdata", "heart_models_RECIPES_STRICT_bundle.rds", package = "heartPredictionR")
#' b <- heart_load_bundle()
#' names(b)
#' @export
heart_load_bundle <- function(path = NULL,
                              filename = "heart_models_RECIPES_STRICT_bundle.rds") {
  if (is.null(path)) {
    path <- system.file("extdata", filename, package = "heartPredictionR")
  }
  if (path == "" || !file.exists(path)) {
    stop(
      "Bundle not found.\n",
      "Expected it inside the installed package at: extdata/", filename, "\n",
      "Or pass path= to a valid bundle file."
    )
  }
  readRDS(path)
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

    # Probability output is usually a matrix/data.frame with class columns
    if (is.matrix(pred) || is.data.frame(pred)) {
      pred <- as.data.frame(pred)
      if (positive_level %in% names(pred)) return(as.numeric(pred[[positive_level]]))
      return(as.numeric(pred[[1]]))
    }

    stop("This ranger model does not return probabilities. It must be trained with probability = TRUE.")
  }

  stop("Unsupported model class: ", paste(class(model_obj), collapse = ", "))
}

#' Predict heart disease probability (positive class)
#'
#' Supported models:
#' \itemize{
#'   \item \code{"topk"}: TopK Random Forest (recommended)
#'   \item \code{"full"}: Full-feature Random Forest
#'   \item \code{"logistic"}: Regularised logistic regression baseline
#' }
#'
#' Implementation notes:
#' \itemize{
#'   \item Input columns are validated using \code{heart_assert_required_cols()}.
#'   \item Inputs are coerced using \code{heart_coerce_inputs()}.
#'   \item Engineered features are added using \code{heart_add_features()}.
#'   \item Prediction supports both \code{caret::train/train.recipe} and \code{ranger} models.
#' }
#'
#' @param df New data (data.frame) with required feature columns.
#' @param bundle A bundle from \code{heart_load_bundle()}.
#' @param model Which model to use: \code{"topk"}, \code{"full"}, \code{"logistic"}.
#' @return Numeric vector of positive-class probabilities between 0 and 1.
#' @examples
#' b <- heart_load_bundle()
#' x <- heart_example_data(include_outcome = FALSE, n = 5)
#' p <- heart_predict_proba(x, bundle = b, model = "topk")
#' head(p)
#' @export
heart_predict_proba <- function(df,
                                bundle = heart_load_bundle(),
                                model = c("topk","full","logistic")) {
  model <- match.arg(model)

  # required schema
  required <- bundle$required_cols %||% heart_required_cols_default()
  heart_assert_required_cols(df, required)

  # preprocess
  df2 <- heart_coerce_inputs(df)
  df2 <- heart_add_features(df2)

  # positive class name
  pos <- bundle$positive_level %||% "Presence"

  if (model == "logistic") {
    # could be caret model or something else; hp_safe_predict_proba will handle caret::train
    if (is.null(bundle$glm_cv)) stop("Bundle missing glm_cv for model='logistic'.")
    return(clamp01(hp_safe_predict_proba(bundle$glm_cv, df2, positive_level = pos)))
  }

  if (model == "full") {
    if (is.null(bundle$rf_full_cv)) stop("Bundle missing rf_full_cv for model='full'.")
    return(clamp01(hp_safe_predict_proba(bundle$rf_full_cv, df2, positive_level = pos)))
  }

  # topk
  if (is.null(bundle$rf_top_cv)) stop("Bundle missing rf_top_cv for model='topk'.")
  clamp01(hp_safe_predict_proba(bundle$rf_top_cv, df2, positive_level = pos))
}

#' Predict heart disease class label
#'
#' High-level prediction function:
#' input = \code{data.frame}, output = predicted class label (factor).
#'
#' @param df New data (data.frame) with required feature columns.
#' @param bundle A bundle from \code{heart_load_bundle()}.
#' @param model Which model to use: \code{"topk"} (default), \code{"full"}, \code{"logistic"}.
#' @param threshold Optional numeric threshold. If \code{NULL}, uses bundle stored threshold for that model.
#' @return A factor with levels \code{c(positive, negative)}.
#' @examples
#' b <- heart_load_bundle()
#' x <- heart_example_data(include_outcome = FALSE, n = 5)
#' heart_predict(x, bundle = b, model = "topk")
#' @export
heart_predict <- function(df,
                          bundle = heart_load_bundle(),
                          model = c("topk","full","logistic"),
                          threshold = NULL) {
  model <- match.arg(model)

  pos <- bundle$positive_level %||% "Presence"
  neg <- bundle$negative_level %||% "Absence"

  thr <- threshold
  if (is.null(thr)) {
    thr <- switch(
      model,
      logistic = bundle$thr_glm  %||% 0.5,
      full     = bundle$thr_full %||% 0.5,
      topk     = bundle$thr_top  %||% 0.5
    )
  }

  p <- heart_predict_proba(df, bundle = bundle, model = model)
  cls <- ifelse(p >= thr, pos, neg)
  factor(cls, levels = c(pos, neg))
}
