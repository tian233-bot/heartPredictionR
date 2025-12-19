#' Load the pre-trained model bundle shipped with the package
#'
#' By default the bundle is loaded from \code{extdata} in the installed package.
#' You can also provide a custom path to an \code{.rds} bundle file.
#'
#' @param path Optional path to a bundle \code{.rds}. If NULL, load from package extdata.
#' @param filename Bundle filename inside package extdata (advanced).
#' @return A list (bundle) containing trained models and metadata.
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

#' Predict heart disease probability (positive class)
#'
#' Supported models:
#' \itemize{
#'   \item \code{"topk"}: TopK random forest (recommended deployment model)
#'   \item \code{"full"}: Full-feature random forest
#'   \item \code{"logistic"}: Regularised logistic regression baseline
#' }
#'
#' Implementation notes:
#' \itemize{
#'   \item Inputs are validated and coerced using \code{heart_coerce_inputs()}.
#'   \item Engineered features are added using \code{heart_add_features()}.
#'   \item For \code{"topk"}, your saved model is a \code{caret} \code{train.recipe}
#'         object, so prediction is done directly via \code{predict()} and the recipe
#'         is handled internally (no manual \code{bake()} required).
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

  # positive class name (used to pick probability column)
  pos <- bundle$positive_level %||% "Presence"

  # predict helper
  safe_prob <- function(model_obj, newdata) {
    # suppressWarnings to avoid recipe factor-type warnings during bake/predict
    pr <- suppressWarnings(stats::predict(model_obj, newdata = newdata, type = "prob"))
    pr <- as.data.frame(pr)
    if (pos %in% names(pr)) return(as.numeric(pr[[pos]]))
    as.numeric(pr[[1]])
  }

  if (model == "logistic") {
    if (!requireNamespace("glmnet", quietly = TRUE)) {
      stop("Package 'glmnet' is required for model='logistic'. Please install it.")
    }
    return(clamp01(safe_prob(bundle$glm_cv, df2)))
  }

  if (model == "full") {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("Package 'ranger' is required for model='full'. Please install it.")
    }
    return(clamp01(safe_prob(bundle$rf_full_cv, df2)))
  }

  # topk (caret train.recipe handles recipe internally)
  if (!requireNamespace("caret", quietly = TRUE)) {
    stop("Package 'caret' is required for model='topk'. Please install it.")
  }
  clamp01(safe_prob(bundle$rf_top_cv, df2))
}

#' Predict heart disease class label
#'
#' This is the coursework-style high-level prediction function:
#' input = \code{data.frame}, output = predicted class label (factor).
#'
#' @param df New data (data.frame) with required feature columns.
#' @param bundle A bundle from \code{heart_load_bundle()}.
#' @param model Which model to use: \code{"topk"} (default), \code{"full"}, \code{"logistic"}.
#' @param threshold Optional numeric threshold. If NULL, uses bundle stored threshold for that model.
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
    thr <- switch(model,
                  logistic = bundle$thr_glm  %||% 0.5,
                  full     = bundle$thr_full %||% 0.5,
                  topk     = bundle$thr_top  %||% 0.5
    )
  }

  p <- heart_predict_proba(df, bundle = bundle, model = model)
  cls <- ifelse(p >= thr, pos, neg)
  factor(cls, levels = c(pos, neg))
}
