#' Load the pre-trained model bundle shipped with the package
#'
#' @param path Optional path to a bundle .rds. If NULL, loads from package extdata.
#' @return A list (bundle) containing trained models + preprocessing objects.
#' @export
heart_load_bundle <- function(path = NULL) {
  if (is.null(path)) {
    path <- system.file("extdata", "heart_models_RECIPES_STRICT_bundle.rds", package = "heartpredict")
  }
  if (path == "" || !file.exists(path)) {
    stop(
      "Bundle not found.\n",
      "Expected: inst/extdata/heart_models_RECIPES_STRICT_bundle.rds (after install),\n",
      "or pass path= to a valid bundle file."
    )
  }
  readRDS(path)
}

#' Predict positive-class probability for heart prediction
#'
#' Supported models: "topk" (recommended), "full", "logistic".
#' For TopK: uses stored prepared recipe (prep_rf_full) to bake features,
#' selects stored top_feats columns, then predicts with rf_top_cv.
#'
#' @param df New data (data.frame) with feature columns.
#' @param bundle A bundle from [heart_load_bundle()].
#' @param model Which model to use: "topk", "full", "logistic".
#' @return Numeric vector of positive-class probabilities between 0 and 1.
#' @export
heart_predict_proba <- function(df, bundle = heart_load_bundle(),
                                model = c("topk","full","logistic")) {
  model <- match.arg(model)

  pos <- bundle$positive_level %||% "Presence"

  required <- bundle$required_cols %||% c(
    "Age","Sex","ChestPainType","RestingBP","Cholesterol",
    "FastingBS","RestingECG","MaxHR","ExerciseAngina","Oldpeak","ST_Slope"
  )
  heart_assert_required_cols(df, required)

  df2 <- heart_coerce_inputs(df)
  df2 <- heart_add_features(df2)

  safe_prob <- function(model_obj, newdata) {
    pr <- suppressWarnings(stats::predict(model_obj, newdata = newdata, type = "prob"))
    pr <- as.data.frame(pr)
    if (pos %in% names(pr)) return(as.numeric(pr[[pos]]))
    as.numeric(pr[[1]])
  }

  if (model == "logistic") {
    return(clamp01(safe_prob(bundle$glm_cv, df2)))
  }
  if (model == "full") {
    return(clamp01(safe_prob(bundle$rf_full_cv, df2)))
  }

  # TopK baked prediction
  prep_rf_full <- bundle$prep_rf_full
  top_feats <- bundle$top_feats %||% character(0)

  if (is.null(prep_rf_full)) stop("Bundle missing prep_rf_full (required for TopK baked prediction).")
  if (length(top_feats) == 0) stop("Bundle missing top_feats.")

  baked <- recipes::bake(prep_rf_full, new_data = df2)

  miss <- setdiff(top_feats, names(baked))
  if (length(miss) > 0) {
    stop(
      "TopK baked columns missing: ", paste(miss, collapse = ", "),
      ". This usually means factor levels differ from training or the recipe changed."
    )
  }

  x_top <- baked[, top_feats, drop = FALSE]
  clamp01(safe_prob(bundle$rf_top_cv, x_top))
}

#' Predict heart prediction class label
#'
#' This is the main exported function:
#' input = data.frame, output = predicted class label (not probability).
#'
#' @param df New data (data.frame) with feature columns.
#' @param bundle A bundle from [heart_load_bundle()].
#' @param model Which model to use: "topk" (default), "full", "logistic".
#' @param threshold Optional threshold. If NULL, uses the stored threshold from bundle.
#' @return A factor with levels = c(positive, negative).
#' @examples
#' b <- heart_load_bundle()
#' new_pat <- data.frame(
#'   Age=54, Sex="M", ChestPainType="ATA", RestingBP=140, Cholesterol=289, FastingBS="0",
#'   RestingECG="Normal", MaxHR=172, ExerciseAngina="N", Oldpeak=0.0, ST_Slope="Up"
#' )
#' heart_predict(new_pat, b, model="topk")
#' @export
heart_predict <- function(df, bundle = heart_load_bundle(),
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
