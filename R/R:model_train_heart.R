#' Training entry (advanced)
#'
#' The package ships a pre-trained model bundle for deployment and reproducibility.
#' Model training is intentionally kept out of the package interface to avoid
#' accidental re-training and to ensure consistent results. For full training
#' details, use the external script under `scripts/`.
#'
#' @param train_df Training data including HeartDisease.
#' @param ... Reserved.
#' @return This function always errors. Training is performed via external scripts.
#' @export
heart_train_models <- function(train_df, ...) {
  stop(
    "This package ships pre-trained models.\n",
    "Training is intentionally not exposed as a package function.\n",
    "Use scripts/train_RECIPES_STRICT_v3_1.R instead."
  )
}
