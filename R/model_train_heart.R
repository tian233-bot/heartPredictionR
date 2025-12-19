#' Train models (advanced / optional)
#'
#' This package is designed for deployment: it ships a pre-trained bundle and
#' focuses on robust prediction and explanation utilities.
#'
#' A training function is included as a placeholder for reproducibility, but
#' training is intentionally not executed inside the package by default.
#'
#' @param train_df Training data including \code{HeartDisease}.
#' @param ... Reserved.
#' @return A list containing fitted models and preprocessing objects.
#' @export
heart_train_models <- function(train_df, ...) {
  stop("This package ships pre-trained models. Training is not executed by default.")
}
