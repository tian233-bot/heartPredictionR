#' Compare models on a labelled dataset
#'
#' This utility predicts probabilities using all supported models and returns a
#' compact performance table at bundle-recommended thresholds.
#'
#' @param df data.frame containing predictors and a \code{HeartDisease} column.
#' @param bundle model bundle.
#' @return tibble of metrics per model.
#' @examples
#' b <- heart_load_bundle()
#' df <- heart_example_data(include_outcome = TRUE, n = 100)
#' # Convert HeartDisease (0/1) to Presence/Absence for evaluation:
#' df$HeartDisease <- ifelse(df$HeartDisease == 1, b$positive_level, b$negative_level)
#' df$HeartDisease <- factor(df$HeartDisease, levels = c(b$positive_level, b$negative_level))
#' heart_compare_models(df, bundle = b)
#' @export
heart_compare_models <- function(df, bundle = heart_load_bundle()) {
  pos <- bundle$positive_level %||% "Presence"
  neg <- bundle$negative_level %||% "Absence"

  if (!("HeartDisease" %in% names(df))) stop("df must include HeartDisease for comparison.")
  y <- factor(as.character(df$HeartDisease), levels = c(pos, neg))

  x <- df
  x$HeartDisease <- NULL

  p_log  <- heart_predict_proba(x, bundle, model = "logistic")
  p_full <- heart_predict_proba(x, bundle, model = "full")
  p_top  <- heart_predict_proba(x, bundle, model = "topk")

  thr_log  <- bundle$thr_glm  %||% 0.5
  thr_full <- bundle$thr_full %||% 0.5
  thr_top  <- bundle$thr_top  %||% 0.5

  m1 <- heart_eval_threshold(y, p_log,  thr_log,  positive_level = pos)
  m2 <- heart_eval_threshold(y, p_full, thr_full, positive_level = pos)
  m3 <- heart_eval_threshold(y, p_top,  thr_top,  positive_level = pos)

  tibble::tibble(
    Model = c("Logistic(glmnet)", "Full_RF", "TopK_RF"),
    Threshold = c(m1$threshold, m2$threshold, m3$threshold),
    Accuracy = c(m1$accuracy, m2$accuracy, m3$accuracy),
    F1 = c(m1$F1, m2$F1, m3$F1),
    AUROC = c(m1$AUROC, m2$AUROC, m3$AUROC),
    AUPRC = c(m1$AUPRC, m2$AUPRC, m3$AUPRC),
    Brier = c(m1$Brier, m2$Brier, m3$Brier)
  )
}
