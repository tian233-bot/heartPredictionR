#' Compute classification metrics at a fixed threshold
#'
#' @param y_true Factor/character of true labels.
#' @param prob_pos Numeric vector of positive probabilities.
#' @param threshold Threshold for positive.
#' @param positive_level Positive class label.
#' @return A named list of metrics.
#' @export
heart_eval_threshold <- function(y_true, prob_pos, threshold,
                                 positive_level = "Presence") {
  prob_pos <- clamp01(prob_pos)

  y_true <- factor(as.character(y_true))
  y_bin <- ifelse(as.character(y_true) == positive_level, 1, 0)
  pred <- ifelse(prob_pos >= threshold, 1, 0)

  TP <- sum(pred == 1 & y_bin == 1, na.rm = TRUE)
  FP <- sum(pred == 1 & y_bin == 0, na.rm = TRUE)
  TN <- sum(pred == 0 & y_bin == 0, na.rm = TRUE)
  FN <- sum(pred == 0 & y_bin == 1, na.rm = TRUE)

  acc <- (TP + TN) / max(1, TP + FP + TN + FN)
  precision <- ifelse((TP + FP) == 0, NA_real_, TP/(TP+FP))
  recall    <- ifelse((TP + FN) == 0, NA_real_, TP/(TP+FN))
  F1 <- ifelse(is.na(precision) || is.na(recall) || (precision + recall) == 0,
               NA_real_, 2 * precision * recall / (precision + recall))

  brier <- mean((y_bin - prob_pos)^2, na.rm = TRUE)

  auroc <- NA_real_
  auprc <- NA_real_

  if (requireNamespace("pROC", quietly = TRUE) && length(unique(y_bin)) == 2) {
    roc_obj <- pROC::roc(y_bin, prob_pos, quiet = TRUE)
    auroc <- as.numeric(pROC::auc(roc_obj))
  }
  if (requireNamespace("PRROC", quietly = TRUE) && sum(y_bin == 1) > 0 && sum(y_bin == 0) > 0) {
    pr <- PRROC::pr.curve(
      scores.class0 = prob_pos[y_bin == 1],
      scores.class1 = prob_pos[y_bin == 0],
      curve = FALSE
    )
    auprc <- pr$auc.integral
  }

  list(
    threshold = threshold,
    accuracy = acc,
    precision = precision,
    recall = recall,
    F1 = F1,
    AUROC = auroc,
    AUPRC = auprc,
    Brier = brier
  )
}

#' Calibration bins and expected calibration error (ECE)
#'
#' @param y_true true labels.
#' @param prob_pos positive probabilities.
#' @param positive_level positive label.
#' @param bins number of bins.
#' @return list(cal_df, ece).
#' @export
heart_calibration <- function(y_true, prob_pos, positive_level = "Presence", bins = 10) {
  df <- tibble::tibble(y = as.character(y_true), p = clamp01(prob_pos)) |>
    tidyr::drop_na()

  df$y_bin <- ifelse(df$y == positive_level, 1, 0)
  df$bin <- cut(df$p, breaks = seq(0, 1, length.out = bins + 1), include.lowest = TRUE)

  cal <- dplyr::group_by(df, .data$bin) |>
    dplyr::summarise(
      n = dplyr::n(),
      mean_pred = mean(.data$p),
      obs_rate  = mean(.data$y_bin),
      .groups = "drop"
    ) |>
    dplyr::filter(.data$n > 0)

  ece <- if (nrow(cal) == 0) NA_real_ else with(cal, sum(n * abs(obs_rate - mean_pred)) / sum(n))
  list(cal_df = cal, ece = ece)
}
