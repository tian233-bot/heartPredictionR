#' Package plotting theme
#'
#' @param base_size Base font size.
#' @return A ggplot2 theme.
#' @export
heart_theme <- function(base_size = 14) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(hjust = 0.5),
      axis.title    = ggplot2::element_text(face = "bold"),
      axis.text     = ggplot2::element_text(color = "black"),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position    = "right",
      legend.title       = ggplot2::element_blank()
    )
}

#' Extract RF permutation importance (vip::vi)
#'
#' @param ranger_model A ranger model object (e.g., caret train finalModel).
#' @param top_n Top N features.
#' @return A tibble with Variable and Importance.
#' @export
heart_importance_rf <- function(ranger_model, top_n = 20) {
  if (!requireNamespace("vip", quietly = TRUE)) {
    stop("Package 'vip' is required for RF importance. Install it with install.packages('vip').")
  }
  vip::vi(ranger_model) |>
    dplyr::arrange(dplyr::desc(.data$Importance)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(Variable = forcats::fct_reorder(.data$Variable, .data$Importance))
}

#' Extract glmnet coefficient importance (absolute coefficients at best lambda)
#'
#' @param glm_caret A caret::train(glmnet) object.
#' @return A tibble with Feature, Coef, and Importance.
#' @keywords internal
heart_glmnet_coef_importance <- function(glm_caret) {
  best_lambda <- glm_caret$bestTune$lambda
  co <- as.matrix(stats::coef(glm_caret$finalModel, s = best_lambda))

  tibble::tibble(
    Feature = rownames(co),
    Coef    = as.numeric(co[, 1])
  ) |>
    dplyr::filter(.data$Feature != "(Intercept)") |>
    dplyr::mutate(Importance = abs(.data$Coef)) |>
    dplyr::arrange(dplyr::desc(.data$Importance))
}

#' Aggregate one-hot dummy importances back to base variables
#'
#' @param glm_caret A caret::train(glmnet) object.
#' @param top_n Top N variables after aggregation.
#' @param cat_prefix Categorical prefixes to aggregate (e.g., Sex_*).
#' @param how Aggregation rule: "sum" or "max".
#' @return A tibble with Variable and Importance.
#' @export
heart_importance_logistic <- function(
    glm_caret,
    top_n = 20,
    cat_prefix = c("Sex","ChestPainType","FastingBS","RestingECG","ExerciseAngina","ST_Slope"),
    how = c("sum","max")
) {
  how <- match.arg(how)
  imp <- heart_glmnet_coef_importance(glm_caret)

  out <- imp |>
    dplyr::mutate(
      BaseFeature = dplyr::if_else(
        stringr::str_detect(.data$Feature, paste0("^(", paste(cat_prefix, collapse = "|"), ")_")),
        stringr::str_extract(.data$Feature, paste0("^(", paste(cat_prefix, collapse = "|"), ")")),
        .data$Feature
      )
    ) |>
    dplyr::group_by(.data$BaseFeature) |>
    dplyr::summarise(
      Importance = if (how == "sum") sum(.data$Importance, na.rm = TRUE) else max(.data$Importance, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(.data$Importance)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::transmute(
      Variable = forcats::fct_reorder(.data$BaseFeature, .data$Importance),
      Importance = .data$Importance
    )

  out
}

#' Plot feature importance as a horizontal bar chart
#'
#' @param imp_tbl A tibble with Variable and Importance.
#' @param title Plot title.
#' @param ylab Y-axis label.
#' @return A ggplot object.
#' @export
heart_plot_importance <- function(imp_tbl, title, ylab = "Importance") {
  ggplot2::ggplot(imp_tbl, ggplot2::aes(x = .data$Variable, y = .data$Importance, fill = .data$Importance)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    heart_theme()
}
