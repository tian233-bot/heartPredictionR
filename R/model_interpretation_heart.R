#' Extract Random Forest permutation importance (vip::vi)
#'
#' @param ranger_model A fitted ranger model.
#' @param top_n Top N variables.
#' @return tibble(Variable, Importance)
#' @examples
#' b <- heart_load_bundle()
#' if (requireNamespace("vip", quietly = TRUE)) {
#'   imp <- heart_importance_rf(b$rf_top_cv, top_n = 10)
#'   imp
#' }
#' @export
heart_importance_rf <- function(ranger_model, top_n = 20) {
  if (!requireNamespace("vip", quietly = TRUE)) {
    stop("Package 'vip' is required for importance functions. Please install it.")
  }

  vip::vi(ranger_model) |>
    dplyr::arrange(dplyr::desc(.data$Importance)) |>
    dplyr::slice_head(n = top_n) |>
    dplyr::mutate(Variable = forcats::fct_reorder(.data$Variable, .data$Importance))
}

#' Aggregate one-hot dummy importances back to raw variables (logistic model)
#'
#' @param glm_caret A caret::train(glmnet) object.
#' @param top_n Top N raw variables.
#' @param cat_prefix Categorical prefixes to aggregate (e.g. Sex_*).
#' @param how Aggregation method: sum or max.
#' @return tibble(Variable, Importance)
#' @examples
#' b <- heart_load_bundle()
#' if (requireNamespace("stringr", quietly = TRUE)) {
#'   imp <- heart_importance_logistic(b$glm_cv, top_n = 10, how = "sum")
#'   imp
#' }
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

#' Plot importance bar chart
#'
#' @param imp_tbl tibble with Variable, Importance.
#' @param title Plot title.
#' @param ylab y-axis label.
#' @return ggplot object.
#' @examples
#' b <- heart_load_bundle()
#' if (requireNamespace("vip", quietly = TRUE)) {
#'   imp <- heart_importance_rf(b$rf_top_cv, top_n = 10)
#'   p <- heart_plot_importance(imp, title = "TopK RF Importance")
#'   p
#' }
#' @export
heart_plot_importance <- function(imp_tbl, title, ylab = "Importance") {
  ggplot2::ggplot(imp_tbl, ggplot2::aes(x = .data$Variable, y = .data$Importance, fill = .data$Importance)) +
    ggplot2::geom_col(width = 0.65) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = title, x = NULL, y = ylab) +
    heart_theme()
}

#' Package ggplot theme
#'
#' @param base_size base font size.
#' @return ggplot theme.
#' @examples
#' heart_theme()
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

#' Extract glmnet coefficient importance (absolute coefficients at best lambda)
#'
#' @param glm_caret A caret::train object fitted with method='glmnet'.
#' @return tibble(Feature, Coef, Importance)
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
