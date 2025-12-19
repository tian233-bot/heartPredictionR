#' Heart prediction feature engineering (deployment-safe)
#'
#' Suggests three engineered features that are simple, interpretable, and stable:
#' \itemize{
#'   \item \code{age_decade}: age binned to decades
#'   \item \code{high_bp_flag}: 1 if \code{RestingBP >= 140} mm Hg, else 0
#'   \item \code{high_chol_flag}: 1 if \code{Cholesterol >= 240} mg/dl, else 0
#' }
#'
#' This function does not modify your original variables beyond numeric coercion.
#'
#' @param df A data.frame / tibble with at least \code{Age}, \code{RestingBP}, \code{Cholesterol}.
#' @return A tibble with engineered columns appended.
#' @export
heart_add_features <- function(df) {
  df <- tibble::as_tibble(df)

  df$Age <- as.numeric(df$Age)
  df$RestingBP <- as.numeric(df$RestingBP)
  df$Cholesterol <- as.numeric(df$Cholesterol)

  dplyr::mutate(
    df,
    age_decade     = floor(.data$Age / 10) * 10,
    high_bp_flag   = dplyr::if_else(.data$RestingBP >= 140, 1L, 0L),
    high_chol_flag = dplyr::if_else(.data$Cholesterol >= 240, 1L, 0L)
  )
}

#' Coerce raw inputs to model-friendly types
#'
#' Numeric columns are coerced to numeric (hard fail if NA is introduced).
#' Categorical columns are coerced to character. (The model/recipe will handle factorisation.)
#'
#' @param df data.frame of predictors.
#' @param numeric_cols Numeric feature names.
#' @param cat_cols Categorical feature names.
#' @return A tibble with coerced columns.
#' @export
heart_coerce_inputs <- function(
    df,
    numeric_cols = c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak"),
    cat_cols = c("Sex","ChestPainType","FastingBS","RestingECG","ExerciseAngina","ST_Slope")
) {
  df <- tibble::as_tibble(df)

  for (cc in intersect(numeric_cols, names(df))) {
    df[[cc]] <- suppressWarnings(as.numeric(df[[cc]]))
    if (anyNA(df[[cc]])) {
      bad <- which(is.na(df[[cc]]))
      stop(sprintf("Non-numeric or missing value in '%s' at row(s): %s", cc, paste(bad, collapse = ",")))
    }
  }

  for (cc in intersect(cat_cols, names(df))) {
    df[[cc]] <- as.character(df[[cc]])
  }

  df
}

#' Validate that required columns exist
#'
#' @param df data.frame.
#' @param required Required column names.
#' @return Invisible TRUE or error.
#' @export
heart_assert_required_cols <- function(df, required) {
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  invisible(TRUE)
}
