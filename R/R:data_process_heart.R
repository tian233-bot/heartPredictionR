#' Heart prediction feature engineering (deployment-safe)
#'
#' Adds engineered features used by the trained models:
#' `age_decade`, `high_bp_flag`, `high_chol_flag`.
#'
#' @param df A data.frame / tibble with at least Age, RestingBP, Cholesterol.
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
    high_bp_flag   = dplyr::if_else(.data$RestingBP >= 140, 1, 0),
    high_chol_flag = dplyr::if_else(.data$Cholesterol >= 240, 1, 0)
  )
}

#' Coerce raw inputs to model-friendly types
#'
#' Designed to make `recipes::bake()` stable in deployment:
#' numeric columns are forced to numeric (fail-fast if NA introduced),
#' categorical columns are forced to character (recipe will handle levels).
#'
#' @param df data.frame of predictors.
#' @param numeric_cols numeric feature names.
#' @param cat_cols categorical feature names.
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
      stop(sprintf("Non-numeric or missing value in '%s' at row(s): %s",
                   cc, paste(bad, collapse = ",")))
    }
  }

  # Force categorical predictors to character to avoid bake() factor warnings
  for (cc in intersect(cat_cols, names(df))) {
    df[[cc]] <- as.character(df[[cc]])
  }

  df
}

#' Validate that required columns exist
#'
#' @param df data.frame.
#' @param required required column names.
#' @return invisible(TRUE) or error.
#' @export
heart_assert_required_cols <- function(df, required) {
  miss <- setdiff(required, names(df))
  if (length(miss) > 0) {
    stop("Missing required columns: ", paste(miss, collapse = ", "))
  }
  invisible(TRUE)
}
