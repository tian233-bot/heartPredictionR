#' @keywords internal
`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

#' @keywords internal
clamp01 <- function(p) pmin(1, pmax(0, p))
