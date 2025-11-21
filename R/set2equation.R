#' Change sets to equations
#' 
#' Takes in sets of covariates derived by \code{adjustment_table}
#' and changes them to equations that can be used as \code{formula}.
#' 
#' @param x Character. Contains sets to be transformed.
#' @param bracket Logical. Bracket covariates into interactions?
#'   Defaults to `FALSE`.
#' 
#' @returns Character vector with components that can be changed
#'   to formulas.
#' 
#' @export
set2equation <- function(x, bracket = FALSE) {
  sapply(seq_along(x), function(i) {
    dplyr::if_else(
      bracket,
      true = stringr::str_replace_all(x[i], c("\\{" = "\\(", "\\}" = "\\)", "\\, " = " + ")),
      false = stringr::str_replace_all(x[i], c("\\{" = "", "\\}" = "", "\\, " = " + "))
    )
  }, USE.NAMES = FALSE)
}
