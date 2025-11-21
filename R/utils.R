#' Print rounded number
#' 
#' @param x Numeric. Number to be rounded.
#' @param d Numeric. Decimal places (defaults to 2).
#' 
#' @return A character with `d` decimals.
#' 
#' @export
rprint <- function(x, d = 2) {
  x0 <- round(x, d)
  dec <- paste0("%.",d,"f")
  sprintf(dec, x0)
}


#' Drop leading zero
#' 
#' @param x Numeric. Number to be rounded.
#' @param d Numeric. Decimal places (defaults to 3).
#' 
#' @return A character with `d` decimals and no leading
#'   zero. If `x < 0.001`, the `"< .001"` is printed.
#' 
#' @export
zerolead <- function(x, d = 3) {
  ifelse(x < .001, "< .001", stringr::str_replace(rprint(x, d), "0", ""))
}


#' Remove set brackets
#' 
#' @param x Character.
#' 
#' @return A character with curly brackets removed.
#' 
#' @export
remove_brackets <- function(x) {
  stringr::str_replace_all(x, c("\\{" = "", "\\}" = ""))
}
