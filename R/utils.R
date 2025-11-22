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


#' Adjust for Multiple Comparisons
#'
#' Evaluate a vector of p-values for statistical significance on
#' 5% False Discovery Rate according to the Benjamini-Hochberg
#' procedure.
#'
#' @param p A vector of p-values.
#'
#' @returns A vector indicating statistical significance after
#'    adjustment.
#'
#' @export
bh_adjust <- function(p) {
  # Extract threshold:
  bh_thres <- data.frame(
    p = sort(p), # sort p-values from smallest to largest
    thres = .05 * (seq_along(p)) / length(p) # prepare BH thresholds for each p-value
  ) |>
    dplyr::mutate(sig = dplyr::if_else(p <= thres, TRUE, FALSE)) |>
    dplyr::filter(sig == TRUE) |>
    dplyr::select(thres) |>
    max() |>
    suppressWarnings() # for cases with nothing significant
  # Return stars based on this threshold:
  dplyr::if_else(p < bh_thres, "*", "")
}
