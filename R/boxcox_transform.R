#' Perform Box-Cox transformation
#'
#' Performs a Box-Cox transformation of all continuous
#' outcomes.
#'
#' @param data Tibble or data.frame. Contains imported
#'   raw data.
#' @param y Character. Name of the outcome variable to
#'   be transformed.
#' @param f Character. Regression formula.
#' @param return_pars Logical. Should the parameter lambda and
#'   additive constant to ensure positivity of raw scale be
#'   returned (`TRUE`) or the transformed variable (`FALSE`,
#'   default)?
#'
#' @returns Either a named numeric vector with the lambda
#'   parameter and constant or a vector of transformed `y`
#'   values.
#'
#' @export
boxcox_transform <- function(data, y, f, return_pars = FALSE) {
  # Add constant to ensure positivity:
  min_y <- min(data[[y]], na.rm = TRUE)
  if (min_y > 0) {
    constant <- 0
  } else {
    constant <- -min_y + 1e-5
    data[[y]] <- data[[y]] + constant
  }
  # Compute lambda:
  bc <- MASS::boxcox(
    as.formula(f),
    data = data,
    lambda = seq(-2, 2, 0.1),
    plotit = FALSE
  )
  lambda <- bc$x[which.max(bc$y)]
  if (return_pars) {
    c(constant = constant, lambda = lambda)
  } else {
    if (lambda == 0) {
      log(data[[y]])
    } else {
      (data[[y]]^lambda - 1) / lambda
    }
  }
}
