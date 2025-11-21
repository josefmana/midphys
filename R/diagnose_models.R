#' Plot model diagnostics
#' 
#' Takes in a list of regressions fitted via
#' \code{fit_models} and visualises residual
#' checks as implemented in [performance::check_model()}.
#' 
#' @param model_list List. Regression models to check.
#' 
#' @returns A list with diagnostic plots of the same nested
#'   structure as `model_list`.
#' 
#' @export
diagnose_models <- function(model_list) {
  lapply(rlang::set_names(names(model_list)), function(type) {
    lapply(model_list[[type]], function(fit) {
      tryCatch(
        performance::check_model(fit),
        error = \(e) e
      )
    })
  })
}
