#' Fit regression models
#' 
#' Using table of adjustments derived via
#' \code{adjustment_table}, this function
#' prepares model specifications for later
#' fitting.
#' 
#' @param data Tibble or data.frame.
#' @param specs Tibble or data.frame.
#' @param log_1 Character. Which variables `y` should be
#'   analysed as their `log(y+1)` versions? Defaults to
#'   `c("GDS15","GAI")`.
#' @param contr Logical. Set sum contrasts to factors to
#'   avoid multicollinearity? Defaults to `TRUE`.
#' 
#' @return A named nested list with regression models.
#' 
#' @export
fit_models <- function(
    data,
    specs,
    log_1 = c("GDS15","GAI"),
    contr = TRUE) {
  # Optionally log transform:
  if (!is.null(log_1)) {
    data <- data |>
      dplyr::mutate(dplyr::across(
        .cols = tidyselect::all_of(log_1),
        .fns = \(x) log(x + 1)
      ))
  }
  # Optionally set orthogonal contrasts:
  if (contr) {
    for(i in names(data)) {
      if (is.factor(data[, i])) {
        contrasts(data[, i]) <- contr.sum(length(levels(data[, i])))
      }
    }
  }
  # Optionally mutate FAQ: 
  if (c(unique(specs[specs$outcome == "FAQ", "likelihood"])) == "binomial") {
    data$FAQ <- data$FAQ / 10
  }
  # Centre age:
  data$Age <- as.numeric(scale(data$Age, center = TRUE, scale = FALSE))
  # Fit it:
  # compute regressions
  lapply(rlang::set_names(paste0(c("un", ""), "adjusted")), function(t) {
    model_specs <- subset(specs, estimate == t)
    labs <- rlang::set_names(
      x = seq_len(nrow(specs)),
      nm = with(model_specs, paste0(outcome," ~ ",exposure," | ",moderator))
    )
    lapply(labs, function(i) {
      with(specs, glm(
        formula = as.formula(formula[i]),
        family = likelihood[i],
        data = data,
        weights = unlist(ifelse(
          outcome[i] == "FAQ" & likelihood[i] == "binomial",
          yes = list(rep(10, nrow(data))),
          no = list(NULL)
        ))
      ))
    })
  })
}
