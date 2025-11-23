#' Fit regression models
#'
#' Using table of adjustments derived via
#' \code{adjustment_table}, this function
#' prepares model specifications for later
#' fitting.
#'
#' @param data Tibble or data.frame.
#' @param specs Tibble or data.frame.
#' @param contr Logical. Set sum contrasts to factors to
#'   avoid multicollinearity? Defaults to `TRUE`.
#'
#' @return A named nested list with regression models.
#'
#' @export
fit_models <- function(data, specs, contr = TRUE) {
  # Optionally set orthogonal contrasts:
  if (contr) {
    for (i in names(data)) {
      if (is.factor(data[, i])) {
        contrasts(data[, i]) <- contr.sum(length(levels(data[, i])))
      }
    }
  }
  # Optionally extarct variables for
  # Box-Cox transformation:
  use_transform <- any(c("transformed", "g-computation") %in% specs$estimate)
  if (use_transform) {
    trans_y <- specs |>
      dplyr::filter(likelihood == "gaussian") |>
      dplyr::distinct(outcome) |>
      dplyr::pull()
  }
  # Centre age:
  data$Age <- as.numeric(scale(data$Age, center = TRUE, scale = FALSE))
  # Fit it:
  types <- specs |>
    dplyr::distinct(estimate) |>
    dplyr::pull() |>
    rlang::set_names()
  lapply(types, function(t) {
    model_specs <- subset(specs, estimate == t)
    labs <- rlang::set_names(
      x = seq_len(nrow(model_specs)),
      nm = with(model_specs, paste0(outcome," ~ ",exposure," | ",moderator))
    )
    lapply(labs, function(i) {
      resp <- model_specs$outcome[i]
      form <- model_specs$formula[i]
      like <- model_specs$likelihood[i]
      # Transform response if appropriate:
      if (t %in% c("transformed", "g-computation") && resp %in% trans_y) {
        trans <- model_specs$transformation[i]
        if (trans == "Box-Cox") {
          if (resp == "MMSE") {
            data[[resp]] <- 30 - data[[resp]]
          }
          data[[resp]] <- boxcox_transform(
            data,
            y = resp,
            f = form,
            return_pars = FALSE
          )
        } else if (trans == "log(y+1)") {
          data[[resp]] <- log(data[[resp]] + 1)
        }
      }
      # Fit model:
      if (like == "gaussian") {
        lm(as.formula(form), data)
      } else {
        glm(as.formula(form), family = like, data = data)
      }
    })
  })
}
