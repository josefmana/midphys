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
#' @return Nested named list with regression models (fits)
#'   and transformation objects (transforms) if applicable.
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
  # Optionally do normalization:
  use_transform <- any(c("transformed", "g-computation") %in% specs$estimate)
  if (use_transform) {
    trans_y <- specs |>
      dplyr::filter(likelihood == "gaussian") |>
      dplyr::distinct(outcome) |>
      dplyr::pull()
    transforms <- lapply(rlang::set_names(trans_y), function(y) {
      bestNormalize::bestNormalize(data[[y]], k = 10, r = 100)
    })
    for (y in trans_y) {
      data[[paste0(y, "_trans")]] <- transforms[[y]]$x.t
    }
  } else {
    transforms <- NULL
  }
  # Centre age:
  data$Age <- as.numeric(scale(data$Age, center = TRUE, scale = FALSE))
  # Fit it:
  types <- specs |>
    dplyr::distinct(estimate) |>
    dplyr::pull() |>
    rlang::set_names()
  # Fit the models:
  fits <- lapply(types, function(t) {
    model_specs <- subset(specs, estimate == t)
    labs <- rlang::set_names(
      x = seq_len(nrow(model_specs)),
      nm = with(model_specs, paste0(outcome, " ~ ", exposure, " | ", moderator))
    )
    lapply(labs, function(i) {
      form <- model_specs$formula[i]
      like <- model_specs$likelihood[i]
      if (like == "gaussian") {
        lm(as.formula(form), data)
      } else {
        glm(as.formula(form), family = like, data = data)
      }
    })
  })
  # Return fits and transformation info:
  lapply(rlang::set_names(c("fits", "transforms")), \(i) get(i))
}
