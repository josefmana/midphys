#' Compute marginal means
#'
#' Based on a regression model (ideally with propensity
#' scores weights), computes marginal means (average
#' predictions)
#'
#' @param mods List. Models used for computation of the
#'   marginal means, including propensity score weights.
#' @param specs Tibble or data frame. Contains model
#'   specifications of the queries.
#'
#' @returns Tibble.
#'
#' @export
compute_predictions <- function(mods, specs) {
  labs <- rlang::set_names(seq_len(nrow(specs)), names(mods))
  purrr::map_dfr(labs, function(i) {
    y <- specs$outcome[i]
    x <- specs$exposure[i]
    m <- specs$moderator[i]
    model <- mods[[glue::glue("{y} ~ {x} | {m}")]]
    vars <- unlist(ifelse(
      m == 1,
      yes = list(x),
      no = list(c(x, m))
    ))
    marginaleffects::avg_predictions(
      model,
      variables = vars,
      wts = "weights"
    ) |>
      tibble::as_tibble() |>
      dplyr::mutate(y = y, x = x, m = m)
  }) |>
      dplyr::mutate(
        mod = dplyr::case_when(
          m == "1" ~ "overall",
          m == "cPA" ~ cPA,
          m == "Education" ~ Education
        ),
        group = dplyr::case_when(
          x == "mPA" ~ mPA,
          x == "cPA" ~ cPA,
          x == "Education" ~ Education
        ),
        Est_SE = glue::glue("{rprint(estimate, 2)} ({rprint(std.error, 2)})"),
        Est_CI = glue::glue("{rprint(estimate, 2)} [{rprint(conf.low, 2)}, {rprint(conf.high, 2)}]")
      ) |>
      dplyr::select(y, x, m, mod, group, Est_SE, Est_CI)
}
