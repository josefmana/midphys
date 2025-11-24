#' Perform inference via G-computation
#'
#' Based on a regression model (ideally with propensity
#' scores weights), computes marginal means (average
#' predictions)
#'
#' @param mods List. Models used for computation of the
#'   marginal means, including propensity score weights.
#' @param specs Tibble or data frame. Contains model
#'   specifications of the queries.
#' @param data List. Tibbles or data frames with raw data
#'   used in models fitting, including weights and clusters.
#'
#' @returns Tibble.
#'
#' @export
g_computations <- function(mods, specs, data) {
  labs <- rlang::set_names(seq_len(nrow(specs)), names(mods))
  purrr::map_dfr(labs, function(i) {
    # Prepare:
    y <- specs$outcome[i]
    x <- specs$exposure[i]
    m <- specs$moderator[i]
    e <- specs$effect[i]
    l <- specs$likelihood[i]
    int <- stringr::str_detect(specs$term[i], ":")
    ctrl <- specs$control[i]
    hyph <- specs$hypothesis[i]
    vars <- lapply(rlang::set_names(x), \(x0) hyph)
    comp <- dplyr::case_when(
      l == "binomial" ~ "lnratioavg",
      l == "gaussian" ~ "differenceavg"
    )
    trans <- unlist(dplyr::case_when(
      l == "binomial" ~ list("exp"),
      l == "gaussian" ~ list(NULL)
    ))
    model <- mods[[glue::glue("{y} ~ {x} | {e}")]]
    match <- specs$matching[i]
    d <- data[[match]]
    nd <- d[as.character(d[[x]]) == ctrl, ]
    # Compute:
    marginaleffects::avg_comparisons(
      model,
      newdata = nd,
      variables = vars,
      by = unlist(ifelse(m == "1", list(FALSE), list(m))),
      vcov = ~ subclass,
      wts = "weights",
      comparison = comp,
      transform = trans
    ) |>
      tibble::as_tibble() |>
      dplyr::mutate(y = y, x = x, m = m, e = e, .before = 1)
  }) |>
    dplyr::mutate(
      contrast = dplyr::if_else(
        stringr::str_detect(contrast, "ln"), "COSACTIW / NANOK", contrast
      ),
      mod = dplyr::case_when(
        m == "1" ~ "overall",
        #m == "cPA" ~ cPA,
        .default = NA
      ),
      Estimate = estimate,
      SE = std.error,
      df = NA
    ) |>
    dplyr::select(
      y, x, m, e, contrast, mod, Estimate, SE, df,
      statistic, p.value, tidyselect::starts_with("conf")
    )
}
