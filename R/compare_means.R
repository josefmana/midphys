#' Compare marginal means
#'
#' Given expected marginal means (e.g., calculated via
#' \code{compute_means}) and contrast specifications,
#'
#' @param means List. Expected marginal means such as
#'   those computed by \code{compute_means}.
#' @param specs Tibble or data.frame. Contains research
#'   questions operationalisation.
#'
#' @returns A tibble with pairwise comparisons.
#'
#' @export
compare_means <- function(means, specs) {
  purrr::map_dfr(seq_len(nrow(specs)), function(i) {
    with(specs, {
      # use the following to get confidence intervals:
      # means[[i]] %>% confint()
      diffs <- means[[i]]$contrasts |>
        tibble::as_tibble()
      ncols <- ncol(diffs)
      diffs |>
        dplyr::select(tidyselect::all_of(c(1:5, (ncols - 1):ncols))) |>
        `colnames<-`(c("contrast", "mod", "Comparison", "SE", "df", "test. stat.", "p value")) |>
        dplyr::mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1)
    })
  })
}
