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
      diffs <- means[[i]]$contrasts |>
        tibble::as_tibble() |>
        dplyr::left_join(
          means[[i]]$contrasts |>
            confint() |>
            tibble::as_tibble()
        )
      ncols <- ncol(diffs)
      diffs |>
        dplyr::select(tidyselect::all_of(c(1:5, (ncols - 3):ncols))) |>
        `colnames<-`(
          c("contrast", "mod", "Estimate", "SE", "df", "statistic", "p.value", "conf.low", "conf.high")
        ) |>
        dplyr::mutate(
          y = outcome[i],
          x = exposure[i],
          m = moderator[i],
          e = effect[i],
          .before = 1
        )
    })
  })
}
