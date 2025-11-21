#' Extract marginal means
#' 
#' Extracts and formats marginal means computed
#' by \code{compute_means}.
#' 
#' @param means List. Expected marginal means such as
#'   those computed by \code{compute_means}.
#' @param specs Tibble or data.frame. Contains research
#'   questions operationalisation.
#' @param digits Integer. Decimal places.
#' 
#' @returns A tibble with formatted marginal means.
#'
#' @export
extract_means <- function(means, specs, digits = 2) {
  purrr::map_dfr(seq_len(nrow(specs)), function(i) {
    with(specs, {
      means[[i]]$emmeans |>
        tibble::as_tibble() |>
        `colnames<-`(c("group", "mod", "Est", "SE", "df", "low.CL", "upp.CL")) |>
        dplyr::mutate(y = outcome[i], x = exposure[i], m = moderator[i], .before = 1) |>
        dplyr::mutate(Est = paste0(rprint(Est, digits), "\n(", rprint(SE, digits),")")) |>
        dplyr::select(y, x, m, mod, group, Est)
    })
  })
}