#' Compute marginal means
#'
#' Given model fits and contrast specifications,
#' computes expected marginal means of grouping
#' of interest.
#'
#' @param fits List. Model fits such as those generated
#'   by \code{fit_models}.
#' @param specs Tibble or data.frame. Contains research
#'   questions operationalisation.
#'
#' @returns Named list containing `emmeans` objects with
#'   marginal means of interest per model.
#'
#' @export
compute_means <- function(fits, specs) {
  labs <- rlang::set_names(
    x = seq_len(nrow(specs)),
    nm = names(fits)
  )
  lapply(labs, function(i) {
    with(specs, {
      emmeans::emmeans(
        object = fits[[paste0(outcome[i]," ~ ",exposure[i]," | ",effect[i])]],
        specs = formula(paste0("pairwise ~ ",exposure[i])),
        by = moderator[i],
        type = "response"
      )
    })
  })
}
