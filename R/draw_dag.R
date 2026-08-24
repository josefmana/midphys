#' Draw a directed acyclic graph (STUB -- always fails)
#'
#' Deliberately non-functional. The graph this function used to return is the
#' causal model of the *Acta Psychologica* brief report, in which the exposure
#' `mPA` was cohort membership (COSACTIW vs. NANOK) standing in for sustained
#' midlife physical activity. Commit 7756455 redefined `mPA` as a
#' dichotomisation of attitudes toward physical activity (`A2PA < 4`, levels
#' `PAW` / `non-PAW`) on the pooled COSACTIW + KOKOSA sample and left the graph
#' untouched. The graph is therefore no longer a statement about the data the
#' pipeline now carries:
#'
#' * it has no A2PA node at all;
#' * its `mPA -> cPA` arrow asserts a midlife-to-later-life temporal ordering
#'   that a concurrently measured attitude does not have;
#' * its selection node `S` represents recruitment into COSACTIW, not survival
#'   to age 80+ and participation in either cohort.
#'
#' The replacement is built under milestone M1, from the A2PA research
#' questions, and only after the A2PA instrument itself is documented. Until
#' then this function raises an error so that no downstream target, figure or
#' manuscript can quietly inherit the Acta assumptions.
#'
#' @param plot Logical. Unused. Retained so that existing call sites reach the
#'   error below rather than an argument-matching error.
#' @param ... Unused. Retained for the same reason.
#'
#' @returns Nothing. Always raises a condition of class
#'   `midphysac_stub_error`.
#'
#' @seealso `git show v1.0.0:R/draw_dag.R` for the Acta implementation,
#'   archived at Zenodo 10.5281/zenodo.20339954.
#'
#' @export
draw_dag <- function(plot = TRUE, ...) {
  rlang::abort(
    class = c("midphysac_stub_error", "midphysac_causal_layer"),
    message = c(
      "`draw_dag()` is stubbed: the causal layer is frozen until the A2PA estimands are fixed.",
      x = "The graph it used to return is the Acta model, with `mPA` = COSACTIW vs. NANOK and no A2PA node.",
      x = "Commit 7756455 redefined `mPA` as `A2PA < 4` (`PAW` / `non-PAW`) on pooled COSACTIW + KOKOSA; the graph was never updated.",
      i = "Acta implementation: `git show v1.0.0:R/draw_dag.R` (Zenodo 10.5281/zenodo.20339954).",
      ">" = "Unblocked by M1 `Build the new DAG for the A2PA question`, which must carry a survival-and-participation selection node.",
      ">" = "Write a new graph. Do not restore the old one."
    )
  )
}
