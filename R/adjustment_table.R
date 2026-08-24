#' Extract adjustment table (STUB -- always fails)
#'
#' Deliberately non-functional. The table this function used to return was not
#' derived from the DAG alone: the outcome/exposure grid (`Cognition`, `Affect`
#' and `cPA` regressed on `mPA` and `cPA`), the total/direct split and the
#' canonical/minimal choice were all hard-coded for the *Acta Psychologica*
#' brief report. Two further Acta-era artefacts were baked in:
#'
#' * `stringr::str_replace("Affect, ", "")`, a hand-edit of the minimal
#'   adjustment set, annotated in the source as a "dirty trick" to turn the
#'   direct effect on `Cognition` into a "cPA-free" effect;
#' * `stringr::str_replace("mPA", "Cosactiw")` in the matching formula, which
#'   silently renames the exposure to the COSACTIW membership indicator.
#'
#' Neither survives the redefinition of `mPA` in commit 7756455, and neither is
#' a consequence of any graph. Rebuilding this table before the new DAG and the
#' estimand ladder exist would reproduce the same failure: adjustment sets that
#' look derived but are in fact asserted.
#'
#' @param DAG Unused. Retained so that existing call sites reach the error
#'   below rather than an argument-matching error.
#' @param ... Unused. Retained for the same reason.
#'
#' @returns Nothing. Always raises a condition of class
#'   `midphysac_stub_error`.
#'
#' @seealso `git show v1.0.0:R/adjustment_table.R` for the Acta implementation.
#'
#' @export
adjustment_table <- function(DAG, ...) {
  rlang::abort(
    class = c("midphysac_stub_error", "midphysac_causal_layer"),
    message = c(
      "`adjustment_table()` is stubbed: adjustment sets cannot be derived before the new DAG exists.",
      x = "Its outcome/exposure grid was hard-coded for the Acta analysis, not read off any graph.",
      x = "It carried two Acta-era string hacks: the `\"Affect, \"` deletion from the minimal set, and `mPA` -> `Cosactiw` in the matching formula.",
      i = "Acta implementation: `git show v1.0.0:R/adjustment_table.R`.",
      ">" = "Unblocked by M1 `Derive adjustment sets and rewrite adjustment_table.R`, which depends on the new DAG and on the A2PA operationalisation decision.",
      ">" = "The rewrite must take the grid from the E1/E2/E3 estimand ladder, and every set from `ggdag`, with no post-hoc string edits."
    )
  )
}
