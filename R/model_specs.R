#' Set models' specifications (STUB -- always fails)
#'
#' Deliberately non-functional, and the most dangerous of the three: unlike the
#' DAG and the adjustment table, this function did not merely become stale, it
#' became silently wrong.
#'
#' It set `control = "NANOK"` for every row with `exposure == "mPA"`. Commit
#' 7756455 kept the column name `mPA` but changed its levels to `PAW` /
#' `non-PAW`, so the `case_when()` branch still fires and still hands a
#' plausible-looking string downstream. In `g_computations()` that string
#' reaches `nd <- d[as.character(d[[x]]) == ctrl, ]`, which now selects **zero
#' rows**: the specification is invalid at the point it is written, but nothing
#' says so there, and whatever `marginaleffects` eventually reports is about an
#' empty control group rather than about a dead contrast. `estimand = "ATC"`,
#' `hypothesis = "revpairwise"` and the `analysis` codes are Acta-specific in
#' the same way.
#'
#' Model specifications are written from registered estimands. Under the M0-M5
#' plan that is milestone M4, after the pre-registration is public (M3), not
#' before.
#'
#' @param table Unused. Retained so that existing call sites reach the error
#'   below rather than an argument-matching error.
#' @param ... Unused. Retained for the same reason.
#'
#' @returns Nothing. Always raises a condition of class
#'   `midphysac_stub_error`.
#'
#' @seealso `git show v1.0.0:R/model_specs.R` for the Acta implementation.
#'
#' @export
model_specs <- function(table, ...) {
  rlang::abort(
    class = c("midphysac_stub_error", "midphysac_causal_layer"),
    message = c(
      "`model_specs()` is stubbed: specifications are written from registered estimands, not before them.",
      x = "It hard-coded `control = \"NANOK\"`, a level that commit 7756455 removed when it set `mPA` to `PAW` / `non-PAW`.",
      x = "The branch matched on the *name* `mPA`, which survived the redefinition, so the dead level propagated silently: `g_computations()` subsets the control group with it and gets zero rows.",
      x = "`estimand = \"ATC\"`, `hypothesis = \"revpairwise\"` and the `analysis` codes are Acta-specific too.",
      i = "Acta implementation: `git show v1.0.0:R/model_specs.R`.",
      ">" = "Unblocked by M4 `Rebuild model_specs.R from the registered estimands`, i.e. after M3 registration.",
      ">" = "Contrasts must then be read from the data's factor levels, never named as string literals."
    )
  )
}
