# Set-up targets for regressions fitting:
targets_regressions <- list(
  targets::tar_target(
    specs,
    command = model_specs(
      table = adjustment_sets
    )
  ),
  targets::tar_target(
    models,
    command = fit_models(
      data,
      specs,
      contr = TRUE
    )
  ),
  targets::tar_target(
    diagnostics,
    command = diagnose_models(models)
  )
)
