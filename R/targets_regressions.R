# Set-up targets for regressions fitting:
targets_regressions <- list(
  targets::tar_target(
    specs,
    command = model_specs(
      table = adjustment_sets,
      faq = "continuous"
    )
  ),
  targets::tar_target(
    models,
    command = fit_models(
      data,
      specs,
      log_1 = NULL,
      contr = TRUE
    )
  ),
  targets::tar_target(
    diagnostics,
    command = diagnose_models(models)
  )
)
