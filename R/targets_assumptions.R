# Set-up targets for assumption drawing:
targets_assumptions <- list(
  targets::tar_target(
    dag,
    command = draw_dag(plot = FALSE)
  ),
  targets::tar_target(
    dag_plot,
    command = draw_dag(plot = TRUE)
  ),
  targets::tar_target(
    adjustment_sets,
    command = adjustment_table(dag)
  )
)
