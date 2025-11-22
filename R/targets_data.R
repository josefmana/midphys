# Set-up targets for data import:
targets_data <- list(
  targets::tar_target(
    datafile,
    command = here::here("data-raw", "COSACTIW_NANOK_pro-jamovi-oprav.xlsx"),
    format = "file"
  ),
  targets::tar_target(
    data,
    command = import_data(
      file = datafile,
      sheet = "cosactiw+nanok",
      norms = memory_norms,
      thresholds = memory_thresholds,
      thres_type = "mean"
    )
  )
)
