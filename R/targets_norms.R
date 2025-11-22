# Set-up targets for normative values:
targets_norms <- list(
  targets::tar_target(
    pvlt_data,
    command = here::here("data-raw", "PVLT_2012_NANOK_export_fin.xls - PVLT Czech.csv"),
    format = "file"
  ),
  targets::tar_target(
    pvlt_excl,
    command = here::here("data-raw", "PVLT_2012_NANOK_export_fin.xls - vyřazení.csv"),
    format = "file"
  ),
  targets::tar_target(
    nanok_demo,
    command = here::here("data-raw", "data_NANOK_2012-2013-2014_pro-Ondreje-Rydla_opr-MoCA.xls"),
    format = "file"
  ),
  targets::tar_target(
    ravlt_norms,
    command = here::here("data-raw", "ravlt_analyza.csv"),
    format = "file"
  ),
  targets::tar_target(
    memory_thresholds,
    command = extract_thresholds(
      pvlt_data_file = pvlt_data,
      pvlt_id_file = pvlt_excl,
      nanok_file = nanok_demo,
      ravlt_file = ravlt_norms,
      output = "thresholds"
    )
  ),
  targets::tar_target(
    memory_norms,
    command = extract_thresholds(
      pvlt_data_file = pvlt_data,
      pvlt_id_file = pvlt_excl,
      nanok_file = nanok_demo,
      ravlt_file = ravlt_norms,
      output = "norms"
    )
  )
)
