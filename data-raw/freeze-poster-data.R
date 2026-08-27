# OSF component GUID
NODE <- "afkcq"
REMOTE_DIR <- "2026-psychologicke-dny-poster/data"

# A2PA is the sole predictor for the whole poster
x <- c(
  "A2PA" = "A2PA"
)

# Outcomes compared between A2PA groups
Y <- c(
  "Age" = "Age",
  "Education" = "Education",
  "BMI" = "BMI",
  "SA" = "SA_comp",
  "Current PA (WHO)" = "cPA_WHO",
  "Current PA (excl. walking, chores)" = "cPA",
  "Current PA (excl. walking, chores, gardening)" = "cPA_no_gardening",
  "MMSE" = "MMSE",
  "GDS-15" = "GDS15",
  "Prob. depression" = "Depr",
  "GAI-SF" = "GAI",
  "Prob. anxiety" = "Anx",
  "FAQ" = "FAQ",
  "Mental activities" = "Total_MA",
  "Subj. health" = "Health",
  "RAVLT-DR" = "Delayed_recall_raw",
  "z(RAVLT-DR)" = "Delayed_recall_z",
  "TMT-B" = "TMT_B_raw",
  "z(TMT-B)" = "TMT_B_z",
  "BNT-30" = "BNT_30_raw",
  "z(BNT-30)" = "BNT_30_z",
  "Animal naming" = "VF_Animals_raw",
  "z(Animal naming)" = "VF_Animals_z",
  "Smoking" = "Smoking",
  "Alcohol" = "Alcoholing",
  "Hypertension" = "Hypertension",
  "Diabetes" = "Diabetes"
)

snapshot <-
  targets::tar_read(data, store = here::here("_targets")) |>
  dplyr::select(tidyselect::all_of(c(x, Y)))

commit <- system2("git", c("rev-parse", "--short", "HEAD"), stdout = TRUE)

cache <- tools::R_user_dir("midphysac", which = "cache")
dir.create(cache, recursive = TRUE, showWarnings = FALSE)

fname <- paste0("poster_data_", Sys.Date(), ".rds")
fpath <- file.path(cache, fname)
rpath <- file.path(cache, "FREEZE.md")

saveRDS(snapshot, fpath, version = 3)

writeLines(c(
  paste("#", fname),
  "",
  "Frozen analysis snapshot behind the Psychologicke dny 2026 poster",
  "(Olomouc, 9-11 September). Restricted: named contributors only.",
  "",
  paste0("- Source: josefmana/cosactiw-midphysac@", commit,
         ", target `data` from `_targets`"),
  paste0("- Frozen: ", Sys.Date(), " by Josef Mana <mana@psu.cas.cz>"),
  paste0("- Size: ", nrow(snapshot), " rows x ", ncol(snapshot), " columns"),
  paste0("- R ", getRversion(), ", saveRDS version 3"),
  "- Renamed and selected at freeze time; not filtered.",
  "",
  "## Structure",
  "",
  "```",
  utils::capture.output(utils::str(snapshot, width = 200, strict.width = "cut")),
  "```",
  "",
  "Read it with midphysac::poster_data(). Do not edit in place: if the",
  "pipeline changes, freeze a new dated file rather than replacing this one."
), rpath)

dest <- osfr::osf_mkdir(osfr::osf_retrieve_node(NODE), path = REMOTE_DIR)

osfr::osf_upload(
  dest,
  path = c(fpath, rpath),
  conflicts = "error"
)

cli::cli_alert_success("Uploaded {fname} and FREEZE.md to {NODE}: {REMOTE_DIR}.")
cli::cli_alert_info("Now set the default `file =` in R/poster_data.R to: {fname}")
