#' Load the frozen poster data
#'
#' Downloads the frozen analysis snapshot from the restricted OSF component on
#' first use and reads it from a local cache thereafter. Needs a personal access
#' token in `OSF_PAT` and contributor access to the component.
#'
#' @param file Character(1). File name of the freeze on OSF.
#' @param node Character(1). GUID of the restricted OSF component.
#' @param refresh Logical. Download again even if a cached copy exists.
#'
#' @return A tibble, exactly as frozen. No filtering or recoding is applied.
#'
#' @export
read_poster_data <- function(
    file = "poster_data_2026-08-27.rds",
    node = "PENDING",
    refresh = FALSE
) {
  cache <- tools::R_user_dir("midphysac", which = "cache")
  dir.create(cache, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(cache, file)

  if (refresh || !file.exists(path)) {
    if (!nzchar(Sys.getenv("OSF_PAT"))) {
      cli::cli_abort(c(
              "No OSF_PAT found. Create a token at https://osf.io/settings/tokens",
        "i" = "Add OSF_PAT=<token> to ~/.Renviron and restart R."
      ))
    }
    hit <- osfr::osf_ls_files(
      osfr::osf_retrieve_node(node),
      pattern = file,
      n_max = 1e5
    )
    if (nrow(hit) != 1L) {
      cli::cli_abort(c(
              "Expected exactly one file on OSF node {node}.",
        "x" = "Found {nrow(hit)} files."
      ))
    }
    osfr::osf_download(hit, path = cache, conflicts = "overwrite")
  }

  readRDS(path)
}
