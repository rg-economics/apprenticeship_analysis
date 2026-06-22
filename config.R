################################################################################
## Apprenticeship Analysis – Shared Configuration
## Source this file at the top of all three scripts:
##   source("config.R")
################################################################################

######################
# Project colours
######################

colour_primary   <- "#2916E0"
colour_secondary <- "#5B4CF0"
colour_accent    <- "#8E84FF"
colour_text      <- "#222222"
colour_panel     <- "#F8F8F8"

# Extra Illuminate-compatible colours (used in stacked/categorical charts)
colour_blue_mid   <- "#0072BC"
colour_blue_light <- "#4CB3E6"
colour_teal       <- "#00A6A6"
colour_grey       <- "#8A8F98"

######################
# Folders
######################

output_folder <- "outputs"
data_folder <- "data"

derived_data_folder <- file.path(output_folder, "data")
exec_output_folder  <- file.path(output_folder, "exec_summary_facts")

dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(data_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(derived_data_folder, recursive = TRUE, showWarnings = FALSE)
dir.create(exec_output_folder, recursive = TRUE, showWarnings = FALSE)

######################
# Xplore / EES data files
######################

# These are expected to be in data_folder after you upload/download the Xplore
# release files. Do not download these via Google Drive unless necessary.

xplore_files <- list(
  routes_standards        = file.path(data_folder, "app-routes-standards-202425-q4.csv"),
  subject_standards       = file.path(data_folder, "app-subject-standards-202425-q4.csv"),
  subject_standards_inyr  = file.path(data_folder, "apps_17_subject_standards_202526_6.csv"),
  provider_starts         = file.path(data_folder, "app-provider-starts-202425-q4.csv"),
  provider_starts_inyr    = file.path(data_folder, "apps_23_provider_starts_202526_6.csv"),
  narts_provider_standard = file.path(data_folder, "app-narts-provider-level-fwk-std.csv"),
  learner_detailed        = file.path(data_folder, "app-learner-detailed-202425-q4.csv"),
  geography_population    = file.path(data_folder, "app-geography-population-202425-q4.csv"),
  geography_detailed      = file.path(data_folder, "app-geography-detailed-202425-q4.csv"),
  historical_summary      = file.path(data_folder, "app-historical-summary-to-2425.csv")
)

######################
# Non-Xplore / external data paths
######################

# These are the files that are NOT part of the main Xplore release download,
# so on Posit Cloud we download them from Google Drive if they are missing.

skills_england_standards_path <- file.path(data_folder, "Apprenticeships.csv")

provider_region_breakdowns_path <- file.path(
  data_folder,
  "2024_25-alllevels-allagegroups-provider_breakdowns.csv"
)

provider_standard_starts_path <- file.path(
  data_folder,
  "2024_25-starts---subjects-and-standards.csv"
)

provider_lad_breakdowns_path <- file.path(
  data_folder,
  "lad-2024_25.csv"
)

######################
# Google Drive IDs for non-Xplore data
######################

# Fill in the missing IDs once you have copied the Google Drive file IDs.
# The Apprenticeships.csv ID is the one you gave me.

google_drive_files <- tibble::tribble(
  ~name, ~file_id, ~path,
  "Skills England standards",
  "1BBwYje2zn7GCSOZ-5gfOMc7jCQyhQ8vc",
  skills_england_standards_path,
  
  "Provider region breakdowns",
  "1Ih4_wKDfbeJNrmVaryChWk9Ug_E0Bn9f",
  provider_region_breakdowns_path,
  
  "Provider-standard starts dashboard",
  "1V3o32Kpkv9g7sDrag9dmjoPYJDivC9la",
  provider_standard_starts_path,
  
  "Provider-LAD breakdowns",
  "19916t2k3N0S-oCgk5EE4gvatuTukOMDX",
  provider_lad_breakdowns_path,

  "Routes standards",
  "1fJXpC8hWZtRiETwEjopZmuacV_VQWoms",
  xplore_files$routes_standards,
  
  "Subject standards",
  "15KSMtBBHkcr9IKBsh4gzW5qfLV7BKN5o",
  xplore_files$subject_standards,
  
  "Subject standards in-year",
  "13rA2hErI3WzyWqf6EwApMccm5DIMi4OB",
  xplore_files$subject_standards_inyr,
  
  "Provider starts",
  "1T9V3qMfIBUFPBJhwgghZ0letjLJpBcb9",
  xplore_files$provider_starts,
  
  "Provider starts in-year",
  "1-9arp2gJ95OAjflfdTmQc6Z6TD4B75Z_",
  xplore_files$provider_starts_inyr,
  
  "NARTS provider standard",
  "1Y1ERXF7QRoeXS5EamuXBcnt2tp-65vfP",
  xplore_files$narts_provider_standard,
  
  "Learner detailed",
  "1TQAGhC5ULvGTcJma4oyWiZQ2n9cJPcb0",
  xplore_files$learner_detailed,
  
  "Geography population",
  "1eGheG3mgPAS8O9P0_qZIgroJqDB9C3SZ",
  xplore_files$geography_population,
  
  "Geography detailed",
  "17SDcoZptEga2NHur6P8_mH4byzbde2Vd",
  xplore_files$geography_detailed,
  
  "Historical summary",
  "19H3DRccG0Kpj46Wt_j3j6uNGBINqh27d",
  xplore_files$historical_summary
  
  
  
)

######################
# Boundary file paths
######################

# Full-resolution (BFC) shapefiles - only needed by 00_prepare_boundaries.R,
# which is run once locally. This folder is gitignored (files exceed
# GitHub's 100 MB limit) and is not needed on Posit Cloud.
lsip_shp_path <- "boundaries/LSIP_OCT_2025/LSIP_OCT_2025_EN_BFC.shp"

lad_shp_path <- "boundaries/Local_Authority_Districts_DEC_2025/LAD_DEC_2025_UK_BFC.shp"

region_shp_path <- "boundaries/Regions_December_2025/RGN_DEC_2025_EN_BFC.shp"

# Simplified boundaries created by 00_prepare_boundaries.R - small enough to
# commit to git, and what 02_analysis.R actually uses.
lsip_light_path   <- "boundaries_light/lsip_boundaries.rds"
lad_light_path    <- "boundaries_light/lad_boundaries.rds"
region_light_path <- "boundaries_light/region_boundaries.rds"

######################
# Google Drive download helper
######################

download_google_drive_file <- function(file_id, path, name = path, overwrite = FALSE) {
  
  if (is.na(file_id) || file_id == "" || file_id == "PUT_GOOGLE_DRIVE_FILE_ID_HERE") {
    stop(
      paste0(
        "Missing Google Drive file ID for: ", name, "\n",
        "Please add the file ID in google_drive_files in config.R."
      )
    )
  }
  
  if (file.exists(path) && !overwrite) {
    message("Already exists, skipping: ", path)
    return(invisible(path))
  }
  
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    install.packages("googledrive")
  }
  
  googledrive::drive_deauth()
  
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  
  message("Downloading: ", name, " -> ", path)
  
  googledrive::drive_download(
    googledrive::as_id(file_id),
    path = path,
    overwrite = TRUE
  )
  
  invisible(path)
}

download_external_data <- function(overwrite = FALSE) {
  
  purrr::pwalk(
    google_drive_files,
    function(name, file_id, path) {
      download_google_drive_file(
        file_id = file_id,
        path = path,
        name = name,
        overwrite = overwrite
      )
    }
  )
  
  invisible(TRUE)
}

check_required_files <- function(paths) {
  
  missing_paths <- paths[!file.exists(paths)]
  
  if (length(missing_paths) > 0) {
    stop(
      paste0(
        "Missing required files:\n",
        paste(missing_paths, collapse = "\n")
      )
    )
  }
  
  message("All required files found.")
  invisible(TRUE)
}
