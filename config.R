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
# Output folder
######################

output_folder <- "outputs"

if (!dir.exists(output_folder)) {
  dir.create(output_folder, recursive = TRUE)
}

######################
# Data folder
######################

# Folder containing the Xplore Education Statistics CSV files
data_folder <- "data"

######################
# Additional data file paths
######################

# Skills England apprenticeship standards list (funding bands, routes, status)
skills_england_standards_path <- "data/Apprenticeships.csv"

# Provider type / delivery region breakdowns (downloaded separately from EES)
provider_region_breakdowns_path <- "data/2024_25-alllevels-allagegroups-provider_breakdowns.csv"

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
