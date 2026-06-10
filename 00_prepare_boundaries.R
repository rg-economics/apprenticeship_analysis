################################################################################
## Apprenticeship Analysis
## 00_prepare_boundaries.R
##
## One-off script: run ONCE on a machine that has the full-resolution (BFC)
## ONS shapefiles in boundaries/. It simplifies them and saves small,
## standardised sf objects to boundaries_light/ (a few MB each), which are
## committed to git and used by 02_analysis.R.
##
## The raw boundaries/ folder is gitignored (the LAD BFC shapefile alone is
## ~116 MB, above GitHub's 100 MB limit) and is NOT needed on Posit Cloud.
##
## Depends on: config.R (raw shapefile paths)
## Suggested:  install.packages("rmapshaper") for topology-preserving
##             simplification (falls back to st_simplify if missing)
################################################################################

library(tidyverse)
library(sf)

source("config.R")

######################
# Helper: simplify an sf object
######################

# rmapshaper preserves shared borders between polygons (no slivers/gaps);
# st_simplify is the fallback and is fine at report scale.
simplify_boundaries <- function(x, keep = 0.05, dtolerance = 200) {
  if (requireNamespace("rmapshaper", quietly = TRUE)) {
    rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE)
  } else {
    message("rmapshaper not installed - using st_simplify fallback")
    st_simplify(x, preserveTopology = TRUE, dTolerance = dtolerance)
  }
}

######################
# Read, standardise, simplify, save
######################

dir.create("boundaries_light", showWarnings = FALSE)

# LSIP boundaries
lsip_boundaries <- st_read(lsip_shp_path, quiet = TRUE) %>%
  rename(
    lsip_code = LSIP25CD,
    lsip_name_boundary = LSIP25NM
  ) %>%
  st_transform(27700) %>%
  st_make_valid() %>%
  simplify_boundaries()

# LAD boundaries (England only)
lad_boundaries <- st_read(lad_shp_path, quiet = TRUE) %>%
  rename(
    lad_code = LAD25CD,
    lad_name_boundary = LAD25NM
  ) %>%
  filter(str_starts(lad_code, "E")) %>%
  st_transform(27700) %>%
  st_make_valid() %>%
  simplify_boundaries()

# Region boundaries
region_boundaries <- st_read(region_shp_path, quiet = TRUE) %>%
  rename(
    region_code = RGN25CD,
    region_name_boundary = RGN25NM
  ) %>%
  st_transform(27700) %>%
  st_make_valid() %>%
  simplify_boundaries()

saveRDS(lsip_boundaries,   lsip_light_path)
saveRDS(lad_boundaries,    lad_light_path)
saveRDS(region_boundaries, region_light_path)

# Check resulting file sizes (should be a few MB each)
tibble(
  file = c(lsip_light_path, lad_light_path, region_light_path),
  size_mb = round(file.size(c(lsip_light_path, lad_light_path, region_light_path)) / 1024^2, 1)
)

# Clean up
rm(lsip_boundaries, lad_boundaries, region_boundaries)
