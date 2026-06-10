################################################################################
## Apprenticeship Analysis
## 02_analysis.R
##
## Covers:
##   - Read boundary shapefiles (LSIP, LAD, Region)
##   - LSIP map joins (overall, under-19, intermediate, advanced starts rates)
##   - Regional map joins (engineering starts, engineering share)
##   - LAD map joins (engineering starts)
##   - LAD thin-market proxy joins (low-start + few-provider map objects)
##   - IS8 proxy sector mapping (industry analysis)
##   - Geographic concentration and spatial spread of technical starts
##   - Fragile-standard LAD map analysis
##   - LAD-to-LSIP lookup and LSIP thin-market aggregation
##   - Industrial Strategy cluster locations (for map overlays)
##
## Depends on:
##   - config.R         (sourced below — paths to shapefiles)
##   - 01_exploratory.R (run first to create output_folder/data/*.rds)
##
## Outputs:
##   RDS files written to output_folder/data/ for use in 03_visualisation.R
################################################################################

source("config.R")
library(tidyverse)
library(sf)

# Load analytical summaries from 01_exploratory.R
data_path <- file.path(output_folder, "data")

lsip_base                <- readRDS(file.path(data_path, "lsip_base.rds"))
lsip_age_base            <- readRDS(file.path(data_path, "lsip_age_base.rds"))
lsip_level_base          <- readRDS(file.path(data_path, "lsip_level_base.rds"))
regional_engineering     <- readRDS(file.path(data_path, "regional_engineering.rds"))
lad_engineering          <- readRDS(file.path(data_path, "lad_engineering.rds"))
thin_market_funding_join <- readRDS(file.path(data_path, "thin_market_funding_join.rds"))
provider_standard_lad_proxy <- readRDS(file.path(data_path, "provider_standard_lad_proxy.rds"))
thin_market_funding           <- readRDS(file.path(data_path, "thin_market_funding.rds"))
potentially_fragile_standards <- readRDS(file.path(data_path, "potentially_fragile_standards.rds"))
regional_priority_subjects    <- readRDS(file.path(data_path, "regional_priority_subjects.rds"))
lad_priority_subjects         <- readRDS(file.path(data_path, "lad_priority_subjects.rds"))

# Shared helper (needed for normalise_apps_level below)
normalise_apps_level <- function(apps_level) {
  dplyr::case_when(
    apps_level %in% c("Intermediate", "Intermediate Apprenticeship") ~ "Intermediate",
    apps_level %in% c("Advanced", "Advanced Apprenticeship") ~ "Advanced",
    apps_level %in% c("Higher", "Higher Apprenticeship") ~ "Higher",
    apps_level == "Total" ~ "Total",
    TRUE ~ apps_level
  )
}

######################
# Read boundary files
######################

# Simplified, standardised boundaries created once by 00_prepare_boundaries.R
# (columns already renamed, England-only LADs, CRS 27700, valid geometry).
# These are small enough for git and for Posit Cloud's memory limits.
lsip_boundaries   <- readRDS(lsip_light_path)
lad_boundaries    <- readRDS(lad_light_path)
region_boundaries <- readRDS(region_light_path)

# Inspect column names if joins fail
names(lsip_boundaries)
names(lad_boundaries)
names(region_boundaries)

######################
# LSIP maps
######################

# Overall starts rate by LSIP
lsip_map_base <- lsip_boundaries %>%
  left_join(
    lsip_base,
    by = "lsip_code"
  )

# Under-19 starts rate by LSIP
lsip_under19_map_base <- lsip_boundaries %>%
  left_join(
    lsip_age_base %>%
      filter(age_summary == "Under 19") %>%
      select(
        lsip_code,
        starts,
        population_estimate,
        starts_rate_per_100000_population
      ),
    by = "lsip_code"
  )

# Intermediate starts rate by LSIP
lsip_intermediate_map_base <- lsip_boundaries %>%
  left_join(
    lsip_level_base %>%
      mutate(apps_level_clean = normalise_apps_level(apps_level)) %>%
      filter(apps_level_clean == "Intermediate") %>%
      select(
        lsip_code,
        starts,
        population_estimate,
        starts_rate_per_100000_population
      ),
    by = "lsip_code"
  )

# Advanced starts rate by LSIP
lsip_advanced_map_base <- lsip_boundaries %>%
  left_join(
    lsip_level_base %>%
      mutate(apps_level_clean = normalise_apps_level(apps_level)) %>%
      filter(apps_level_clean == "Advanced") %>%
      select(
        lsip_code,
        starts,
        population_estimate,
        starts_rate_per_100000_population
      ),
    by = "lsip_code"
  )

# Clean up temporary objects (only the map bases are needed downstream)
rm(lsip_age_base, lsip_level_base)

######################
# Regional maps
######################

region_engineering_map_base <- region_boundaries %>%
  left_join(
    regional_engineering %>%
      select(
        region_code,
        starts,
        pct_region_starts,
        national_engineering_share
      ),
    by = "region_code"
  )

######################
# LAD maps
######################

lad_engineering_map_base <- lad_boundaries %>%
  left_join(
    lad_engineering %>%
      select(
        lad_code,
        starts,
        national_engineering_share
      ),
    by = "lad_code"
  )

######################
# LAD thin-market proxy: estimated delivery of low-start + few-provider standards
######################

# 1. Attach quadrant data to provider-standard-LAD proxy
provider_standard_lad_quadrant_proxy <- provider_standard_lad_proxy %>%
  mutate(
    standard_name_clean = standard_name %>%
      str_squish()
  ) %>%
  left_join(
    thin_market_funding_join,
    by = "standard_name_clean"
  )

# 2. Check match quality
provider_standard_lad_quadrant_match_check <- provider_standard_lad_quadrant_proxy %>%
  summarise(
    rows = n(),
    matched_rows = sum(!is.na(market_type)),
    unmatched_rows = sum(is.na(market_type)),
    match_rate = round(100 * matched_rows / rows, 1),
    estimated_starts_total = sum(estimated_lad_starts, na.rm = TRUE),
    estimated_starts_matched = sum(estimated_lad_starts[!is.na(market_type)], na.rm = TRUE)
  )

provider_standard_lad_quadrant_match_check

# 3. Aggregate estimated starts in the low-start + few-provider quadrant by delivery LAD
lad_low_starts_few_providers_proxy <- provider_standard_lad_quadrant_proxy %>%
  filter(
    market_type == "Low starts + few providers",
    !is.na(delivery_lad)
  ) %>%
  group_by(delivery_lad) %>%
  summarise(
    estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    standards = n_distinct(standard_name_clean),
    providers = n_distinct(provider_ukprn),
    .groups = "drop"
  ) %>%
  mutate(
    national_share = round(100 * estimated_starts / sum(estimated_starts, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(estimated_starts))

lad_low_starts_few_providers_proxy %>%
  slice_head(n = 30) %>%
  print(n = 30, width = Inf)

# Check LAD boundary name column if needed
names(lad_boundaries)

lad_low_starts_few_providers_map_base <- lad_boundaries %>%
  left_join(
    lad_low_starts_few_providers_proxy,
    by = c("lad_name_boundary" = "delivery_lad")
  )

# Check unmatched delivery LADs
lad_low_starts_few_providers_proxy %>%
  anti_join(
    lad_boundaries %>%
      st_drop_geometry() %>%
      select(lad_name_boundary),
    by = c("delivery_lad" = "lad_name_boundary")
  ) %>%
  arrange(desc(estimated_starts)) %>%
  print(n = 50, width = Inf)

# 4. Share of estimated delivery starts in low-start + few-provider standards

lad_all_quadrants_proxy <- provider_standard_lad_quadrant_proxy %>%
  filter(
    !is.na(market_type),
    !is.na(delivery_lad)
  ) %>%
  group_by(delivery_lad) %>%
  summarise(
    all_matched_estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    .groups = "drop"
  )

lad_low_starts_few_providers_share_proxy <- lad_low_starts_few_providers_proxy %>%
  left_join(
    lad_all_quadrants_proxy,
    by = "delivery_lad"
  ) %>%
  mutate(
    pct_matched_estimated_starts = round(
      100 * estimated_starts / all_matched_estimated_starts,
      1
    )
  )

lad_low_starts_few_providers_share_map_base <- lad_boundaries %>%
  left_join(
    lad_low_starts_few_providers_share_proxy,
    by = c("lad_name_boundary" = "delivery_lad")
  )

# Clean up temporary objects
rm(lad_all_quadrants_proxy)

######################
# Industry analysis: IS8 proxy sector mapping
######################

# This is a proxy mapping from apprenticeship standards to the Industrial
# Strategy 8 sectors. It is not an official classification.
#
# Rationale:
# - Apprenticeship standards are not directly coded to IS8 sectors.
# - The Data City report highlights that frontier sectors are difficult to
#   capture using traditional classifications, especially clean energy,
#   defence, digital technologies and advanced manufacturing.
# - We therefore use a transparent route + keyword approach to identify
#   standards plausibly relevant to IS8 sectors.

assign_is8_proxy <- function(standard_name, route) {

  standard_lower <- str_to_lower(standard_name)
  route_lower <- str_to_lower(route)

  case_when(
    # Digital and Technologies
    str_detect(
      standard_lower,
      "cyber|artificial intelligence|\\bai\\b|machine learning|software|data|digital|network|telecom|telecommunications|cloud|quantum|semiconductor|coding|programmer|systems engineer|ux|user experience"
    ) |
      str_detect(route_lower, "digital") ~ "Digital and Technologies",

    # Life Sciences
    str_detect(
      standard_lower,
      "clinical|biotech|bioinformatics|pharma|pharmaceutical|laboratory|lab scientist|healthcare science|medical|genomics|nursing|midwife|dietitian|podiatrist|radiographer|physiotherapist|orthotist|prosthetist|paramedic"
    ) |
      str_detect(route_lower, "health and science") ~ "Life Sciences",

    # Clean Energy Industries
    str_detect(
      standard_lower,
      "nuclear|wind|hydrogen|low carbon|heat pump|heating|carbon|energy|power|utilities|gas network|electricity|electrical power|smart meter|water process|water network|environmental|sustainability|retrofit"
    ) ~ "Clean Energy Industries",

    # Defence
    str_detect(
      standard_lower,
      "defence|defense|ordnance|munitions|explosives|aviation|aerospace|aircraft|air traffic|marine|maritime|naval|army|royal navy|royal air force|security|survival equipment"
    ) ~ "Defence",

    # Advanced Manufacturing
    str_detect(
      standard_lower,
      "manufacturing|manufacturer|engineering|robot|robotics|automotive|aerospace|materials|composites|battery|machining|machinist|welding|welder|fabrication|metal|foundry|casting|toolmaker|maintenance technician|rail engineering|mechatronics|process operative"
    ) |
      str_detect(route_lower, "engineering and manufacturing") ~ "Advanced Manufacturing",

    # Financial Services
    str_detect(
      standard_lower,
      "finance|financial|investment|insurance|actuary|actuarial|mortgage|banking|pensions|tax|audit|accounting|accountancy|risk"
    ) |
      str_detect(route_lower, "legal, finance and accounting") ~ "Financial Services",

    # Creative Industries
    str_detect(
      standard_lower,
      "creative|media|broadcast|game|gaming|animation|film|design|designer|advertising|visual|arts|curator|museum|gallery|archive"
    ) |
      str_detect(route_lower, "creative and design") ~ "Creative Industries",

    # Professional and Business Services
    str_detect(
      standard_lower,
      "consultant|consulting|business analyst|management|manager|project|legal|solicitor|paralegal|hr|human resources|procurement|marketing|sales|operations|leadership"
    ) |
      str_detect(route_lower, "business and administration") ~ "Professional and Business Services",

    TRUE ~ "Other / not clearly IS8"
  )
}

thin_market_is8 <- thin_market_funding %>%
  mutate(
    is8_proxy_sector = assign_is8_proxy(
      std_fwk_name_stcode,
      route
    ),
    is8_proxy_flag = is8_proxy_sector != "Other / not clearly IS8"
  )

# IS8 proxy mapping checks
thin_market_is8 %>%
  count(is8_proxy_sector, sort = TRUE) %>%
  print(n = Inf)

potentially_fragile_standards_is8 <- potentially_fragile_standards %>%
  mutate(
    is8_proxy_sector = assign_is8_proxy(
      std_fwk_name_stcode,
      route
    )
  )

potentially_fragile_standards_is8 %>%
  count(is8_proxy_sector, sort = TRUE) %>%
  print(n = Inf)

# Summary of the thin-market landscape by IS8 proxy sector
is8_proxy_summary <- thin_market_is8 %>%
  group_by(is8_proxy_sector) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    median_standard_starts = median(starts, na.rm = TRUE),
    median_providers = median(providers, na.rm = TRUE),
    median_funding = median(max_funding, na.rm = TRUE),
    low_starts_few_providers = sum(market_type == "Low starts + few providers"),
    pct_low_starts_few_providers = round(
      100 * low_starts_few_providers / standards,
      1
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(low_starts_few_providers))

is8_proxy_summary %>%
  print(n = Inf, width = Inf)

######################
# Geographic concentration and spatial spread of technical starts
######################

# Detailed geographic concentration helper.
# Extends the simple helper in 01_exploratory.R with HHI-based effective
# spread: the number of equally sized areas that would produce the same
# level of concentration.
geo_concentration_detailed <- function(df, geo_col, value_col) {
  df %>%
    filter(!is.na({{ geo_col }}), !is.na({{ value_col }})) %>%
    group_by({{ geo_col }}) %>%
    summarise(
      starts = sum({{ value_col }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(starts > 0) %>%
    arrange(desc(starts)) %>%
    mutate(
      share = starts / sum(starts),
      rank = row_number()
    ) %>%
    summarise(
      geographies = n(),
      total_starts = sum(starts),
      median_starts = median(starts),
      top_1_share = round(100 * sum(share[rank <= 1]), 1),
      top_5_share = round(100 * sum(share[rank <= 5]), 1),
      top_10_share = round(100 * sum(share[rank <= 10]), 1),
      hhi = round(sum(share^2), 4),
      effective_geographies = round(1 / sum(share^2), 1),
      top_1_vs_equal_share = round((sum(share[rank <= 1]) / (1 / n())), 1)
    )
}

# How many areas are needed to reach a given share of starts?
areas_to_reach_share <- function(df, geo_col, value_col, target_share = 0.5) {
  df %>%
    filter(!is.na({{ geo_col }}), !is.na({{ value_col }})) %>%
    group_by({{ geo_col }}) %>%
    summarise(
      starts = sum({{ value_col }}, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(starts > 0) %>%
    arrange(desc(starts)) %>%
    mutate(
      share = starts / sum(starts),
      cumulative_share = cumsum(share)
    ) %>%
    summarise(
      geographies = n(),
      areas_to_50_pct = min(row_number()[cumulative_share >= 0.5]),
      areas_to_75_pct = min(row_number()[cumulative_share >= 0.75]),
      share_of_areas_for_50_pct = round(100 * areas_to_50_pct / geographies, 1),
      share_of_areas_for_75_pct = round(100 * areas_to_75_pct / geographies, 1)
    )
}

# Areas needed to reach 50% of starts
lad_engineering_areas_to_share <- areas_to_reach_share(
  lad_engineering,
  lad_name,
  starts,
  0.5
)

regional_engineering_areas_to_share <- areas_to_reach_share(
  regional_engineering,
  region_name,
  starts,
  0.5
)

lad_priority_areas_to_share <- areas_to_reach_share(
  lad_priority_subjects,
  lad_name,
  priority_starts,
  0.5
)

regional_priority_areas_to_share <- areas_to_reach_share(
  regional_priority_subjects,
  region_name,
  priority_starts,
  0.5
)

lad_engineering_areas_to_share
regional_engineering_areas_to_share
lad_priority_areas_to_share
regional_priority_areas_to_share

# Concentration metrics
regional_engineering_concentration <- geo_concentration_detailed(
  regional_engineering,
  region_name,
  starts
)

lad_engineering_concentration <- geo_concentration_detailed(
  lad_engineering,
  lad_name,
  starts
)

regional_priority_concentration <- geo_concentration_detailed(
  regional_priority_subjects,
  region_name,
  priority_starts
)

lad_priority_concentration <- geo_concentration_detailed(
  lad_priority_subjects,
  lad_name,
  priority_starts
)

regional_engineering_concentration
lad_engineering_concentration
regional_priority_concentration
lad_priority_concentration

# Clean summary table from the concentration outputs
# (used for the gt table and the spatial spread chart in 03_visualisation.R)
spatial_spread_table <- tibble::tibble(
  apprenticeship_area = c(
    "Engineering and manufacturing",
    "Priority technical subjects"
  ),
  regions_with_starts = c(
    regional_engineering_concentration$geographies,
    regional_priority_concentration$geographies
  ),
  largest_region_share = c(
    regional_engineering_concentration$top_1_share,
    regional_priority_concentration$top_1_share
  ),
  effective_region_spread = c(
    regional_engineering_concentration$effective_geographies,
    regional_priority_concentration$effective_geographies
  ),
  lads_with_starts = c(
    lad_engineering_concentration$geographies,
    lad_priority_concentration$geographies
  ),
  largest_lad_share = c(
    lad_engineering_concentration$top_1_share,
    lad_priority_concentration$top_1_share
  ),
  effective_lad_spread = c(
    lad_engineering_concentration$effective_geographies,
    lad_priority_concentration$effective_geographies
  )
) %>%
  mutate(
    largest_region_share = paste0(largest_region_share, "%"),
    effective_region_spread = round(effective_region_spread, 0),
    largest_lad_share = paste0(largest_lad_share, "%"),
    effective_lad_spread = round(effective_lad_spread, 0)
  )

spatial_spread_table

# Clean up temporary objects
rm(
  lad_engineering_areas_to_share,
  regional_engineering_areas_to_share,
  lad_priority_areas_to_share,
  regional_priority_areas_to_share
)

######################
# Thin-market and fragile-standard LAD map analysis
######################

# The downloaded EES/Xplore standard-level files do not directly report how
# many starts on each standard occur in each LAD, so we cannot observe
# "this LAD has X actual starts in thin-market standards". Instead we
# estimate likely geography by allocating each provider's standard-level
# starts across the LADs where that provider delivers (the provider-LAD
# proxy built in 01_exploratory.R). The resulting maps show estimated
# delivery exposure to thin-market and fragile standards and should be
# interpreted as an indicative proxy, not observed standard-by-place starts.

# 1. Create standard-level fragile flags.
# Use the same name cleaning as thin_market_funding_join so the flags join
# onto the provider-standard-LAD proxy by standard_name_clean.
fragile_flags <- potentially_fragile_standards %>%
  mutate(
    standard_name_clean = std_fwk_name_stcode %>%
      str_remove("\\s*\\(ST[0-9]+\\)$") %>%
      str_remove("\\s*\\(FA[0-9]+\\)$") %>%
      str_squish()
  ) %>%
  distinct(standard_name_clean) %>%
  mutate(
    potentially_fragile = TRUE
  )

# 2. Join flags to the provider-standard-LAD quadrant proxy.
# provider_standard_lad_quadrant_proxy already carries market_type from
# thin_market_funding_join.
provider_standard_lad_thin <- provider_standard_lad_quadrant_proxy %>%
  left_join(
    fragile_flags,
    by = "standard_name_clean"
  ) %>%
  mutate(
    low_start_few_provider = replace_na(
      market_type == "Low starts + few providers",
      FALSE
    ),
    potentially_fragile = replace_na(potentially_fragile, FALSE)
  )

# 3. Summarise to delivery LAD
lad_thin_market_summary <- provider_standard_lad_thin %>%
  filter(!is.na(delivery_lad)) %>%
  group_by(delivery_lad) %>%
  summarise(
    total_estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    low_start_few_provider_estimated_starts = sum(
      estimated_lad_starts[low_start_few_provider],
      na.rm = TRUE
    ),
    fragile_estimated_starts = sum(
      estimated_lad_starts[potentially_fragile],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pct_low_start_few_provider = 100 * low_start_few_provider_estimated_starts / total_estimated_starts,
    pct_fragile = 100 * fragile_estimated_starts / total_estimated_starts
  )

lad_thin_market_summary %>%
  arrange(desc(fragile_estimated_starts)) %>%
  print(n = 30, width = Inf)

# 4. Join to LAD boundaries (delivery LADs are names, not codes)
lad_fragile_map_base <- lad_boundaries %>%
  left_join(
    lad_thin_market_summary,
    by = c("lad_name_boundary" = "delivery_lad")
  )

######################
# LAD to LSIP lookup
######################

# Build a lookup by assigning each LAD to the LSIP containing its centroid
# (st_point_on_surface guarantees the point falls inside the LAD polygon).
# Both boundary objects are already standardised and in CRS 27700 above.

lad_centroids <- lad_boundaries %>%
  st_make_valid() %>%
  st_point_on_surface()

lad_lsip_lookup <- lad_centroids %>%
  st_join(
    lsip_boundaries %>%
      st_make_valid(),
    join = st_within
  ) %>%
  st_drop_geometry() %>%
  transmute(
    lad_code,
    lad_name = lad_name_boundary,
    lsip_code,
    lsip_name = lsip_name_boundary
  ) %>%
  filter(!is.na(lsip_code)) %>%
  distinct()

# Check: LADs per LSIP
lad_lsip_lookup %>%
  count(lsip_name, sort = TRUE) %>%
  print(n = Inf)

write_csv(
  lad_lsip_lookup,
  file.path(output_folder, "lookup_lad_to_lsip.csv")
)

######################
# LSIP thin-market summary
######################

# Aggregate the LAD proxy up to LSIPs. The proxy carries delivery LAD names,
# so the lookup joins by LAD name.
lsip_thin_market_summary <- provider_standard_lad_thin %>%
  filter(!is.na(delivery_lad)) %>%
  left_join(
    lad_lsip_lookup,
    by = c("delivery_lad" = "lad_name")
  ) %>%
  filter(!is.na(lsip_code)) %>%
  group_by(lsip_code, lsip_name) %>%
  summarise(
    total_estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    low_start_few_provider_estimated_starts = sum(
      estimated_lad_starts[low_start_few_provider],
      na.rm = TRUE
    ),
    fragile_estimated_starts = sum(
      estimated_lad_starts[potentially_fragile],
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    pct_low_start_few_provider = 100 * low_start_few_provider_estimated_starts / total_estimated_starts,
    pct_fragile = 100 * fragile_estimated_starts / total_estimated_starts
  )

lsip_thin_market_summary %>%
  arrange(desc(pct_low_start_few_provider)) %>%
  print(n = Inf)

# Check: delivery LADs that fail to match the lookup (estimated starts lost
# in the LSIP aggregation)
provider_standard_lad_thin %>%
  filter(!is.na(delivery_lad)) %>%
  anti_join(
    lad_lsip_lookup,
    by = c("delivery_lad" = "lad_name")
  ) %>%
  group_by(delivery_lad) %>%
  summarise(
    estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(estimated_starts)) %>%
  print(n = 50, width = Inf)

write_csv(
  lsip_thin_market_summary,
  file.path(output_folder, "table_lsip_thin_market_summary.csv")
)

# Join to LSIP boundaries for mapping in 03_visualisation.R
lsip_thin_market_map_base <- lsip_boundaries %>%
  left_join(
    lsip_thin_market_summary,
    by = "lsip_code"
  )

# Clean up temporary objects
rm(fragile_flags, provider_standard_lad_thin, lad_centroids)

######################
# Industrial Strategy cluster locations (England)
######################

# Sub-regional clusters named in the 2025 UK Industrial Strategy
# ("Unleashing the potential of our cities and regions", p.106).
# Coordinates are approximate anchor points (major city or named asset) for
# overlaying on the LSIP/LAD maps. Scotland, Wales and Northern Ireland
# clusters are excluded because the maps are England-only.
# Edit this table to refine positions or add/remove clusters.

is_clusters <- tribble(
  ~is_region,                 ~cluster_name,                    ~lon,  ~lat,
  "South West",               "West of England",                -2.59, 51.45,
  "South West",               "Somerset",                       -3.00, 51.13,
  "South West",               "Plymouth",                       -4.14, 50.38,
  "South East",               "Oxford",                         -1.26, 51.75,
  "South East",               "Solent",                         -1.40, 50.91,
  "Greater London",           "London",                         -0.12, 51.51,
  "East of England",          "Cambridgeshire & Peterborough",   0.12, 52.21,
  "West Midlands",            "West Midlands",                  -1.90, 52.48,
  "East Midlands",            "East Midlands",                  -1.35, 52.95,
  "East Midlands",            "Greater Lincolnshire",           -0.54, 53.23,
  "North West",               "Greater Manchester",             -2.24, 53.48,
  "North West",               "Liverpool City Region",          -2.98, 53.41,
  "North West",               "Cheshire",                       -2.60, 53.20,
  "North West",               "Lancashire",                     -2.70, 53.76,
  "Yorkshire and the Humber", "South Yorkshire",                -1.47, 53.38,
  "Yorkshire and the Humber", "West Yorkshire",                 -1.55, 53.80,
  "Yorkshire and the Humber", "York & North Yorkshire",         -1.08, 53.96,
  "Yorkshire and the Humber", "Hull and East Yorkshire",        -0.34, 53.74,
  "North East",               "North East",                     -1.61, 54.97,
  "North East",               "Tees Valley",                    -1.23, 54.57
)

# Convert to sf points in the same CRS as the boundary objects (27700)
is_clusters_sf <- is_clusters %>%
  st_as_sf(
    coords = c("lon", "lat"),
    crs = 4326
  ) %>%
  st_transform(27700)

# Check: every cluster point should fall inside an LSIP
is_clusters_sf %>%
  st_join(
    lsip_boundaries %>%
      st_make_valid() %>%
      select(lsip_name_boundary),
    join = st_within
  ) %>%
  st_drop_geometry() %>%
  print(n = Inf)

# Clean up temporary objects
rm(is_clusters)

######################
# Save map objects for 03_visualisation.R
######################

saveRDS(lsip_map_base,                              file.path(data_path, "lsip_map_base.rds"))
saveRDS(lsip_under19_map_base,                      file.path(data_path, "lsip_under19_map_base.rds"))
saveRDS(lsip_intermediate_map_base,                 file.path(data_path, "lsip_intermediate_map_base.rds"))
saveRDS(lsip_advanced_map_base,                     file.path(data_path, "lsip_advanced_map_base.rds"))
saveRDS(region_engineering_map_base,                file.path(data_path, "region_engineering_map_base.rds"))
saveRDS(lad_engineering_map_base,                   file.path(data_path, "lad_engineering_map_base.rds"))
saveRDS(lad_low_starts_few_providers_map_base,      file.path(data_path, "lad_low_starts_few_providers_map_base.rds"))
saveRDS(lad_low_starts_few_providers_share_map_base, file.path(data_path, "lad_low_starts_few_providers_share_map_base.rds"))

# Industry (IS8), spatial spread and fragile-standard map objects
saveRDS(is8_proxy_summary,                  file.path(data_path, "is8_proxy_summary.rds"))
saveRDS(potentially_fragile_standards_is8,  file.path(data_path, "potentially_fragile_standards_is8.rds"))
saveRDS(spatial_spread_table,               file.path(data_path, "spatial_spread_table.rds"))
saveRDS(regional_engineering_concentration, file.path(data_path, "regional_engineering_concentration.rds"))
saveRDS(lad_engineering_concentration,      file.path(data_path, "lad_engineering_concentration.rds"))
saveRDS(regional_priority_concentration,    file.path(data_path, "regional_priority_concentration.rds"))
saveRDS(lad_priority_concentration,         file.path(data_path, "lad_priority_concentration.rds"))
saveRDS(lad_thin_market_summary,            file.path(data_path, "lad_thin_market_summary.rds"))
saveRDS(lad_fragile_map_base,               file.path(data_path, "lad_fragile_map_base.rds"))
saveRDS(lad_lsip_lookup,                    file.path(data_path, "lad_lsip_lookup.rds"))
saveRDS(lsip_thin_market_summary,           file.path(data_path, "lsip_thin_market_summary.rds"))
saveRDS(lsip_thin_market_map_base,          file.path(data_path, "lsip_thin_market_map_base.rds"))
saveRDS(is_clusters_sf,                     file.path(data_path, "is_clusters_sf.rds"))
