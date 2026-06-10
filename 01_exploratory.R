################################################################################
## Apprenticeship Analysis
## 01_exploratory.R
##
## Covers:
##   00 Packages
##   01 Source config + helper functions
##   02 Load data
##   03 Standards market: 2024/25 snapshot
##   04 Standards market: low-start and route analysis
##   05 Standards market: multi-year trends
##   06 Provider market: 2024/25 snapshot
##   07 Provider market: multi-year trends
##   08 Provider subject concentration and quadrant examples
##   09 Provider-standard thin-market analysis
##   10 Learner age and level analysis
##   11 Geography and spatial variation analysis
##   12 Historical starts analysis
##
## Depends on: config.R (sourced below)
## Outputs:    RDS files written to output_folder/data/
################################################################################

######################
# 00 Packages
######################

library(tidyverse)
library(sf)
library(scales)

source("config.R")

######################
# 01 Helper functions
######################

# Convert columns that should be numeric but are imported as character.
# Xplore data often stores numbers as text, sometimes with commas.
clean_numeric <- function(df) {

  num_patterns <- c(
    "starts",
    "achievements",
    "participation",
    "enrolments",
    "vacancies",
    "adverts",
    "leavers",
    "completers",
    "achievers",
    "commitments",
    "redundancies",
    "population",
    "rate",
    "percent"
  )

  cols_to_convert <- names(df)[
    map_lgl(names(df), ~ any(str_detect(.x, num_patterns)))
  ]

  df %>%
    mutate(
      across(
        all_of(cols_to_convert),
        ~ suppressWarnings(as.numeric(gsub(",", "", .x)))
      )
    )
}

# Create consistent starts bands for standards distribution analysis
create_starts_band <- function(starts) {
  factor(
    case_when(
      starts < 50 ~ "<50",
      starts < 100 ~ "50-99",
      starts < 250 ~ "100-249",
      starts < 500 ~ "250-499",
      starts < 1000 ~ "500-999",
      starts < 2500 ~ "1,000-2,499",
      starts < 5000 ~ "2,500-4,999",
      TRUE ~ "5,000+"
    ),
    levels = c(
      "<50",
      "50-99",
      "100-249",
      "250-499",
      "500-999",
      "1,000-2,499",
      "2,500-4,999",
      "5,000+"
    )
  )
}

# Create consistent estimated-starts bands for provider-standard-region and
# provider-standard-LAD concentration summaries.
# Replaces three identical inline case_when blocks.
create_estimated_starts_band <- function(x) {
  case_when(
    x < 50 ~ "<50 starts",
    x < 100 ~ "50-99 starts",
    x < 250 ~ "100-249 starts",
    x < 500 ~ "250-499 starts",
    x >= 500 ~ "500+ starts",
    TRUE ~ NA_character_
  )
}

# Academic year conversion
make_academic_year <- function(time_period) {
  paste0(
    substr(as.character(time_period), 1, 4),
    "/",
    substr(as.character(time_period), 5, 6)
  )
}

# Harmonise apprenticeship level labels across files
normalise_apps_level <- function(apps_level) {
  case_when(
    apps_level %in% c("Intermediate", "Intermediate Apprenticeship") ~ "Intermediate",
    apps_level %in% c("Advanced", "Advanced Apprenticeship") ~ "Advanced",
    apps_level %in% c("Higher", "Higher Apprenticeship") ~ "Higher",
    apps_level == "Total" ~ "Total",
    TRUE ~ apps_level
  )
}

# Geographic concentration helper.
# Higher top_5_share / HHI = more geographically concentrated.
geo_concentration <- function(data, geography_col, starts_col = starts) {

  data %>%
    filter(
      !is.na({{ geography_col }}),
      !is.na({{ starts_col }}),
      {{ starts_col }} > 0
    ) %>%
    mutate(
      total_starts = sum({{ starts_col }}, na.rm = TRUE),
      starts_share = {{ starts_col }} / total_starts
    ) %>%
    arrange(desc({{ starts_col }})) %>%
    mutate(rank = row_number()) %>%
    summarise(
      geographies = n_distinct({{ geography_col }}),
      total_starts = sum({{ starts_col }}, na.rm = TRUE),
      median_starts = median({{ starts_col }}, na.rm = TRUE),
      top_1_share = round(100 * sum(starts_share[rank <= 1], na.rm = TRUE), 1),
      top_5_share = round(100 * sum(starts_share[rank <= 5], na.rm = TRUE), 1),
      top_10_share = round(100 * sum(starts_share[rank <= 10], na.rm = TRUE), 1),
      hhi = round(sum(starts_share^2, na.rm = TRUE), 4),
      .groups = "drop"
    )
}

######################
# 02 Load data
######################

# Find all CSV files in the Xplore data folder.
# We do not read every file. The folder contains many overlapping historic
# releases, and the latest full-year files often already contain prior years.
csv_files <- list.files(
  path = data_folder,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

# Files actually used in this script.
required_files <- c(
  # Standards analysis
  "app-routes-standards-202425-q4.csv",
  "app-subject-standards-202425-q4.csv",
  "apps_17_subject_standards_202526_6.csv",

  # Provider analysis
  "app-provider-starts-202425-q4.csv",
  "apps_23_provider_starts_202526_6.csv",

  # Provider-standard footprint / NARTS
  "app-narts-provider-level-fwk-std.csv",

  # Learner age and level
  "app-learner-detailed-202425-q4.csv",

  # Geography
  "app-geography-population-202425-q4.csv",
  "app-geography-detailed-202425-q4.csv",

  # Historical starts
  "app-historical-summary-to-2425.csv"
)

# Create lookup from filename to full path
file_lookup <- tibble(
  file_path = csv_files,
  file_name = basename(csv_files)
)

# Check all required files are present
missing_files <- setdiff(required_files, file_lookup$file_name)

if (length(missing_files) > 0) {
  stop(
    paste(
      "Missing required files:",
      paste(missing_files, collapse = ", ")
    )
  )
}

# Keep only required files
analysis_files <- file_lookup %>%
  filter(file_name %in% required_files)

# Clean up temporary objects
rm(csv_files, missing_files, required_files)

# Helper to read one named file from the analysis folder
read_analysis_csv <- function(file_name) {

  file_path <- analysis_files %>%
    filter(.data$file_name == !!file_name) %>%
    pull(file_path)

  if (length(file_path) != 1) {
    stop(paste("Could not uniquely identify file:", file_name))
  }

  read_csv(file_path, show_col_types = FALSE) %>%
    clean_numeric()
}

######################
# Diagnostic: Skills England standards data
######################

# Update this path if needed (path is set in config.R)
skills_england_raw <- read_csv(
  skills_england_standards_path,
  skip = 1,
  show_col_types = FALSE
) %>%
  clean_numeric()

glimpse(skills_england_raw)

names(skills_england_raw)

skills_england_raw %>%
  summarise(
    rows = n(),
    standards = n_distinct(Reference),
    routes = n_distinct(Route),
    statuses = n_distinct(Status),
    total_with_funding = sum(!is.na(`Maximum Funding (£)`))
  )

skills_england_raw %>%
  count(Status, sort = TRUE) %>%
  print(n = Inf)

skills_england_raw %>%
  count(Route, sort = TRUE) %>%
  print(n = Inf)

skills_england_standards <- skills_england_raw %>%
  rename(
    programme_type = `Programme Type`,
    standard_name = Name,
    standard_ref = Reference,
    version_number = `Version Number`,
    proposal_approved = `Proposal Approved`,
    standard_approved = `Standard Approved`,
    assessment_plan_approved = `Assessment Plan Approved`,
    funding_approved = `Funding Approved`,
    status = Status,
    approved_for_delivery_date = `Approved for Delivery Date`,
    retired_date = `Retired date`,
    withdrawn_date = `Withdrawn date`,
    route = Route,
    level = Level,
    integrated_degree = `Integrated Degree`,
    integration = Integration,
    integrated_apprenticeship = `Integrated Apprenticeship`,
    max_funding = `Maximum Funding (£)`,
    typical_duration = `Typical Duration`,
    minimum_hours_for_compliance = `Minimum Hours For Compliance`,
    core_and_options = `Core and options`,
    regulated_standard = `Regulated Standard`,
    trailblazer_contact = `Trailblazer Contact`,
    lars_code = `LARS code for providers only`,
    learning_aim_class_code = `Learning Aim Class Code`,
    ssa2_code = `SSA2 code`,
    eqa_provider = `EQA Provider`,
    professional_recognition = `Professional recognition`,
    link = Link,
    last_updated = `Last Updated`,
    job_titles = `Job Titles`,
    overview_of_role = `Overview of role`,
    banners = Banners
  ) %>%
  mutate(
    standard_ref = str_trim(standard_ref),
    standard_name = str_trim(standard_name),
    route = str_trim(route),
    status = str_trim(status),
    level = as.numeric(level),
    max_funding = as.numeric(max_funding),
    typical_duration = as.numeric(typical_duration),
    minimum_hours_for_compliance = as.numeric(minimum_hours_for_compliance),
    is_active = status == "Approved for delivery",
    is_available_for_starts = str_detect(
      status,
      regex("Approved for delivery", ignore_case = TRUE)
    )
  )

skills_england_standards %>%
  summarise(
    rows = n(),
    standards = n_distinct(standard_ref),
    active_standards = sum(is_active, na.rm = TRUE),
    available_for_starts = sum(is_available_for_starts, na.rm = TRUE),
    median_funding = median(max_funding, na.rm = TRUE),
    p75_funding = quantile(max_funding, 0.75, na.rm = TRUE),
    max_funding = max(max_funding, na.rm = TRUE)
  )

skills_england_standards %>%
  count(programme_type, sort = TRUE) %>%
  print(n = Inf)

skills_england_standards %>%
  count(status, sort = TRUE) %>%
  print(n = Inf)

skills_england_standards %>%
  count(route, sort = TRUE) %>%
  print(n = Inf)

# Clean up temporary objects
rm(skills_england_raw)


######################
# 03 Standards market: 2024/25 snapshot
######################

# Main dataset for standards fragmentation analysis.
# This contains starts, achievements and enrolments by route and standard.
routes_std <- read_analysis_csv("app-routes-standards-202425-q4.csv")

# Create a cleaned standards-level base.
#
# Cleaning logic:
# 1. Use the latest available academic year in the file.
# 2. Keep only the "Total" demographic breakdown to avoid double counting.
# 3. Remove aggregate route and aggregate standard rows.
# 4. Remove "Total" level rows because each standard also appears under its actual level.
# 5. Remove rows with suppressed or missing starts.
# 6. Remove "No Route" rows because these appear to be legacy frameworks.
# 7. Deduplicate at route × level × standard level.
#
# This is the canonical dataset for the 2024/25 standards analysis.
standards_base <- routes_std %>%
  filter(
    time_period == max(time_period),
    breakdown_topic == "Total",
    breakdown == "Total",
    route != "Total",
    std_fwk_name_stcode != "Total",
    apps_level_detailed != "Total",
    !is.na(starts),
    route != "No Route"
  ) %>%
  distinct(
    route,
    apps_level_detailed,
    std_fwk_name_stcode,
    .keep_all = TRUE
  ) %>%
  mutate(
    starts_band = create_starts_band(starts)
  )

# Core sample size and distribution statistics
headline_metrics <- standards_base %>%
  summarise(
    standards = n(),
    unique_standards = n_distinct(std_fwk_name_stcode),
    total_starts = sum(starts),
    min_starts = min(starts),
    p10 = quantile(starts, 0.10),
    p25 = quantile(starts, 0.25),
    median_starts = median(starts),
    p75 = quantile(starts, 0.75),
    p90 = quantile(starts, 0.90),
    max_starts = max(starts)
  )

headline_metrics

# Number and share of standards in each starts band
standards_dist <- standards_base %>%
  count(starts_band, name = "standards") %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1)
  )

standards_dist

# Share of total starts accounted for by the largest standards
concentration <- standards_base %>%
  arrange(desc(starts)) %>%
  mutate(
    rank = row_number(),
    cumulative_starts = cumsum(starts),
    cumulative_share = cumulative_starts / sum(starts)
  ) %>%
  filter(rank %in% c(10, 25, 50, 100)) %>%
  transmute(
    top_n_standards = rank,
    cumulative_share = round(100 * cumulative_share, 1)
  )

concentration

# Clean up temporary objects (routes_std only feeds standards_base)
rm(routes_std)

######################
# 04 Standards market: low-start and route analysis
######################

# Define low-start standards as those with fewer than 50 annual starts.
# This threshold is simple and transparent, but should be treated as analytical,
# not official.
low_start_standards <- standards_base %>%
  filter(starts < 50)

# Low-start standards by route
low_start_by_route <- low_start_standards %>%
  count(route, name = "low_start_standards", sort = TRUE) %>%
  mutate(
    pct_low_start = round(100 * low_start_standards / sum(low_start_standards), 1)
  )

low_start_by_route

# Low-start standards by route and level
low_start_by_route_level <- low_start_standards %>%
  count(route, apps_level_detailed, name = "low_start_standards", sort = TRUE)

low_start_by_route_level

# Route-level summary of all observable standards
route_summary <- standards_base %>%
  group_by(route) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts),
    median_starts = median(starts),
    mean_starts = mean(starts),
    low_start_standards = sum(starts < 50),
    pct_low_start = round(100 * low_start_standards / standards, 1),
    .groups = "drop"
  ) %>%
  arrange(median_starts)

route_summary

# Compare each route's share of all standards with its share of low-start standards.
# Overrepresentation > 1 means the route is more common in the low-start tail than
# in the overall observable standards market.
route_compare <- standards_base %>%
  count(route, name = "all_standards") %>%
  mutate(
    pct_all = 100 * all_standards / sum(all_standards)
  ) %>%
  left_join(
    low_start_standards %>%
      count(route, name = "low_start_standards"),
    by = "route"
  ) %>%
  mutate(
    low_start_standards = replace_na(low_start_standards, 0),
    pct_low = 100 * low_start_standards / sum(low_start_standards),
    overrepresentation = pct_low / pct_all
  ) %>%
  arrange(desc(overrepresentation))

route_compare

# Clean up temporary objects
rm(low_start_standards)


######################
# 05 Standards market: multi-year trends
######################

# Use the latest full-year release as the canonical source for historic trends.
# This avoids double-counting the same academic years across multiple releases.
subject_standards_full_year_file <- analysis_files %>%
  filter(file_name == "app-subject-standards-202425-q4.csv") %>%
  pull(file_path)

# Use the 2025/26 file separately because it is an in-year release, not a full-year Q4 file.
subject_standards_in_year_file <- analysis_files %>%
  filter(file_name == "apps_17_subject_standards_202526_6.csv") %>%
  pull(file_path)

clean_subject_standards <- function(file_path, release_type) {

  df <- read_csv(file_path, show_col_types = FALSE) %>%
    clean_numeric()

  # Harmonise older/newer column names if needed
  if ("characteristic_group" %in% names(df)) {
    df <- df %>%
      rename(
        breakdown_topic = characteristic_group,
        breakdown = learner_characteristic
      )
  }

  if ("ssa_t1_desc" %in% names(df)) {
    df <- df %>%
      rename(
        ssa_tier_1 = ssa_t1_desc
      )
  }

  df %>%
    filter(
      breakdown_topic == "Total",
      breakdown == "Total",
      ssa_tier_1 != "Total",
      std_fwk_name_stcode != "Total",
      apps_level_detailed != "Total",
      !is.na(starts)
    ) %>%
    distinct(
      time_period,
      ssa_tier_1,
      apps_level_detailed,
      std_fwk_name_stcode,
      .keep_all = TRUE
    ) %>%
    mutate(
      release_type = release_type,
      source_file = basename(file_path),
      starts_band = create_starts_band(starts)
    )
}

standards_subject_full_year <- clean_subject_standards(
  subject_standards_full_year_file,
  release_type = "Full year"
)

standards_subject_in_year <- clean_subject_standards(
  subject_standards_in_year_file,
  release_type = "In-year"
)

# Combine, while keeping release_type so 2025/26 is never mistaken for a full-year estimate
standards_subject_base <- bind_rows(
  standards_subject_full_year,
  standards_subject_in_year %>%
    filter(time_period == 202526)
)

standards_subject_base %>%
  count(time_period, release_type, source_file) %>%
  arrange(time_period)

# Headline standards fragmentation metrics by academic year.
# 2025/26 is included but marked as in-year, so it should not be compared
# directly with full-year releases.
standards_subject_trends <- standards_subject_base %>%
  group_by(time_period, release_type) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts),
    median_starts = median(starts),
    p25 = quantile(starts, 0.25),
    p10 = quantile(starts, 0.10),
    low_start_standards = sum(starts < 50),
    pct_low_start = round(100 * low_start_standards / standards, 1),
    .groups = "drop"
  ) %>%
  arrange(time_period)

standards_subject_trends

# Concentration of starts in the largest standards.
standards_concentration_trends <- standards_subject_base %>%
  group_by(time_period, release_type) %>%
  arrange(desc(starts), .by_group = TRUE) %>%
  mutate(
    rank = row_number(),
    cumulative_share = cumsum(starts) / sum(starts)
  ) %>%
  filter(rank %in% c(10, 25, 50, 100)) %>%
  transmute(
    time_period,
    release_type,
    top_n_standards = rank,
    cumulative_share = round(100 * cumulative_share, 1)
  ) %>%
  ungroup()

standards_concentration_trends

# Subject composition of low-start standards over time.
# This uses SSA rather than route because SSA is available consistently across years.
low_start_subject_trends <- standards_subject_base %>%
  filter(
    release_type == "Full year",
    starts < 50
  ) %>%
  group_by(time_period, ssa_tier_1) %>%
  summarise(
    low_start_standards = n(),
    .groups = "drop"
  ) %>%
  group_by(time_period) %>%
  mutate(
    pct_low_start = round(100 * low_start_standards / sum(low_start_standards), 1)
  ) %>%
  ungroup() %>%
  arrange(time_period, desc(low_start_standards))

# Keep the main subject areas used in the time-series chart
low_start_subject_share_trends <- low_start_subject_trends %>%
  filter(
    ssa_tier_1 %in% c(
      "Engineering and Manufacturing Technologies",
      "Construction, Planning and the Built Environment",
      "Health, Public Services and Care",
      "Arts, Media and Publishing",
      "Agriculture, Horticulture and Animal Care"
    )
  ) %>%
  mutate(
    academic_year = make_academic_year(time_period)
  )

low_start_subject_share_trends

# Clean up temporary objects
rm(
  subject_standards_full_year_file,
  subject_standards_in_year_file,
  standards_subject_full_year,
  standards_subject_in_year,
  low_start_subject_trends
)


######################
# 06 Provider market: 2024/25 snapshot
######################

# Provider starts data gives provider-level apprenticeship starts by level and subject.
# For overall provider market concentration, keep total level and total subject rows.

provider_starts <- read_analysis_csv("app-provider-starts-202425-q4.csv")

providers_base <- provider_starts %>%
  filter(
    time_period == max(time_period),
    apps_level == "Total",
    ssa_tier_1 == "Total",
    !is.na(starts)
  ) %>%
  distinct(
    provider_ukprn,
    provider_name,
    .keep_all = TRUE
  ) %>%
  mutate(
    starts_band = create_starts_band(starts)
  )

# Provider market headline metrics
provider_headline_metrics <- providers_base %>%
  summarise(
    providers = n(),
    total_starts = sum(starts),
    median_starts = median(starts),
    p25 = quantile(starts, 0.25),
    p10 = quantile(starts, 0.10),
    max_starts = max(starts)
  )

provider_headline_metrics

# Provider concentration
provider_concentration <- providers_base %>%
  arrange(desc(starts)) %>%
  mutate(
    rank = row_number(),
    cumulative_starts = cumsum(starts),
    cumulative_share = cumulative_starts / sum(starts)
  ) %>%
  filter(rank %in% c(10, 25, 50, 100)) %>%
  transmute(
    top_n_providers = rank,
    cumulative_share = round(100 * cumulative_share, 1)
  )

provider_concentration

# Provider size distribution
providers_dist <- providers_base %>%
  count(starts_band, name = "providers") %>%
  mutate(
    pct_providers = round(100 * providers / sum(providers), 1)
  )

providers_dist

# NARTS provider-standard data gives us provider × standard outcomes.
# It does not measure current starts or approved provider supply.
# We use it as a lagged provider-footprint proxy: providers with leavers
# on each standard in 2023/24.

narts_provider_std <- read_analysis_csv("app-narts-provider-level-fwk-std.csv")

provider_standard_base <- narts_provider_std %>%
  filter(
    time_period == 202324,
    age_youth_adult == "Total",
    age_group == "Total",
    level == "Total",
    std_fwk_name_stcode != "Total",
    !is.na(leavers)
  ) %>%
  distinct(
    provider_ukprn,
    provider_name,
    std_fwk_name_stcode,
    .keep_all = TRUE
  )

# Count providers with leavers for each standard
providers_per_standard <- provider_standard_base %>%
  group_by(std_fwk_name_stcode, ssa_tier_1) %>%
  summarise(
    providers = n_distinct(provider_ukprn),
    leavers = sum(leavers),
    achievers = sum(achievers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    standard_ref = str_extract(std_fwk_name_stcode, "ST[0-9]+|FA[0-9]+")
  )

# 2023/24 starts by standard, from the standards subject dataset
standards_202324_keyed <- standards_subject_base %>%
  filter(
    time_period == 202324,
    release_type == "Full year"
  ) %>%
  select(
    std_fwk_name_stcode,
    ssa_tier_1,
    starts,
    apps_level_detailed
  ) %>%
  mutate(
    standard_ref = str_extract(std_fwk_name_stcode, "ST[0-9]+|FA[0-9]+")
  )

# Merge starts and provider-footprint data.
# Missing standard_ref values are removed to avoid many-to-many joins on NA.
thin_market_base <- standards_202324_keyed %>%
  filter(!is.na(standard_ref)) %>%
  left_join(
    providers_per_standard %>%
      filter(!is.na(standard_ref)) %>%
      select(standard_ref, providers, leavers, achievers),
    by = "standard_ref"
  )

# Check match quality
thin_market_match_quality <- thin_market_base %>%
  summarise(
    standards = n(),
    matched_provider_data = sum(!is.na(providers)),
    unmatched_provider_data = sum(is.na(providers)),
    match_rate = round(100 * matched_provider_data / standards, 1)
  )

thin_market_match_quality

# Classify standards using median starts and median providers as simple thresholds.
# This creates a transparent 2x2 segmentation:
# high/low starts × many/few providers.
starts_threshold <- thin_market_base %>%
  filter(!is.na(providers)) %>%
  summarise(value = median(starts, na.rm = TRUE)) %>%
  pull(value)

providers_threshold <- thin_market_base %>%
  filter(!is.na(providers)) %>%
  summarise(value = median(providers, na.rm = TRUE)) %>%
  pull(value)

thin_market_summary <- thin_market_base %>%
  filter(!is.na(providers)) %>%
  mutate(
    starts_group = if_else(
      starts < starts_threshold,
      "Low starts",
      "High starts"
    ),
    provider_group = if_else(
      providers < providers_threshold,
      "Few providers",
      "Many providers"
    ),
    market_type = case_when(
      starts_group == "High starts" & provider_group == "Many providers" ~ "High starts + many providers",
      starts_group == "High starts" & provider_group == "Few providers" ~ "High starts + few providers",
      starts_group == "Low starts" & provider_group == "Many providers" ~ "Low starts + many providers",
      starts_group == "Low starts" & provider_group == "Few providers" ~ "Low starts + few providers"
    )
  )

thin_market_segments <- thin_market_summary %>%
  count(market_type, name = "standards") %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1)
  )

thin_market_segments

# Subject composition of the thin-market quadrant
thin_market_by_subject <- thin_market_summary %>%
  filter(market_type == "Low starts + few providers") %>%
  count(ssa_tier_1, name = "standards", sort = TRUE) %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1)
  )

thin_market_by_subject

# Clean up temporary objects
rm(
  narts_provider_std,
  provider_standard_base,
  standards_202324_keyed,
  thin_market_base
)


######################
# 07 Learner age and level analysis
######################

# This section tests whether apprenticeship starts have shifted towards
# older learners and higher-level apprenticeships.
#
# The learner detailed file is heavily stacked by demographic breakdowns.
# To avoid double counting, we keep only national rows where ethnicity,
# sex and LLDD are all set to Total.

learner_detailed <- read_analysis_csv("app-learner-detailed-202425-q4.csv")

learner_age_level_base <- learner_detailed %>%
  filter(
    geographic_level == "National",
    age_summary %in% c("Under 19", "19-24", "25+"),
    apps_level != "Total",
    minority_ethnic == "Total",
    ethnicity_major == "Total",
    ethnicity_minor == "Total",
    sex == "Total",
    lldd == "Total",
    !is.na(starts)
  ) %>%
  select(
    time_period,
    age_summary,
    apps_level,
    starts,
    achievements,
    participation
  ) %>%
  mutate(
    academic_year = make_academic_year(time_period)
  )

# Check annual totals
learner_age_level_check <- learner_age_level_base %>%
  group_by(time_period, academic_year) %>%
  summarise(
    starts = sum(starts),
    rows = n(),
    .groups = "drop"
  )

learner_age_level_check


# Starts by age group
age_trends <- learner_age_level_base %>%
  group_by(time_period, academic_year, age_summary) %>%
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>%
  group_by(time_period, academic_year) %>%
  mutate(
    pct_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  ungroup()

age_trends


# Starts by apprenticeship level
level_trends <- learner_age_level_base %>%
  group_by(time_period, academic_year, apps_level) %>%
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>%
  group_by(time_period, academic_year) %>%
  mutate(
    pct_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  ungroup()

level_trends


# Starts by age group and level
age_level_trends <- learner_age_level_base %>%
  group_by(time_period, academic_year, age_summary, apps_level) %>%
  summarise(
    starts = sum(starts),
    .groups = "drop"
  ) %>%
  group_by(time_period, academic_year) %>%
  mutate(
    pct_total_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  ungroup()

age_level_trends


# Before/after summary for 2017/18 and 2024/25
age_level_summary <- age_level_trends %>%
  filter(
    academic_year %in% c("2017/18", "2024/25")
  ) %>%
  select(
    academic_year,
    age_summary,
    apps_level,
    starts,
    pct_total_starts
  ) %>%
  arrange(age_summary, apps_level, academic_year)

age_level_summary


# Focused trend: 25+ Higher apprenticeships as a share of all starts
higher_25_trend <- age_level_trends %>%
  filter(
    age_summary == "25+",
    apps_level == "Higher Apprenticeship"
  ) %>%
  select(
    academic_year,
    starts,
    pct_total_starts
  )

higher_25_trend

# Clean up temporary objects
rm(learner_detailed, learner_age_level_base, age_level_trends)


######################
# 08 Geography and spatial variation analysis
######################

# This section tests whether apprenticeship starts vary materially across places.
# The main geography used here is LSIP area because it is directly relevant to
# local employer coordination and skills planning.
#
# The geography-population file includes starts rates per 100,000 population.
# We use 2024/25 only for the spatial snapshot.

geo_pop <- read_analysis_csv("app-geography-population-202425-q4.csv")


# Overall LSIP starts rates
lsip_base <- geo_pop %>%
  filter(
    time_period == 202425,
    geographic_level == "Local skills improvement plan area",
    apps_level == "Total",
    age_summary == "Total",
    !is.na(starts_rate_per_100000_population)
  ) %>%
  select(
    lsip_code,
    lsip_name,
    starts,
    participation,
    achievements,
    population_estimate,
    starts_rate_per_100000_population,
    participation_rate_per_100000_population,
    achievements_rate_per_100000_population
  ) %>%
  distinct(lsip_code, lsip_name, .keep_all = TRUE)

lsip_summary <- lsip_base %>%
  summarise(
    lsips = n(),
    total_starts = sum(starts),
    median_starts_rate = median(starts_rate_per_100000_population),
    min_starts_rate = min(starts_rate_per_100000_population),
    max_starts_rate = max(starts_rate_per_100000_population),
    max_min_ratio = round(max_starts_rate / min_starts_rate, 2)
  )

lsip_summary

# LSIP starts rates by apprenticeship level
lsip_level_base <- geo_pop %>%
  filter(
    time_period == 202425,
    geographic_level == "Local skills improvement plan area",
    apps_level != "Total",
    age_summary == "Total",
    !is.na(starts_rate_per_100000_population)
  ) %>%
  select(
    lsip_code,
    lsip_name,
    apps_level,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  ) %>%
  distinct(lsip_code, lsip_name, apps_level, .keep_all = TRUE)

lsip_level_variation <- lsip_level_base %>%
  group_by(apps_level) %>%
  summarise(
    lsips = n(),
    median_rate = median(starts_rate_per_100000_population),
    min_rate = min(starts_rate_per_100000_population),
    max_rate = max(starts_rate_per_100000_population),
    max_min_ratio = round(max_rate / min_rate, 2),
    .groups = "drop"
  )

lsip_level_variation


# LSIP starts rates by age group
lsip_age_base <- geo_pop %>%
  filter(
    time_period == 202425,
    geographic_level == "Local skills improvement plan area",
    apps_level == "Total",
    age_summary %in% c("Under 19", "19-24", "25+"),
    !is.na(starts_rate_per_100000_population)
  ) %>%
  select(
    lsip_code,
    lsip_name,
    age_summary,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  ) %>%
  distinct(lsip_code, lsip_name, age_summary, .keep_all = TRUE)

lsip_age_variation <- lsip_age_base %>%
  group_by(age_summary) %>%
  summarise(
    lsips = n(),
    median_rate = median(starts_rate_per_100000_population),
    min_rate = min(starts_rate_per_100000_population),
    max_rate = max(starts_rate_per_100000_population),
    max_min_ratio = round(max_rate / min_rate, 2),
    .groups = "drop"
  )

lsip_age_variation

# LSIP starts rates by age group and apprenticeship level
# This is the strongest spatial test because it identifies where local variation
# is sharpest: younger and lower-level apprenticeship routes.
lsip_age_level_base <- geo_pop %>%
  filter(
    time_period == 202425,
    geographic_level == "Local skills improvement plan area",
    apps_level != "Total",
    age_summary %in% c("Under 19", "19-24", "25+"),
    !is.na(starts_rate_per_100000_population)
  ) %>%
  select(
    lsip_code,
    lsip_name,
    apps_level,
    age_summary,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  ) %>%
  distinct(
    lsip_code,
    lsip_name,
    apps_level,
    age_summary,
    .keep_all = TRUE
  )

lsip_age_level_variation <- lsip_age_level_base %>%
  group_by(age_summary, apps_level) %>%
  summarise(
    lsips = n(),
    median_rate = median(starts_rate_per_100000_population),
    min_rate = min(starts_rate_per_100000_population),
    max_rate = max(starts_rate_per_100000_population),
    max_min_ratio = round(max_rate / min_rate, 2),
    .groups = "drop"
  ) %>%
  mutate(
    age_level = paste(age_summary, apps_level, sep = " - "),
    age_level = fct_reorder(age_level, max_min_ratio)
  ) %>%
  arrange(desc(max_min_ratio))

lsip_age_level_variation

# This section strengthens the spatial analysis by asking whether apprenticeship
# activity is geographically concentrated or dispersed.
#
# Important limitations:
# - geo_pop is good for LSIP analysis by age and broad apprenticeship level.
# - geo_detailed is good for regional/LAD subject analysis.
# - The current files do not appear to provide standard × geography starts,
#   so we cannot directly map thin-market standards by place yet.
# - Level 3-5 cannot be identified exactly from geo_detailed because it only
#   has Intermediate / Advanced / Higher. Advanced is a Level 3 proxy; Higher
#   includes Level 4+ rather than only Levels 4-5.

geo_detailed <- read_analysis_csv("app-geography-detailed-202425-q4.csv")

# Check whether any standard-by-geography files exist in the folder.
# If this returns a relevant file, we can later do proper thin-standard geography.
possible_geo_standard_files <- file_lookup %>%
  filter(
    str_detect(
      file_name,
      regex("standard|std|fwk", ignore_case = TRUE)
    ),
    str_detect(
      file_name,
      regex("geo|local|region|lad|lsip", ignore_case = TRUE)
    )
  ) %>%
  arrange(file_name)

possible_geo_standard_files

# LSIP age-level base
lsip_age_level_extended <- geo_pop %>%
  filter(
    time_period == 202425,
    geographic_level == "Local skills improvement plan area",
    apps_level != "Total",
    age_summary %in% c("Under 19", "19-24", "25+"),
    !is.na(starts)
  ) %>%
  mutate(
    apps_level_clean = normalise_apps_level(apps_level)
  ) %>%
  select(
    lsip_code,
    lsip_name,
    apps_level_clean,
    age_summary,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  ) %>%
  distinct(
    lsip_code,
    lsip_name,
    apps_level_clean,
    age_summary,
    .keep_all = TRUE
  )

# Concentration metrics by age × level
lsip_age_level_concentration <- lsip_age_level_extended %>%
  group_by(age_summary, apps_level_clean) %>%
  group_modify(
    ~ geo_concentration(.x, lsip_name, starts)
  ) %>%
  ungroup() %>%
  mutate(
    segment = paste(age_summary, apps_level_clean, sep = " - ")
  ) %>%
  arrange(desc(top_5_share), desc(hhi))

lsip_age_level_concentration %>%
  print(n = Inf)

# Highest / lowest LSIPs for key entry-route segments
ranked_lsip_under19_intermediate <- lsip_age_level_extended %>%
  filter(
    age_summary == "Under 19",
    apps_level_clean == "Intermediate"
  ) %>%
  arrange(desc(starts_rate_per_100000_population)) %>%
  select(
    lsip_name,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  )

ranked_lsip_under19_advanced <- lsip_age_level_extended %>%
  filter(
    age_summary == "Under 19",
    apps_level_clean == "Advanced"
  ) %>%
  arrange(desc(starts_rate_per_100000_population)) %>%
  select(
    lsip_name,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  )

ranked_lsip_19_24_intermediate <- lsip_age_level_extended %>%
  filter(
    age_summary == "19-24",
    apps_level_clean == "Intermediate"
  ) %>%
  arrange(desc(starts_rate_per_100000_population)) %>%
  select(
    lsip_name,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  )

ranked_lsip_under19_intermediate %>% print(n = Inf)
ranked_lsip_under19_advanced %>% print(n = Inf)
ranked_lsip_19_24_intermediate %>% print(n = Inf)

# Compact top/bottom table for report or annex
lsip_key_segment_top_bottom <- bind_rows(
  ranked_lsip_under19_intermediate %>%
    slice_head(n = 5) %>%
    mutate(segment = "Under 19 - Intermediate", rank_group = "Highest"),
  ranked_lsip_under19_intermediate %>%
    slice_tail(n = 5) %>%
    mutate(segment = "Under 19 - Intermediate", rank_group = "Lowest"),
  ranked_lsip_under19_advanced %>%
    slice_head(n = 5) %>%
    mutate(segment = "Under 19 - Advanced", rank_group = "Highest"),
  ranked_lsip_under19_advanced %>%
    slice_tail(n = 5) %>%
    mutate(segment = "Under 19 - Advanced", rank_group = "Lowest")
) %>%
  select(
    segment,
    rank_group,
    lsip_name,
    starts,
    population_estimate,
    starts_rate_per_100000_population
  )

lsip_key_segment_top_bottom

# Clean up temporary objects
# (lsip_age_base and lsip_level_base are kept: they are saved for 02_analysis.R)
rm(
  geo_pop,
  lsip_age_level_base,
  lsip_age_level_extended,
  ranked_lsip_under19_intermediate,
  ranked_lsip_under19_advanced,
  ranked_lsip_19_24_intermediate
)

# Regional subject base
regional_subject_base <- geo_detailed %>%
  filter(
    time_period == 202425,
    geographic_level == "Regional",
    !is.na(region_name),
    region_name != "Outside of England and unknown",
    sex == "Total",
    ethnicity_major == "Total",
    apps_level == "Total",
    ssa_tier_1 != "Total",
    !is.na(starts)
  ) %>%
  select(
    region_code,
    region_name,
    ssa_tier_1,
    starts
  ) %>%
  distinct(
    region_code,
    region_name,
    ssa_tier_1,
    .keep_all = TRUE
  )

regional_total_starts <- regional_subject_base %>%
  group_by(region_code, region_name) %>%
  summarise(
    total_region_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  )

regional_engineering <- regional_subject_base %>%
  filter(ssa_tier_1 == "Engineering and Manufacturing Technologies") %>%
  left_join(
    regional_total_starts,
    by = c("region_code", "region_name")
  ) %>%
  mutate(
    pct_region_starts = round(100 * starts / total_region_starts, 1),
    national_engineering_share = round(100 * starts / sum(starts, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(starts))

regional_engineering %>%
  print(n = Inf)

regional_engineering_concentration <- geo_concentration(
  regional_engineering,
  region_name,
  starts
)

regional_engineering_concentration

# LAD subject base
lad_subject_base <- geo_detailed %>%
  filter(
    time_period == 202425,
    geographic_level == "Local authority district",
    !is.na(lad_code),
    !is.na(lad_name),
    sex == "Total",
    ethnicity_major == "Total",
    apps_level == "Total",
    ssa_tier_1 != "Total",
    !is.na(starts)
  ) %>%
  select(
    lad_code,
    lad_name,
    region_name,
    ssa_tier_1,
    starts
  ) %>%
  distinct(
    lad_code,
    lad_name,
    ssa_tier_1,
    .keep_all = TRUE
  )

lad_engineering <- lad_subject_base %>%
  filter(ssa_tier_1 == "Engineering and Manufacturing Technologies") %>%
  arrange(desc(starts)) %>%
  mutate(
    national_engineering_share = round(100 * starts / sum(starts, na.rm = TRUE), 1)
  )

lad_engineering_concentration <- geo_concentration(
  lad_engineering,
  lad_name,
  starts
)

lad_engineering_top_30 <- lad_engineering %>%
  slice_head(n = 30)

lad_engineering_concentration
lad_engineering_top_30

priority_subjects <- c(
  "Engineering and Manufacturing Technologies",
  "Construction, Planning and the Built Environment",
  "Digital Technology",
  "Science and Mathematics",
  "Health, Public Services and Care"
)

regional_priority_subjects <- regional_subject_base %>%
  filter(ssa_tier_1 %in% priority_subjects) %>%
  group_by(region_code, region_name) %>%
  summarise(
    priority_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    regional_total_starts,
    by = c("region_code", "region_name")
  ) %>%
  mutate(
    pct_region_starts = round(100 * priority_starts / total_region_starts, 1),
    national_priority_share = round(100 * priority_starts / sum(priority_starts, na.rm = TRUE), 1)
  ) %>%
  arrange(desc(priority_starts))

regional_priority_subjects %>%
  print(n = Inf)

regional_priority_concentration <- geo_concentration(
  regional_priority_subjects,
  region_name,
  priority_starts
)

regional_priority_concentration


lad_priority_subjects <- lad_subject_base %>%
  filter(ssa_tier_1 %in% priority_subjects) %>%
  group_by(lad_code, lad_name, region_name) %>%
  summarise(
    priority_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(priority_starts)) %>%
  mutate(
    national_priority_share = round(100 * priority_starts / sum(priority_starts, na.rm = TRUE), 1)
  )

lad_priority_concentration <- geo_concentration(
  lad_priority_subjects,
  lad_name,
  priority_starts
)

lad_priority_top_30 <- lad_priority_subjects %>%
  slice_head(n = 30)

lad_priority_concentration
lad_priority_top_30

# Regional level base
regional_level_base <- geo_detailed %>%
  filter(
    time_period == 202425,
    geographic_level == "Regional",
    !is.na(region_name),
    region_name != "Outside of England and unknown",
    sex == "Total",
    ethnicity_major == "Total",
    ssa_tier_1 == "Total",
    apps_level != "Total",
    !is.na(starts)
  ) %>%
  mutate(
    apps_level_clean = normalise_apps_level(apps_level)
  ) %>%
  select(
    region_code,
    region_name,
    apps_level_clean,
    starts
  ) %>%
  distinct(
    region_code,
    region_name,
    apps_level_clean,
    .keep_all = TRUE
  )

regional_level_concentration <- regional_level_base %>%
  group_by(apps_level_clean) %>%
  group_modify(
    ~ geo_concentration(.x, region_name, starts)
  ) %>%
  ungroup() %>%
  arrange(desc(top_5_share), desc(hhi))

regional_level_concentration %>%
  print(n = Inf)

regional_advanced_higher <- regional_level_base %>%
  filter(apps_level_clean %in% c("Advanced", "Higher")) %>%
  group_by(region_code, region_name) %>%
  summarise(
    advanced_higher_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(advanced_higher_starts)) %>%
  mutate(
    national_share = round(100 * advanced_higher_starts / sum(advanced_higher_starts, na.rm = TRUE), 1)
  )

regional_advanced_higher_concentration <- geo_concentration(
  regional_advanced_higher,
  region_name,
  advanced_higher_starts
)

regional_advanced_higher
regional_advanced_higher_concentration

# Clean up temporary objects
rm(
  geo_detailed,
  regional_subject_base,
  regional_total_starts,
  lad_subject_base,
  regional_level_base,
  regional_advanced_higher
)

######################
# Provider type and regional delivery data
######################

# This file was downloaded separately from the main Xplore/EES folder.
# It provides provider type, apprenticeship level, age group,
# delivery region, learner home region and starts/enrolments/achievements.
# (path is set in config.R as provider_region_breakdowns_path)

provider_region_raw <- read_csv(
  provider_region_breakdowns_path,
  show_col_types = FALSE
) %>%
  clean_numeric()

provider_region <- provider_region_raw %>%
  rename(
    apps_level = apps_Level
  ) %>%
  mutate(
    apps_level_clean = normalise_apps_level(apps_level)
  )

# Clean up temporary objects
rm(provider_region_raw)

# Extract UKPRN from provider name, e.g. "Provider Name (10012345)"
extract_provider_ukprn <- function(provider_name) {
  str_extract(provider_name, "(?<=\\()[0-9]+(?=\\))")
}

provider_region_keyed <- provider_region %>%
  mutate(
    provider_ukprn = extract_provider_ukprn(provider_name)
  )

# Rule-based provider type classification.
# This splits the original "Other" category into universities/HE,
# NHS/health bodies, armed forces/defence, and residual unclassified providers.
provider_region_classified <- provider_region_keyed %>%
  mutate(
    provider_type_detailed = case_when(
      provider_type == "General FE College incl Tertiary" ~ "FE college",
      provider_type == "Private Sector Public Funded" ~ "Private training provider",
      provider_type == "Local Authority" ~ "Local authority",
      provider_type == "Special College" ~ "Special college",
      provider_type == "Sixth Form College" ~ "Sixth form college",
      provider_type == "Schools" ~ "School",

      provider_type == "Other" &
        str_detect(
          provider_name,
          regex(
            "UNIVERSITY|UNIVERSITIES|COLLEGE LONDON|BPP UNIVERSITY|OPEN UNIVERSITY|CRANFIELD",
            ignore_case = TRUE
          )
        ) ~ "University / HE",

      provider_type == "Other" &
        str_detect(
          provider_name,
          regex(
            "NHS|NATIONAL HEALTH SERVICE|AMBULANCE|HOSPITAL|FOUNDATION TRUST|HEALTHCARE",
            ignore_case = TRUE
          )
        ) ~ "NHS / health body",

      provider_type == "Other" &
        str_detect(
          provider_name,
          regex(
            "ARMY|NAVY|RAF|ROYAL AIR FORCE|MINISTRY OF DEFENCE|DEFENCE",
            ignore_case = TRUE
          )
        ) ~ "Armed forces / defence",

      provider_type == "Other" ~ "Other / unclassified",

      TRUE ~ provider_type
    )
  )

# Provider type summary
provider_type_detailed_summary <- provider_region_classified %>%
  group_by(provider_type_detailed) %>%
  summarise(
    providers = n_distinct(provider_name),
    starts = sum(starts, na.rm = TRUE),
    enrolments = sum(enrolments, na.rm = TRUE),
    achievements = sum(achievements, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  arrange(desc(starts))

provider_type_detailed_summary

# Provider type by apprenticeship level
provider_type_detailed_by_level <- provider_region_classified %>%
  group_by(apps_level_clean, provider_type_detailed) %>%
  summarise(
    providers = n_distinct(provider_name),
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(apps_level_clean) %>%
  mutate(
    pct_level_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  ungroup() %>%
  arrange(apps_level_clean, desc(starts))

provider_type_detailed_by_level

provider_region %>%
  count(
    year,
    provider_type,
    apps_level_clean,
    age_group,
    delivery_region,
    learner_home_region,
    sort = TRUE
  ) %>%
  print(n = 50, width = Inf)

provider_region %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_name),
    provider_types = n_distinct(provider_type),
    delivery_regions = n_distinct(delivery_region),
    learner_home_regions = n_distinct(learner_home_region),
    total_starts = sum(starts, na.rm = TRUE),
    total_enrolments = sum(enrolments, na.rm = TRUE),
    total_achievements = sum(achievements, na.rm = TRUE)
  )

provider_region %>%
  count(provider_type, sort = TRUE) %>%
  print(n = Inf)

provider_region %>%
  group_by(provider_type) %>%
  summarise(
    providers = n_distinct(provider_name),
    starts = sum(starts, na.rm = TRUE),
    enrolments = sum(enrolments, na.rm = TRUE),
    achievements = sum(achievements, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starts = round(100 * starts / sum(starts), 1)
  ) %>%
  arrange(desc(starts)) %>%
  print(n = Inf)

# Clean up temporary objects
# (provider_region_keyed is kept: it feeds the provider-standard-region proxy)
rm(provider_region, provider_region_classified)

provider_standard_raw <- read_csv(
  "data/2024_25-starts---subjects-and-standards.csv",
  show_col_types = FALSE
) %>%
  clean_numeric()

provider_standard_starts <- provider_standard_raw %>%
  rename(
    apps_level = apps_Level,
    standard_name = std_fwk_name,
    ssa_tier_1 = ssa_t1_desc,
    ssa_tier_2 = ssa_t2_desc,
    standard_or_framework = std_fwk_flag,
    starts = values
  ) %>%
  filter(
    measure == "Starts",
    !is.na(starts)
  ) %>%
  mutate(
    apps_level_clean = normalise_apps_level(apps_level),
    provider_ukprn = extract_provider_ukprn(provider_name)
  )

# Clean up temporary objects
rm(provider_standard_raw)

# Basic checks
provider_standard_starts %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_name),
    provider_refs = n_distinct(provider_ukprn),
    standards = n_distinct(standard_name),
    total_starts = sum(starts, na.rm = TRUE)
  )

provider_standard_starts_keyed <- provider_standard_starts

provider_standard_starts %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_name),
    provider_types = n_distinct(provider_type),
    standards = n_distinct(standard_name),
    total_starts = sum(starts, na.rm = TRUE)
  )

provider_standard_starts %>%
  count(standard_or_framework, sort = TRUE)

provider_standard_starts %>%
  group_by(apps_level_clean) %>%
  summarise(
    standards = n_distinct(standard_name),
    providers = n_distinct(provider_name),
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_starts = round(100 * starts / sum(starts), 1)
  )

######################
# Match provider-standard file to provider-region file using UKPRN
######################

provider_name_match_check_ukprn <- provider_standard_starts_keyed %>%
  distinct(provider_ukprn, provider_name) %>%
  mutate(
    in_provider_region = provider_ukprn %in% unique(provider_region_keyed$provider_ukprn)
  ) %>%
  summarise(
    providers_in_standard_file = n(),
    providers_with_ukprn = sum(!is.na(provider_ukprn)),
    matched_to_region_file = sum(in_provider_region, na.rm = TRUE),
    unmatched = sum(!in_provider_region | is.na(in_provider_region)),
    match_rate = round(100 * matched_to_region_file / providers_in_standard_file, 1)
  )

provider_name_match_check_ukprn

# Clean up temporary objects
# (provider_standard_starts_keyed carries everything forward from here)
rm(provider_standard_starts)

######################
# Provider-standard-region proxy
######################

# 1. Create provider × level × delivery region shares
# This uses actual provider regional starts, summed across age groups and learner home regions.

provider_delivery_region_profile <- provider_region_keyed %>%
  filter(
    !is.na(provider_ukprn),
    !is.na(apps_level_clean),
    !is.na(delivery_region),
    !is.na(starts)
  ) %>%
  group_by(
    provider_ukprn,
    apps_level_clean,
    delivery_region
  ) %>%
  summarise(
    provider_level_region_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(
    provider_ukprn,
    apps_level_clean
  ) %>%
  mutate(
    provider_level_total_starts = sum(provider_level_region_starts, na.rm = TRUE),
    delivery_region_share = provider_level_region_starts / provider_level_total_starts
  ) %>%
  ungroup() %>%
  filter(
    provider_level_total_starts > 0,
    delivery_region_share > 0
  )

# 2. Join provider-standard starts to provider delivery-region shares.
# This allocates a provider's starts on each standard across delivery regions
# according to that provider's level-specific regional delivery profile.

provider_standard_region_proxy <- provider_standard_starts_keyed %>%
  filter(
    !is.na(provider_ukprn),
    !is.na(apps_level_clean),
    !is.na(starts),
    starts > 0
  ) %>%
  left_join(
    provider_delivery_region_profile,
    by = c("provider_ukprn", "apps_level_clean")
  ) %>%
  mutate(
    estimated_region_starts = starts * delivery_region_share
  )

# 3. Check match quality and whether totals are preserved
provider_standard_region_proxy_check <- provider_standard_region_proxy %>%
  summarise(
    provider_standard_rows = n(),
    rows_with_region_profile = sum(!is.na(delivery_region)),
    rows_without_region_profile = sum(is.na(delivery_region)),
    observed_provider_standard_starts = sum(starts, na.rm = TRUE),
    estimated_region_starts = sum(estimated_region_starts, na.rm = TRUE)
  )

provider_standard_region_proxy_check

# correction
provider_standard_region_proxy_check_clean <- tibble(
  provider_standard_rows = nrow(provider_standard_starts_keyed %>% filter(starts > 0)),
  rows_with_region_profile = provider_standard_region_proxy %>%
    filter(!is.na(delivery_region)) %>%
    distinct(provider_ukprn, apps_level_clean, standard_name, delivery_region) %>%
    nrow(),
  original_provider_standard_starts = provider_standard_starts_keyed %>%
    filter(starts > 0) %>%
    summarise(value = sum(starts, na.rm = TRUE)) %>%
    pull(value),
  estimated_region_starts = provider_standard_region_proxy %>%
    summarise(value = sum(estimated_region_starts, na.rm = TRUE)) %>%
    pull(value)
)

provider_standard_region_proxy_check_clean

# 4. Collapse to standard × delivery region
standard_region_proxy <- provider_standard_region_proxy %>%
  filter(!is.na(delivery_region)) %>%
  group_by(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    standard_or_framework,
    delivery_region
  ) %>%
  summarise(
    estimated_region_starts = sum(estimated_region_starts, na.rm = TRUE),
    providers = n_distinct(provider_ukprn),
    .groups = "drop"
  )

# 5. Standard-level geographic concentration metrics
standard_region_concentration_proxy <- standard_region_proxy %>%
  group_by(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    standard_or_framework
  ) %>%
  arrange(desc(estimated_region_starts), .by_group = TRUE) %>%
  mutate(
    total_estimated_starts = sum(estimated_region_starts, na.rm = TRUE),
    region_share = estimated_region_starts / total_estimated_starts,
    region_rank = row_number()
  ) %>%
  summarise(
    estimated_starts = first(total_estimated_starts),
    regions_with_estimated_starts = n_distinct(delivery_region),
    top_1_region_share = round(100 * sum(region_share[region_rank <= 1], na.rm = TRUE), 1),
    top_3_region_share = round(100 * sum(region_share[region_rank <= 3], na.rm = TRUE), 1),
    hhi_region = round(sum(region_share^2, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(top_1_region_share))

provider_standard_region_proxy_check
standard_region_concentration_proxy %>% print(n = 50, width = Inf)

######################
# Provider-standard-region proxy: concentration by starts band
######################

standard_region_concentration_summary <- standard_region_concentration_proxy %>%
  mutate(
    estimated_starts_band = create_estimated_starts_band(estimated_starts),
    concentrated_flag = top_1_region_share >= 75,
    dispersed_flag = regions_with_estimated_starts >= 5 & top_1_region_share < 50
  ) %>%
  group_by(estimated_starts_band) %>%
  summarise(
    standards = n(),
    median_estimated_starts = median(estimated_starts, na.rm = TRUE),
    median_regions = median(regions_with_estimated_starts, na.rm = TRUE),
    median_top_1_region_share = median(top_1_region_share, na.rm = TRUE),
    median_top_3_region_share = median(top_3_region_share, na.rm = TRUE),
    pct_concentrated = round(100 * mean(concentrated_flag, na.rm = TRUE), 1),
    pct_dispersed = round(100 * mean(dispersed_flag, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      estimated_starts_band,
      levels = c("<50 starts", "50-99 starts", "100-249 starts", "250-499 starts", "500+ starts")
    )
  )

standard_region_concentration_summary


# More meaningful list: standards with at least 50 estimated starts
standard_region_concentration_proxy %>%
  filter(estimated_starts >= 50) %>%
  arrange(desc(top_1_region_share), estimated_starts) %>%
  select(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    estimated_starts,
    regions_with_estimated_starts,
    top_1_region_share,
    top_3_region_share,
    hhi_region
  ) %>%
  print(n = 50, width = Inf)


# Technical standards with at least 50 estimated starts
standard_region_concentration_proxy %>%
  filter(
    estimated_starts >= 50,
    ssa_tier_1 %in% c(
      "Engineering and Manufacturing Technologies",
      "Construction, Planning and the Built Environment",
      "Digital Technology",
      "Science and Mathematics",
      "Health, Public Services and Care"
    )
  ) %>%
  arrange(desc(top_1_region_share), estimated_starts) %>%
  select(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    estimated_starts,
    regions_with_estimated_starts,
    top_1_region_share,
    top_3_region_share,
    hhi_region
  ) %>%
  print(n = 50, width = Inf)

provider_standard_region_proxy_check %>%
  print(width = Inf)

standard_region_concentration_summary %>%
  print(n = Inf, width = Inf)

technical_standard_region_concentration_summary <- standard_region_concentration_proxy %>%
  filter(
    ssa_tier_1 %in% c(
      "Engineering and Manufacturing Technologies",
      "Construction, Planning and the Built Environment",
      "Digital Technology",
      "Science and Mathematics",
      "Health, Public Services and Care"
    )
  ) %>%
  mutate(
    estimated_starts_band = create_estimated_starts_band(estimated_starts),
    concentrated_flag = top_1_region_share >= 75,
    dispersed_flag = regions_with_estimated_starts >= 5 & top_1_region_share < 50
  ) %>%
  group_by(estimated_starts_band) %>%
  summarise(
    standards = n(),
    median_estimated_starts = median(estimated_starts, na.rm = TRUE),
    median_regions = median(regions_with_estimated_starts, na.rm = TRUE),
    median_top_1_region_share = median(top_1_region_share, na.rm = TRUE),
    median_top_3_region_share = median(top_3_region_share, na.rm = TRUE),
    pct_concentrated = round(100 * mean(concentrated_flag, na.rm = TRUE), 1),
    pct_dispersed = round(100 * mean(dispersed_flag, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      estimated_starts_band,
      levels = c("<50 starts", "50-99 starts", "100-249 starts", "250-499 starts", "500+ starts")
    )
  )

technical_standard_region_concentration_summary %>%
  print(n = Inf, width = Inf)

# Clean up temporary objects from the region proxy
rm(
  provider_region_keyed,
  provider_delivery_region_profile,
  provider_standard_region_proxy,
  standard_region_proxy
)

######################
# 09 Provider market trends
######################

# This section tracks whether the apprenticeship provider market has become
# more or less concentrated over time.

# Provider files overlap historically, so we use:
# - app-provider-starts-202425-q4.csv as the canonical full-year source
# - apps_23_provider_starts_202526_6.csv as the in-year 2025/26 source

clean_provider_starts <- function(file_path, release_type) {

  df <- read_csv(file_path, show_col_types = FALSE) %>%
    clean_numeric()

  # Harmonise subject column names across releases
  if ("ssa_tier_1" %in% names(df) == FALSE && "ssa_t1_desc" %in% names(df)) {
    df <- df %>%
      rename(ssa_tier_1 = ssa_t1_desc)
  }

  if ("ssa_tier_1" %in% names(df) == FALSE && "ssa1" %in% names(df)) {
    df <- df %>%
      rename(ssa_tier_1 = ssa1)
  }

  df %>%
    filter(
      apps_level == "Total",
      ssa_tier_1 == "Total",
      !is.na(starts)
    ) %>%
    distinct(
      time_period,
      provider_ukprn,
      provider_name,
      .keep_all = TRUE
    ) %>%
    mutate(
      release_type = release_type,
      source_file = basename(file_path),
      starts_band = create_starts_band(starts)
    )
}

provider_full_year_file <- analysis_files %>%
  filter(file_name == "app-provider-starts-202425-q4.csv")

provider_in_year_file <- analysis_files %>%
  filter(file_name == "apps_23_provider_starts_202526_6.csv")

providers_full_year_base <- provider_full_year_file %>%
  mutate(data = map(file_path, ~ clean_provider_starts(.x, "Full year"))) %>%
  select(data) %>%
  unnest(data)

providers_in_year_base <- provider_in_year_file %>%
  mutate(data = map(file_path, ~ clean_provider_starts(.x, "In-year"))) %>%
  select(data) %>%
  unnest(data)

providers_trend_base <- bind_rows(
  providers_full_year_base,
  providers_in_year_base %>%
    filter(time_period == 202526)
)

provider_trends <- providers_trend_base %>%
  group_by(time_period, release_type) %>%
  summarise(
    providers = n(),
    total_starts = sum(starts),
    median_starts = median(starts),
    p25 = quantile(starts, 0.25),
    p10 = quantile(starts, 0.10),
    small_providers = sum(starts < 50),
    pct_small_providers = round(100 * small_providers / providers, 1),
    .groups = "drop"
  ) %>%
  arrange(time_period)

provider_trends

provider_concentration_trends <- providers_trend_base %>%
  group_by(time_period, release_type) %>%
  arrange(desc(starts), .by_group = TRUE) %>%
  mutate(
    rank = row_number(),
    cumulative_share = cumsum(starts) / sum(starts)
  ) %>%
  filter(rank %in% c(10, 25, 50, 100)) %>%
  transmute(
    time_period,
    release_type,
    top_n_providers = rank,
    cumulative_share = round(100 * cumulative_share, 1)
  ) %>%
  ungroup()

provider_concentration_trends

# Clean up temporary objects
rm(
  provider_full_year_file,
  provider_in_year_file,
  providers_full_year_base,
  providers_in_year_base,
  providers_trend_base
)


######################
# 10 Provider subject concentration and quadrant examples
######################

# This section checks whether broad subject areas have concentrated provider
# markets, and extracts illustrative examples from the starts-provider footprint
# quadrants.

provider_subject_base <- provider_starts %>%
  filter(
    time_period == 202425,
    apps_level == "Total",
    ssa_tier_1 != "Total",
    !is.na(starts)
  ) %>%
  distinct(
    provider_ukprn,
    provider_name,
    ssa_tier_1,
    .keep_all = TRUE
  )

provider_subject_summary <- provider_subject_base %>%
  group_by(ssa_tier_1) %>%
  summarise(
    providers = n_distinct(provider_ukprn),
    total_starts = sum(starts),
    median_starts_per_provider = median(starts),
    mean_starts_per_provider = mean(starts),
    .groups = "drop"
  ) %>%
  arrange(providers)

provider_subject_summary

provider_subject_concentration <- provider_subject_base %>%
  group_by(ssa_tier_1) %>%
  arrange(desc(starts), .by_group = TRUE) %>%
  mutate(
    rank = row_number(),
    cumulative_share = cumsum(starts) / sum(starts)
  ) %>%
  filter(rank %in% c(5, 10, 25)) %>%
  transmute(
    ssa_tier_1,
    top_n_providers = rank,
    cumulative_share = round(100 * cumulative_share, 1)
  ) %>%
  ungroup()

provider_subject_concentration

# Representative examples from each thin-market quadrant.
# These are selected as standards closest to the median starts and median
# provider footprint within each quadrant, rather than simply the smallest cases.
quadrant_examples_representative <- thin_market_summary %>%
  group_by(market_type) %>%
  mutate(
    median_starts_in_quadrant = median(starts, na.rm = TRUE),
    median_providers_in_quadrant = median(providers, na.rm = TRUE),
    distance_from_quadrant_median =
      abs(log10(starts) - log10(median_starts_in_quadrant)) +
      abs(log10(providers) - log10(median_providers_in_quadrant))
  ) %>%
  arrange(distance_from_quadrant_median) %>%
  slice_head(n = 8) %>%
  ungroup() %>%
  select(
    market_type,
    std_fwk_name_stcode,
    ssa_tier_1,
    apps_level_detailed,
    starts,
    providers,
    leavers
  )

quadrant_examples_representative

# Clean up temporary objects
# (provider_starts is no longer needed after the subject concentration analysis)
rm(provider_starts, provider_subject_base)

######################
# Funding bands and thin-market merge
######################

# Check standard reference availability in thin-market data
thin_market_summary %>%
  summarise(
    standards = n(),
    standard_refs = n_distinct(standard_ref, na.rm = TRUE),
    missing_standard_ref = sum(is.na(standard_ref))
  )

# Keep one Skills England record per standard reference.
# If there are multiple versions, keep the latest version where possible.
skills_england_match_base <- skills_england_standards %>%
  filter(
    programme_type == "Apprenticeship standard",
    !is.na(standard_ref)
  ) %>%
  arrange(
    standard_ref,
    desc(is_available_for_starts),
    desc(is_active),
    desc(version_number)
  ) %>%
  distinct(
    standard_ref,
    .keep_all = TRUE
  ) %>%
  select(
    standard_ref,
    skills_standard_name = standard_name,
    programme_type,
    route,
    level,
    status,
    max_funding,
    typical_duration,
    minimum_hours_for_compliance,
    regulated_standard,
    integrated_degree,
    integrated_apprenticeship,
    link
  )

thin_market_funding <- thin_market_summary %>%
  left_join(
    skills_england_match_base,
    by = "standard_ref"
  )

thin_market_funding_match_check <- thin_market_funding %>%
  summarise(
    standards = n(),
    matched_to_skills_england = sum(!is.na(max_funding)),
    unmatched = sum(is.na(max_funding)),
    match_rate = round(100 * matched_to_skills_england / standards, 1)
  )

thin_market_funding_match_check

thin_market_funding %>%
  filter(is.na(max_funding)) %>%
  select(
    standard_ref,
    std_fwk_name_stcode,
    starts,
    providers,
    market_type
  ) %>%
  arrange(desc(starts)) %>%
  print(n = 50, width = Inf)

######################
# Funding bands by thin-market quadrant
######################

# Compute funding_p75 before it is used in funding_by_quadrant and
# potentially_fragile_standards
funding_p75 <- quantile(thin_market_funding$max_funding, 0.75, na.rm = TRUE)

funding_by_quadrant <- thin_market_funding %>%
  group_by(market_type) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    median_standard_starts = median(starts, na.rm = TRUE),
    median_providers = median(providers, na.rm = TRUE),
    median_funding = median(max_funding, na.rm = TRUE),
    mean_funding = mean(max_funding, na.rm = TRUE),
    p75_funding = quantile(max_funding, 0.75, na.rm = TRUE),
    high_funding_standards = sum(
      max_funding >= quantile(thin_market_funding$max_funding, 0.75, na.rm = TRUE),
      na.rm = TRUE
    ),
    pct_high_funding = round(
      100 * high_funding_standards / standards,
      1
    ),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_high_funding))

funding_by_quadrant %>%
  print(n = Inf, width = Inf)

######################
# Route and level mix by thin-market quadrant
######################

route_mix_by_quadrant <- thin_market_funding %>%
  group_by(market_type, route) %>%
  summarise(
    standards = n(),
    starts = sum(starts, na.rm = TRUE),
    median_funding = median(max_funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(market_type) %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1)
  ) %>%
  ungroup() %>%
  arrange(market_type, desc(standards))

route_mix_by_quadrant %>%
  print(n = 80, width = Inf)


level_mix_by_quadrant <- thin_market_funding %>%
  group_by(market_type, level) %>%
  summarise(
    standards = n(),
    starts = sum(starts, na.rm = TRUE),
    median_funding = median(max_funding, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(market_type) %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1)
  ) %>%
  ungroup() %>%
  arrange(market_type, level)

level_mix_by_quadrant %>%
  print(n = Inf)

######################
# Potentially fragile standards
######################

priority_routes <- c(
  "Engineering and manufacturing",
  "Construction and the built environment",
  "Health and science",
  "Digital",
  "Transport and logistics",
  "Agriculture, environmental and animal care"
)

potentially_fragile_standards <- thin_market_funding %>%
  mutate(
    low_starts = starts < 50,
    below_median_starts = starts < median(starts, na.rm = TRUE),
    few_providers = providers < median(providers, na.rm = TRUE),
    high_funding = max_funding >= funding_p75,
    priority_route = route %in% priority_routes,
    level_3_5 = level %in% 3:5,
    fragile_score =
      as.integer(low_starts) +
      as.integer(few_providers) +
      as.integer(high_funding) +
      as.integer(priority_route) +
      as.integer(level_3_5)
  ) %>%
  filter(
    priority_route,
    high_funding,
    starts < median(starts, na.rm = TRUE),
    providers <= median(providers, na.rm = TRUE)
  ) %>%
  arrange(
    desc(fragile_score),
    starts,
    providers,
    desc(max_funding)
  ) %>%
  select(
    standard_ref,
    std_fwk_name_stcode,
    route,
    level,
    market_type,
    starts,
    providers,
    leavers,
    max_funding,
    typical_duration,
    regulated_standard,
    integrated_degree,
    fragile_score
  )

potentially_fragile_standards %>%
  print(n = 60, width = Inf)

# Clean up temporary objects
rm(skills_england_match_base)


######################
# Diagnostic: LAD provider delivery breakdowns
######################

provider_lad_raw <- read_csv(
  "data/lad-2024_25.csv",
  show_col_types = FALSE
) %>%
  clean_numeric()

glimpse(provider_lad_raw)

names(provider_lad_raw)

provider_lad_raw %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_name),
    learner_home_lads = n_distinct(`Learner home LAD`),
    delivery_lads = n_distinct(`Delivery LAD`),
    total_starts = sum(starts, na.rm = TRUE),
    total_enrolments = sum(enrolments, na.rm = TRUE),
    total_achievements = sum(achievements, na.rm = TRUE)
  )

######################
# LAD provider delivery breakdowns: clean base
######################

provider_lad <- provider_lad_raw %>%
  rename(
    learner_home_lad = `Learner home LAD`,
    delivery_lad = `Delivery LAD`
  ) %>%
  mutate(
    provider_ukprn = extract_provider_ukprn(provider_name)
  )

# Clean up temporary objects
rm(provider_lad_raw)

# Basic validation
provider_lad %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_ukprn),
    learner_home_lads = n_distinct(learner_home_lad),
    delivery_lads = n_distinct(delivery_lad),
    total_starts = sum(starts, na.rm = TRUE),
    total_enrolments = sum(enrolments, na.rm = TRUE),
    total_achievements = sum(achievements, na.rm = TRUE)
  )

######################
# Learner home LAD vs delivery LAD
######################

lad_home_delivery_flows <- provider_lad %>%
  group_by(
    learner_home_lad,
    delivery_lad
  ) %>%
  summarise(
    starts = sum(starts, na.rm = TRUE),
    enrolments = sum(enrolments, na.rm = TRUE),
    achievements = sum(achievements, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    same_lad = learner_home_lad == delivery_lad
  )

same_lad_summary <- lad_home_delivery_flows %>%
  summarise(
    total_starts = sum(starts, na.rm = TRUE),
    same_lad_starts = sum(starts[same_lad], na.rm = TRUE),
    different_lad_starts = sum(starts[!same_lad], na.rm = TRUE),
    pct_same_lad = round(100 * same_lad_starts / total_starts, 1),
    pct_different_lad = round(100 * different_lad_starts / total_starts, 1)
  )

same_lad_summary


######################
# Provider LAD delivery footprint
######################

provider_lad_footprint <- provider_lad %>%
  filter(starts > 0) %>%
  group_by(
    provider_ukprn,
    provider_name
  ) %>%
  summarise(
    starts = sum(starts, na.rm = TRUE),
    delivery_lads = n_distinct(delivery_lad),
    learner_home_lads = n_distinct(learner_home_lad),
    .groups = "drop"
  ) %>%
  arrange(desc(starts))

provider_lad_footprint %>%
  summarise(
    providers = n(),
    median_starts = median(starts, na.rm = TRUE),
    median_delivery_lads = median(delivery_lads, na.rm = TRUE),
    median_learner_home_lads = median(learner_home_lads, na.rm = TRUE),
    p75_delivery_lads = quantile(delivery_lads, 0.75, na.rm = TRUE),
    max_delivery_lads = max(delivery_lads, na.rm = TRUE)
  )

provider_lad_footprint %>%
  slice_head(n = 30) %>%
  print(n = 30, width = Inf)


######################
# same_lad_chart_data: used in Chart p_same_lad_delivery (03_visualisation.R)
######################

same_lad_chart_data <- same_lad_summary %>%
  pivot_longer(
    cols = c(same_lad_starts, different_lad_starts),
    names_to = "delivery_type",
    values_to = "starts"
  ) %>%
  mutate(
    delivery_type = case_when(
      delivery_type == "same_lad_starts" ~ "Same LAD as learner home",
      delivery_type == "different_lad_starts" ~ "Different delivery LAD",
      TRUE ~ delivery_type
    ),
    pct_starts = round(100 * starts / sum(starts), 1),
    delivery_type = factor(
      delivery_type,
      levels = c("Same LAD as learner home", "Different delivery LAD")
    )
  )

# Clean up temporary objects
rm(lad_home_delivery_flows, same_lad_summary)

######################
# Provider-standard-LAD proxy
######################

# 1. Create provider × delivery LAD shares
provider_delivery_lad_profile <- provider_lad %>%
  filter(
    !is.na(provider_ukprn),
    !is.na(delivery_lad),
    !is.na(starts)
  ) %>%
  group_by(
    provider_ukprn,
    delivery_lad
  ) %>%
  summarise(
    provider_lad_starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(provider_ukprn) %>%
  mutate(
    provider_total_starts = sum(provider_lad_starts, na.rm = TRUE),
    delivery_lad_share = provider_lad_starts / provider_total_starts
  ) %>%
  ungroup() %>%
  filter(
    provider_total_starts > 0,
    delivery_lad_share > 0
  )

# 2. Join provider-standard starts to provider LAD profiles.
# This estimates where provider-standard starts are delivered by applying
# each provider's overall delivery LAD profile.
provider_standard_lad_proxy <- provider_standard_starts_keyed %>%
  filter(
    !is.na(provider_ukprn),
    !is.na(starts),
    starts > 0
  ) %>%
  left_join(
    provider_delivery_lad_profile,
    by = "provider_ukprn"
  ) %>%
  mutate(
    estimated_lad_starts = starts * delivery_lad_share
  )

# 3. Validation check
provider_standard_lad_proxy_check <- tibble(
  original_provider_standard_rows = provider_standard_starts_keyed %>%
    filter(starts > 0) %>%
    nrow(),
  expanded_provider_standard_lad_rows = provider_standard_lad_proxy %>%
    filter(!is.na(delivery_lad)) %>%
    nrow(),
  original_provider_standard_starts = provider_standard_starts_keyed %>%
    filter(starts > 0) %>%
    summarise(value = sum(starts, na.rm = TRUE)) %>%
    pull(value),
  estimated_lad_starts = provider_standard_lad_proxy %>%
    summarise(value = sum(estimated_lad_starts, na.rm = TRUE)) %>%
    pull(value),
  starts_difference = estimated_lad_starts - original_provider_standard_starts
)

provider_standard_lad_proxy_check

# Clean up temporary objects
rm(provider_lad, provider_delivery_lad_profile)

######################
# Standard-level LAD concentration proxy
######################

standard_lad_proxy <- provider_standard_lad_proxy %>%
  filter(!is.na(delivery_lad)) %>%
  group_by(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    standard_or_framework,
    delivery_lad
  ) %>%
  summarise(
    estimated_lad_starts = sum(estimated_lad_starts, na.rm = TRUE),
    providers = n_distinct(provider_ukprn),
    .groups = "drop"
  )

standard_lad_concentration_proxy <- standard_lad_proxy %>%
  group_by(
    standard_name,
    ssa_tier_1,
    apps_level_clean,
    standard_or_framework
  ) %>%
  arrange(desc(estimated_lad_starts), .by_group = TRUE) %>%
  mutate(
    total_estimated_starts = sum(estimated_lad_starts, na.rm = TRUE),
    lad_share = estimated_lad_starts / total_estimated_starts,
    lad_rank = row_number()
  ) %>%
  summarise(
    estimated_starts = first(total_estimated_starts),
    delivery_lads_with_estimated_starts = n_distinct(delivery_lad),
    top_1_lad_share = round(100 * sum(lad_share[lad_rank <= 1], na.rm = TRUE), 1),
    top_5_lad_share = round(100 * sum(lad_share[lad_rank <= 5], na.rm = TRUE), 1),
    top_10_lad_share = round(100 * sum(lad_share[lad_rank <= 10], na.rm = TRUE), 1),
    hhi_lad = round(sum(lad_share^2, na.rm = TRUE), 4),
    .groups = "drop"
  ) %>%
  arrange(desc(top_1_lad_share))

standard_lad_concentration_proxy %>%
  print(n = 50, width = Inf)

######################
# LAD concentration by standard starts band
######################

standard_lad_concentration_summary <- standard_lad_concentration_proxy %>%
  mutate(
    estimated_starts_band = create_estimated_starts_band(estimated_starts),
    highly_localised_flag = top_5_lad_share >= 75,
    dispersed_flag = delivery_lads_with_estimated_starts >= 25 & top_5_lad_share < 50
  ) %>%
  group_by(estimated_starts_band) %>%
  summarise(
    standards = n(),
    median_estimated_starts = median(estimated_starts, na.rm = TRUE),
    median_delivery_lads = median(delivery_lads_with_estimated_starts, na.rm = TRUE),
    median_top_1_lad_share = median(top_1_lad_share, na.rm = TRUE),
    median_top_5_lad_share = median(top_5_lad_share, na.rm = TRUE),
    median_top_10_lad_share = median(top_10_lad_share, na.rm = TRUE),
    pct_highly_localised = round(100 * mean(highly_localised_flag, na.rm = TRUE), 1),
    pct_dispersed = round(100 * mean(dispersed_flag, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(
    factor(
      estimated_starts_band,
      levels = c("<50 starts", "50-99 starts", "100-249 starts", "250-499 starts", "500+ starts")
    )
  )

standard_lad_concentration_summary %>%
  print(n = Inf, width = Inf)

######################
# Join key for thin-market quadrant analysis
######################

thin_market_funding_join <- thin_market_funding %>%
  mutate(
    standard_name_clean = std_fwk_name_stcode %>%
      str_remove("\\s*\\(ST[0-9]+\\)$") %>%
      str_remove("\\s*\\(FA[0-9]+\\)$") %>%
      str_squish()
  ) %>%
  select(
    standard_ref,
    standard_name_clean,
    std_fwk_name_stcode,
    market_type,
    starts,
    providers,
    leavers,
    route,
    level,
    max_funding
  )

######################
# LAD concentration by thin-market quadrant
######################

standard_lad_concentration_quadrant <- standard_lad_concentration_proxy %>%
  mutate(
    standard_name_clean = str_squish(standard_name)
  ) %>%
  left_join(
    thin_market_funding_join,
    by = "standard_name_clean"
  )

standard_lad_quadrant_match_check <- standard_lad_concentration_quadrant %>%
  summarise(
    standards = n(),
    matched_to_quadrant = sum(!is.na(market_type)),
    unmatched = sum(is.na(market_type)),
    match_rate = round(100 * matched_to_quadrant / standards, 1)
  )

standard_lad_quadrant_match_check

lad_concentration_by_quadrant <- standard_lad_concentration_quadrant %>%
  filter(!is.na(market_type)) %>%
  mutate(
    highly_localised_flag = top_5_lad_share >= 75,
    dispersed_flag = delivery_lads_with_estimated_starts >= 25 & top_5_lad_share < 50
  ) %>%
  group_by(market_type) %>%
  summarise(
    standards = n(),
    total_estimated_starts = sum(estimated_starts, na.rm = TRUE),
    median_estimated_starts = median(estimated_starts, na.rm = TRUE),
    median_delivery_lads = median(delivery_lads_with_estimated_starts, na.rm = TRUE),
    median_top_1_lad_share = median(top_1_lad_share, na.rm = TRUE),
    median_top_5_lad_share = median(top_5_lad_share, na.rm = TRUE),
    median_top_10_lad_share = median(top_10_lad_share, na.rm = TRUE),
    pct_highly_localised = round(100 * mean(highly_localised_flag, na.rm = TRUE), 1),
    pct_dispersed = round(100 * mean(dispersed_flag, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_highly_localised))

lad_concentration_by_quadrant %>%
  print(n = Inf, width = Inf)

# Clean up temporary objects from the LAD proxy
rm(standard_lad_proxy, standard_lad_concentration_quadrant)


######################
# 11 Historical starts analysis
######################

historical_summary <- read_analysis_csv("app-historical-summary-to-2425.csv")

historical_starts <- historical_summary %>%
  filter(
    geographic_level == "National",
    apps_level == "Total",
    age_summary == "Total",
    ssa_tier_1 == "Total",
    !is.na(starts)
  ) %>%
  select(
    time_period,
    starts
  ) %>%
  distinct() %>%
  mutate(
    academic_year = make_academic_year(time_period)
  )

# Clean up temporary objects
rm(historical_summary)


######################
# Save analytical objects for downstream scripts
######################

dir.create(file.path(output_folder, "data"), showWarnings = FALSE, recursive = TRUE)

saveRDS(standards_base,                                file.path(output_folder, "data", "standards_base.rds"))
saveRDS(standards_subject_base,                        file.path(output_folder, "data", "standards_subject_base.rds"))
saveRDS(standards_subject_trends,                      file.path(output_folder, "data", "standards_subject_trends.rds"))
saveRDS(standards_concentration_trends,                file.path(output_folder, "data", "standards_concentration_trends.rds"))
saveRDS(low_start_subject_share_trends,                file.path(output_folder, "data", "low_start_subject_share_trends.rds"))
saveRDS(route_summary,                                 file.path(output_folder, "data", "route_summary.rds"))
saveRDS(route_compare,                                 file.path(output_folder, "data", "route_compare.rds"))
saveRDS(standards_dist,                                file.path(output_folder, "data", "standards_dist.rds"))
saveRDS(providers_base,                                file.path(output_folder, "data", "providers_base.rds"))
saveRDS(providers_dist,                                file.path(output_folder, "data", "providers_dist.rds"))
saveRDS(provider_concentration,                        file.path(output_folder, "data", "provider_concentration.rds"))
saveRDS(provider_concentration_trends,                 file.path(output_folder, "data", "provider_concentration_trends.rds"))
saveRDS(provider_subject_concentration,                file.path(output_folder, "data", "provider_subject_concentration.rds"))
saveRDS(thin_market_summary,                           file.path(output_folder, "data", "thin_market_summary.rds"))
saveRDS(thin_market_funding,                           file.path(output_folder, "data", "thin_market_funding.rds"))
saveRDS(potentially_fragile_standards,                 file.path(output_folder, "data", "potentially_fragile_standards.rds"))
saveRDS(age_trends,                                    file.path(output_folder, "data", "age_trends.rds"))
saveRDS(level_trends,                                  file.path(output_folder, "data", "level_trends.rds"))
saveRDS(higher_25_trend,                               file.path(output_folder, "data", "higher_25_trend.rds"))
saveRDS(lsip_base,                                     file.path(output_folder, "data", "lsip_base.rds"))
saveRDS(lsip_level_variation,                          file.path(output_folder, "data", "lsip_level_variation.rds"))
saveRDS(lsip_age_variation,                            file.path(output_folder, "data", "lsip_age_variation.rds"))
saveRDS(lsip_age_level_variation,                      file.path(output_folder, "data", "lsip_age_level_variation.rds"))
saveRDS(lsip_age_level_concentration,                  file.path(output_folder, "data", "lsip_age_level_concentration.rds"))
saveRDS(regional_engineering,                          file.path(output_folder, "data", "regional_engineering.rds"))
saveRDS(technical_standard_region_concentration_summary, file.path(output_folder, "data", "technical_standard_region_concentration_summary.rds"))
saveRDS(lad_concentration_by_quadrant,                 file.path(output_folder, "data", "lad_concentration_by_quadrant.rds"))
saveRDS(same_lad_chart_data,                           file.path(output_folder, "data", "same_lad_chart_data.rds"))
saveRDS(funding_by_quadrant,                           file.path(output_folder, "data", "funding_by_quadrant.rds"))
saveRDS(historical_starts,                             file.path(output_folder, "data", "historical_starts.rds"))
saveRDS(starts_threshold,                              file.path(output_folder, "data", "starts_threshold.rds"))
saveRDS(providers_threshold,                           file.path(output_folder, "data", "providers_threshold.rds"))
# Additional objects needed by 02_analysis.R and 03_visualisation.R
saveRDS(lsip_age_base,                                 file.path(output_folder, "data", "lsip_age_base.rds"))
saveRDS(lsip_level_base,                               file.path(output_folder, "data", "lsip_level_base.rds"))
saveRDS(regional_engineering,                          file.path(output_folder, "data", "regional_engineering.rds"))
saveRDS(lad_engineering,                               file.path(output_folder, "data", "lad_engineering.rds"))
saveRDS(lad_engineering_top_30,                        file.path(output_folder, "data", "lad_engineering_top_30.rds"))
saveRDS(funding_p75,                                   file.path(output_folder, "data", "funding_p75.rds"))
saveRDS(thin_market_funding_join,                      file.path(output_folder, "data", "thin_market_funding_join.rds"))
saveRDS(provider_standard_lad_proxy,                   file.path(output_folder, "data", "provider_standard_lad_proxy.rds"))
saveRDS(standard_region_concentration_proxy,           file.path(output_folder, "data", "standard_region_concentration_proxy.rds"))
# Objects needed by the industry (IS8), provider-type and spatial-spread
# sections in 02_analysis.R and 03_visualisation.R
saveRDS(provider_type_detailed_by_level,               file.path(output_folder, "data", "provider_type_detailed_by_level.rds"))
saveRDS(regional_priority_subjects,                    file.path(output_folder, "data", "regional_priority_subjects.rds"))
saveRDS(lad_priority_subjects,                         file.path(output_folder, "data", "lad_priority_subjects.rds"))
