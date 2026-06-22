################################################################################
## 04_exec_summary_facts.R
##
## Standalone script for Michael / executive-summary figures.
## This script reloads the objects it needs, rather than assuming they remain
## in memory after 01_exploratory.R.
##
## CHANGE LOG (this version):
## - Added extract_provider_ukprn(), which was called in section 04 but never
##   defined in this script (it only existed in 01_exploratory.R). This was
##   the missing-function error you were hitting.
## - Tidied the indentation of the provider-standard Google Drive reload block
##   in section 04 so the `if` statements are unambiguous standalone
##   statements rather than looking like (but not being) part of the pipe.
##   No logic has changed - this block executed correctly before, but the
##   layout invited a future edit that would have broken it.
################################################################################

######################
# 00 Packages and config
######################

library(tidyverse)
library(readr)
library(stringr)
library(scales)

source("config.R")

# If you have not yet added derived_data_folder to config.R, create it here.
if (!exists("derived_data_folder")) {
  derived_data_folder <- file.path(output_folder, "data")
}

if (!dir.exists(derived_data_folder)) {
  dir.create(derived_data_folder, recursive = TRUE)
}

exec_output_folder <- file.path(output_folder, "exec_summary_facts")

if (!dir.exists(exec_output_folder)) {
  dir.create(exec_output_folder, recursive = TRUE)
}

######################
# 01 Helper functions
######################

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
    "percent",
    "funding",
    "duration",
    "level"
  )
  
  cols_to_convert <- names(df)[
    purrr::map_lgl(names(df), ~ any(stringr::str_detect(.x, num_patterns)))
  ]
  
  df %>%
    mutate(
      across(
        all_of(cols_to_convert),
        ~ suppressWarnings(as.numeric(gsub(",", "", .x)))
      )
    )
}

extract_standard_ref <- function(x) {
  str_extract(x, "ST[0-9]+|FA[0-9]+")
}

clean_standard_name <- function(x) {
  x %>%
    str_remove("\\s*\\(ST[0-9]+\\)$") %>%
    str_remove("\\s*\\(FA[0-9]+\\)$") %>%
    str_squish()
}

# Extract UKPRN from provider name, e.g. "Provider Name (10012345)".
# This was previously only defined in 01_exploratory.R. It is duplicated
# here (rather than sourcing 01_exploratory.R) so that 04 stays standalone
# and does not require loading 01's full data pipeline into memory.
# If this regex is ever changed in 01_exploratory.R, change it here too.
extract_provider_ukprn <- function(provider_name) {
  str_extract(provider_name, "(?<=\\()[0-9]+(?=\\))")
}

pct <- function(x, denom, digits = 1) {
  ifelse(
    is.na(denom) | denom == 0,
    NA_real_,
    round(100 * x / denom, digits)
  )
}

safe_first_non_missing <- function(x) {
  out <- x[!is.na(x)]
  if (length(out) == 0) {
    NA
  } else {
    out[1]
  }
}

normalise_apps_level <- function(apps_level) {
  case_when(
    apps_level %in% c("Intermediate", "Intermediate Apprenticeship") ~ "Intermediate",
    apps_level %in% c("Advanced", "Advanced Apprenticeship") ~ "Advanced",
    apps_level %in% c("Higher", "Higher Apprenticeship") ~ "Higher",
    apps_level == "Total" ~ "Total",
    TRUE ~ apps_level
  )
}

write_exec_csv <- function(df, file_name) {
  readr::write_csv(
    df,
    file.path(exec_output_folder, file_name)
  )
}

write_exec_xlsx <- function(named_tables, file_name = "exec_summary_facts_tables.xlsx") {
  if (requireNamespace("openxlsx", quietly = TRUE)) {
    wb <- openxlsx::createWorkbook()
    
    for (sheet_name in names(named_tables)) {
      safe_sheet_name <- str_sub(sheet_name, 1, 31)
      openxlsx::addWorksheet(wb, safe_sheet_name)
      openxlsx::writeData(wb, safe_sheet_name, named_tables[[sheet_name]])
    }
    
    openxlsx::saveWorkbook(
      wb,
      file.path(exec_output_folder, file_name),
      overwrite = TRUE
    )
  } else {
    message("Package 'openxlsx' is not installed. CSVs have still been exported.")
  }
}

load_rds_flex <- function(object_name) {
  
  candidate_paths <- c(
    file.path(derived_data_folder, paste0(object_name, ".rds")),
    file.path("data", paste0(object_name, ".rds")),
    file.path(output_folder, "data", paste0(object_name, ".rds"))
  )
  
  existing_paths <- candidate_paths[file.exists(candidate_paths)]
  
  if (length(existing_paths) == 0) {
    stop(
      paste0(
        "Could not find RDS for object: ", object_name, "\n",
        "Looked in:\n",
        paste(candidate_paths, collapse = "\n")
      )
    )
  }
  
  readRDS(existing_paths[1])
}

file_exists_or_stop <- function(path, label = path) {
  if (!file.exists(path)) {
    stop(paste("Missing required file:", label, "\nPath checked:", path))
  }
  invisible(TRUE)
}

######################
# 02 Reload Skills England standards metadata
######################

# Do not rely on skills_england_standards still being in memory.
# Reload from raw Skills England file.

# 1. Load required packages (and install googledrive/tidyverse if missing)
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library(googledrive)
library(tidyverse)

# 2. De-authorize Google Drive so it bypasses personal login screens
drive_deauth()

# 3. Define your file ID and local target name
file_id <- "1BBwYje2zn7GCSOZ-5gfOMc7jCQyhQ8vc"
local_file <- "Apprenticeships.csv"

# 4. Download the file silently into your Posit Cloud project directory
drive_download(
  as_id(file_id), 
  path = local_file, 
  overwrite = TRUE
)

# 5. Read the local file and execute your existing cleaning pipeline
skills_england_standards <- read_csv(
  local_file,
  skip = 1,
  show_col_types = FALSE
) %>%
  clean_numeric() %>% # Assumes clean_numeric() is predefined in your script/package
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

# 6. Verify the final result
head(skills_england_standards)

skills_england_standards %>%
  summarise(
    rows = n(),
    standards = n_distinct(standard_ref),
    active = sum(is_active, na.rm = TRUE),
    available_for_starts = sum(is_available_for_starts, na.rm = TRUE)
  ) %>%
  print()

######################
# 03 Reload derived analysis objects
######################

# These should have been saved by 01_exploratory.R.
# The flexible loader checks both outputs/data and data.

standards_base <- load_rds_flex("standards_base")
standards_subject_base <- load_rds_flex("standards_subject_base")
thin_market_summary <- load_rds_flex("thin_market_summary")
thin_market_funding <- load_rds_flex("thin_market_funding")
potentially_fragile_standards <- load_rds_flex("potentially_fragile_standards")

# These are useful but can be recreated if missing.
starts_threshold <- tryCatch(
  load_rds_flex("starts_threshold"),
  error = function(e) {
    median(thin_market_funding$starts, na.rm = TRUE)
  }
)

providers_threshold <- tryCatch(
  load_rds_flex("providers_threshold"),
  error = function(e) {
    median(thin_market_funding$providers, na.rm = TRUE)
  }
)

funding_p75 <- tryCatch(
  load_rds_flex("funding_p75"),
  error = function(e) {
    quantile(thin_market_funding$max_funding, 0.75, na.rm = TRUE)
  }
)

######################
# 04 Reload provider-standard dashboard starts if needed
######################

# This is only needed for standards-family provider counts.
# The exec script can still run without it, but family provider counts will be NA.

# 1. Load required packages (and install googledrive/tidyverse if missing)
if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library(googledrive)
library(tidyverse)

# 2. De-authorize Google Drive so it bypasses personal login screens
drive_deauth()

# 3. Define the new file ID and the exact file name
new_file_id <- "1V3o32Kpkv9g7sDrag9dmjoPYJDivC9la"
local_csv_name <- "2024_25-starts---subjects-and-standards.csv"

# 4. Download the file into your Posit Cloud project directory
drive_download(
  as_id(new_file_id), 
  path = local_csv_name, 
  overwrite = TRUE
)

# 5. Load the raw data without any cleaning steps
provider_standard_starts_path <- read_csv(
  local_csv_name,
  show_col_types = FALSE
)

# 6. Verify the file loaded correctly
head(provider_standard_starts_path)

# 7. Clean and rename. This used to be written with the `if` blocks
# indented as though they were part of the pipe below `clean_numeric()`.
# They are NOT part of the pipe - the pipe ends at clean_numeric(), and each
# `if` is a separate top-level statement that reassigns
# provider_standard_starts_keyed in place. That still worked, but the
# indentation made it look broken/ambiguous. Restructured below with the
# same logic, written unambiguously.
provider_standard_starts_keyed <- provider_standard_starts_path %>%
  clean_numeric()

# Rename only if the source columns exist (handles dashboard exports where
# column names vary slightly between downloads).
if ("apps_Level" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(apps_level = apps_Level)
}

if ("std_fwk_name" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(standard_name = std_fwk_name)
}

if ("ssa_t1_desc" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(ssa_tier_1 = ssa_t1_desc)
}

if ("ssa_t2_desc" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(ssa_tier_2 = ssa_t2_desc)
}

if ("std_fwk_flag" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(standard_or_framework = std_fwk_flag)
}

if ("values" %in% names(provider_standard_starts_keyed)) {
  provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
    rename(starts = values)
}

provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
  filter(
    measure == "Starts",
    !is.na(starts),
    starts > 0
  ) %>%
  mutate(
    apps_level_clean = normalise_apps_level(apps_level),
    provider_ukprn = extract_provider_ukprn(provider_name),
    standard_name_clean = clean_standard_name(standard_name),
    standard_ref = extract_standard_ref(standard_name)
  )

provider_standard_starts_keyed %>%
  summarise(
    rows = n(),
    providers = n_distinct(provider_ukprn),
    standards = n_distinct(standard_name_clean),
    total_starts = sum(starts, na.rm = TRUE)
  ) %>%
  print()

######################
# 05 Defensive object checks
######################

required_objects <- c(
  "skills_england_standards",
  "standards_base",
  "standards_subject_base",
  "thin_market_summary",
  "thin_market_funding",
  "potentially_fragile_standards"
)

missing_objects <- required_objects[
  !map_lgl(required_objects, exists, envir = .GlobalEnv)
]

if (length(missing_objects) > 0) {
  stop(
    paste(
      "Missing required objects after reload:",
      paste(missing_objects, collapse = ", ")
    )
  )
}

required_thin_cols <- c(
  "standard_ref",
  "std_fwk_name_stcode",
  "starts",
  "providers",
  "market_type",
  "route",
  "level",
  "max_funding"
)

missing_thin_cols <- setdiff(required_thin_cols, names(thin_market_funding))

if (length(missing_thin_cols) > 0) {
  stop(
    paste(
      "thin_market_funding is missing required columns:",
      paste(missing_thin_cols, collapse = ", ")
    )
  )
}

required_standards_cols <- c(
  "std_fwk_name_stcode",
  "route",
  "apps_level_detailed",
  "starts"
)

missing_standards_cols <- setdiff(required_standards_cols, names(standards_base))

if (length(missing_standards_cols) > 0) {
  stop(
    paste(
      "standards_base is missing required columns:",
      paste(missing_standards_cols, collapse = ", ")
    )
  )
}

message("All required objects loaded successfully. You can now run the executive summary facts block.")



################################################################################
# Executive summary facts
################################################################################

# This section produces a set of defensible headline numbers for the report.
# It uses:
# - observable standards with starts as the denominator for starts-volume figures;
# - the current thin-market quadrant approach for quadrant and sensitivity tables;
# - Skills England metadata for status, route, level, funding and regulated fields;
# - provider-standard starts dashboard data where available for family-level
#   unique provider counts.
#
# Important denominator note:
# "observable standards" means standards with positive starts in the cleaned
# standards-level starts data. This is not the same as all approved standards
# in the Skills England standards catalogue.

exec_output_folder <- file.path(output_folder, "exec_summary_facts")

if (!dir.exists(exec_output_folder)) {
  dir.create(exec_output_folder, recursive = TRUE)
}

######################
# Helper functions
######################
# Note: extract_standard_ref, clean_standard_name, pct, safe_first_non_missing,
# write_exec_csv and write_exec_xlsx are already defined in section 01 above.
# They are intentionally NOT redefined here to avoid two diverging copies in
# the same script. (The original script redefined them a second time in this
# section; that redundancy has been removed.)

######################
# 1. Clean observable standards denominator
######################

# Collapse to one row per standard. This avoids counting standards multiple
# times where a standard appears across more than one detailed level.
observable_standards_latest <- standards_base %>%
  mutate(
    standard_ref = extract_standard_ref(std_fwk_name_stcode),
    standard_name_clean = clean_standard_name(std_fwk_name_stcode)
  ) %>%
  filter(
    !is.na(standard_ref),
    !is.na(starts),
    starts > 0
  ) %>%
  group_by(
    standard_ref,
    standard_name_clean
  ) %>%
  summarise(
    std_fwk_name_stcode = safe_first_non_missing(std_fwk_name_stcode),
    route_xplore = safe_first_non_missing(route),
    levels_xplore = paste(sort(unique(apps_level_detailed)), collapse = "; "),
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    skills_england_standards %>%
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
      distinct(standard_ref, .keep_all = TRUE) %>%
      select(
        standard_ref,
        skills_standard_name = standard_name,
        route,
        level,
        status,
        is_active,
        is_available_for_starts,
        max_funding,
        typical_duration,
        regulated_standard,
        integrated_degree
      ),
    by = "standard_ref"
  ) %>%
  mutate(
    route_final = coalesce(route, route_xplore)
  )

observable_denominator <- nrow(observable_standards_latest)

observable_standards_check <- observable_standards_latest %>%
  summarise(
    observable_standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    matched_to_skills_england = sum(!is.na(status)),
    skills_england_match_rate = pct(matched_to_skills_england, observable_standards),
    missing_status = sum(is.na(status))
  )

observable_standards_check

######################
# 2. Standards-volume figures for executive summary
######################

volume_thresholds <- c(50, 100, 200, 250)

standards_volume_summary <- tibble(
  threshold = volume_thresholds
) %>%
  mutate(
    statement = paste0("Fewer than ", threshold, " starts"),
    standards = map_int(
      threshold,
      ~ sum(observable_standards_latest$starts < .x, na.rm = TRUE)
    ),
    denominator = observable_denominator,
    pct_standards = pct(standards, denominator),
    denominator_note = "Observable standards with positive starts in the cleaned latest-year standards data"
  ) %>%
  select(
    statement,
    threshold,
    standards,
    denominator,
    pct_standards,
    denominator_note
  )

standards_volume_summary

write_exec_csv(
  standards_volume_summary,
  "table_standards_volume_summary.csv"
)

######################
# 3. Current thin-market quadrant numbers
######################

market_type_levels <- c(
  "High starts + many providers",
  "High starts + few providers",
  "Low starts + many providers",
  "Low starts + few providers"
)

thin_market_current_summary <- thin_market_funding %>%
  mutate(
    market_type = factor(market_type, levels = market_type_levels)
  ) %>%
  count(market_type, name = "standards") %>%
  complete(
    market_type = factor(market_type_levels, levels = market_type_levels),
    fill = list(standards = 0)
  ) %>%
  mutate(
    denominator = sum(standards),
    pct_standards = pct(standards, denominator),
    starts_threshold_used = starts_threshold,
    providers_threshold_used = providers_threshold,
    denominator_note = "Matched standards in the current thin-market quadrant dataset"
  )

thin_market_current_summary

write_exec_csv(
  thin_market_current_summary,
  "table_thin_market_current_quadrants.csv"
)

######################
# 4. Thin-market sensitivity table
######################

# Sensitivity is run on thin_market_funding because this is the current
# quadrant dataset with starts, providers and Skills England metadata attached.

thin_market_sensitivity_base <- thin_market_funding %>%
  filter(
    !is.na(starts),
    !is.na(providers)
  )

median_starts_threshold <- median(thin_market_sensitivity_base$starts, na.rm = TRUE)
median_providers_threshold <- median(thin_market_sensitivity_base$providers, na.rm = TRUE)

starts_threshold_options <- tibble(
  starts_threshold_label = c("Median", "<50", "<100", "<200", "<250"),
  starts_threshold_value = c(
    median_starts_threshold,
    50,
    100,
    200,
    250
  )
)

provider_threshold_options <- tibble(
  provider_threshold_label = c("Median", "<5 providers", "<10 providers"),
  provider_threshold_value = c(
    median_providers_threshold,
    5,
    10
  )
)

classify_quadrants <- function(df, starts_threshold_value, provider_threshold_value) {
  df %>%
    mutate(
      starts_group = if_else(
        starts < starts_threshold_value,
        "Low starts",
        "High starts"
      ),
      provider_group = if_else(
        providers < provider_threshold_value,
        "Few providers",
        "Many providers"
      ),
      market_type = case_when(
        starts_group == "High starts" & provider_group == "Many providers" ~ "High starts + many providers",
        starts_group == "High starts" & provider_group == "Few providers" ~ "High starts + few providers",
        starts_group == "Low starts" & provider_group == "Many providers" ~ "Low starts + many providers",
        starts_group == "Low starts" & provider_group == "Few providers" ~ "Low starts + few providers",
        TRUE ~ NA_character_
      ),
      market_type = factor(market_type, levels = market_type_levels)
    )
}

thin_market_sensitivity <- crossing(
  starts_threshold_options,
  provider_threshold_options
) %>%
  mutate(
    data = map2(
      starts_threshold_value,
      provider_threshold_value,
      ~ classify_quadrants(
        thin_market_sensitivity_base,
        starts_threshold_value = .x,
        provider_threshold_value = .y
      )
    )
  ) %>%
  select(
    starts_threshold_label,
    starts_threshold_value,
    provider_threshold_label,
    provider_threshold_value,
    data
  ) %>%
  unnest(data) %>%
  count(
    starts_threshold_label,
    starts_threshold_value,
    provider_threshold_label,
    provider_threshold_value,
    market_type,
    name = "standards"
  ) %>%
  group_by(
    starts_threshold_label,
    starts_threshold_value,
    provider_threshold_label,
    provider_threshold_value
  ) %>%
  complete(
    market_type = factor(market_type_levels, levels = market_type_levels),
    fill = list(standards = 0)
  ) %>%
  mutate(
    denominator = sum(standards),
    pct_standards = pct(standards, denominator)
  ) %>%
  ungroup() %>%
  arrange(
    starts_threshold_value,
    provider_threshold_value,
    market_type
  )

thin_market_sensitivity

write_exec_csv(
  thin_market_sensitivity,
  "table_thin_market_quadrant_sensitivity.csv"
)

######################
# 5. Test whether "majority are thin markets" is accurate
######################

majority_claims_summary <- bind_rows(
  tibble(
    statement = "Observable standards below 50 starts",
    standards = sum(observable_standards_latest$starts < 50, na.rm = TRUE),
    denominator = observable_denominator,
    pct_standards = pct(standards, denominator),
    interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
    concept = "Low-volume standards"
  ),
  tibble(
    statement = "Observable standards below 100 starts",
    standards = sum(observable_standards_latest$starts < 100, na.rm = TRUE),
    denominator = observable_denominator,
    pct_standards = pct(standards, denominator),
    interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
    concept = "Low-volume standards"
  ),
  tibble(
    statement = "Observable standards below 200 starts",
    standards = sum(observable_standards_latest$starts < 200, na.rm = TRUE),
    denominator = observable_denominator,
    pct_standards = pct(standards, denominator),
    interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
    concept = "Low-volume standards"
  ),
  tibble(
    statement = "Observable standards below 250 starts",
    standards = sum(observable_standards_latest$starts < 250, na.rm = TRUE),
    denominator = observable_denominator,
    pct_standards = pct(standards, denominator),
    interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
    concept = "Low-volume standards"
  ),
  thin_market_funding %>%
    summarise(
      statement = "Standards in low-start + few-provider quadrant",
      standards = sum(market_type == "Low starts + few providers", na.rm = TRUE),
      denominator = n(),
      pct_standards = pct(standards, denominator),
      interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
      concept = "Thin-market quadrant"
    ),
  thin_market_funding %>%
    summarise(
      statement = "Standards in low-start + many-provider quadrant",
      standards = sum(market_type == "Low starts + many providers", na.rm = TRUE),
      denominator = n(),
      pct_standards = pct(standards, denominator),
      interpretation = if_else(pct_standards > 50, "Majority", "Not majority"),
      concept = "Dispersed low-volume market"
    )
)

majority_claims_summary

write_exec_csv(
  majority_claims_summary,
  "table_majority_claims_summary.csv"
)

######################
# 6. Standards proliferation by route and level
######################

technical_routes <- c(
  "Engineering and manufacturing",
  "Construction and the built environment",
  "Health and science",
  "Digital",
  "Transport and logistics",
  "Agriculture, environmental and animal care"
)

approved_standards_route_level <- skills_england_standards %>%
  filter(
    programme_type == "Apprenticeship standard",
    is_available_for_starts
  ) %>%
  mutate(
    technical_route_flag = route %in% technical_routes
  ) %>%
  group_by(route, level, technical_route_flag) %>%
  summarise(
    approved_standards = n_distinct(standard_ref),
    .groups = "drop"
  ) %>%
  arrange(desc(approved_standards))

observable_standards_route_level <- observable_standards_latest %>%
  mutate(
    technical_route_flag = route_final %in% technical_routes
  ) %>%
  group_by(route_final, level, technical_route_flag) %>%
  summarise(
    observable_standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    median_starts = median(starts, na.rm = TRUE),
    below_50 = sum(starts < 50, na.rm = TRUE),
    below_100 = sum(starts < 100, na.rm = TRUE),
    below_200 = sum(starts < 200, na.rm = TRUE),
    below_250 = sum(starts < 250, na.rm = TRUE),
    pct_below_50 = pct(below_50, observable_standards),
    pct_below_100 = pct(below_100, observable_standards),
    pct_below_200 = pct(below_200, observable_standards),
    pct_below_250 = pct(below_250, observable_standards),
    .groups = "drop"
  ) %>%
  arrange(desc(observable_standards))

route_volume_summary <- observable_standards_latest %>%
  mutate(
    technical_route_flag = route_final %in% technical_routes
  ) %>%
  group_by(route_final, technical_route_flag) %>%
  summarise(
    observable_standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    median_starts = median(starts, na.rm = TRUE),
    below_50 = sum(starts < 50, na.rm = TRUE),
    below_100 = sum(starts < 100, na.rm = TRUE),
    below_200 = sum(starts < 200, na.rm = TRUE),
    below_250 = sum(starts < 250, na.rm = TRUE),
    pct_below_50 = pct(below_50, observable_standards),
    pct_below_100 = pct(below_100, observable_standards),
    pct_below_200 = pct(below_200, observable_standards),
    pct_below_250 = pct(below_250, observable_standards),
    .groups = "drop"
  ) %>%
  arrange(desc(pct_below_250), desc(observable_standards))

approved_standards_route_level
observable_standards_route_level
route_volume_summary

write_exec_csv(
  approved_standards_route_level,
  "table_approved_standards_by_route_level.csv"
)

write_exec_csv(
  observable_standards_route_level,
  "table_observable_standards_by_route_level.csv"
)

write_exec_csv(
  route_volume_summary,
  "table_route_volume_summary.csv"
)

######################
# 7. Standards family / possible consolidation analysis
######################

standard_family_patterns <- tribble(
  ~family, ~pattern,
  "Maintenance / engineering maintenance", "maintenance|engineering maintenance|maintain",
  "Aerospace", "aerospace|aircraft|aviation",
  "Automotive", "automotive|motor vehicle|vehicle|autocare",
  "Manufacturing technician / manufacturing engineer", "manufacturing technician|manufacturing engineer|manufacturing|production engineer|process operative",
  "Electrical / electrotechnical", "electrical|electrotechnical|electrician|electricity|power",
  "Construction operative / construction technician", "construction operative|construction technician|construction|built environment|civil engineering|building services",
  "Installation / solar / retrofit", "installation|installer|solar|retrofit|heat pump|low carbon",
  "Digital / cyber / data", "digital|cyber|data|software|network|cloud|ai|artificial intelligence|machine learning",
  "Rail", "\\brail\\b|railway|train",
  "Nuclear", "nuclear"
)

standards_family_long <- observable_standards_latest %>%
  mutate(
    standard_text = str_to_lower(
      paste(
        standard_name_clean,
        route_final,
        sep = " "
      )
    )
  ) %>%
  crossing(standard_family_patterns) %>%
  filter(
    str_detect(standard_text, regex(pattern, ignore_case = TRUE))
  ) %>%
  select(
    family,
    pattern,
    standard_ref,
    standard_name_clean,
    route_final,
    level,
    starts,
    max_funding,
    regulated_standard,
    status
  )

# Optional unique provider counts by family from the 2024/25 provider-standard
# dashboard file, if that object exists in the current session.
if (exists("provider_standard_starts_keyed")) {
  family_provider_counts <- provider_standard_starts_keyed %>%
    mutate(
      standard_text = str_to_lower(standard_name)
    ) %>%
    crossing(standard_family_patterns) %>%
    filter(
      str_detect(standard_text, regex(pattern, ignore_case = TRUE)),
      !is.na(provider_ukprn),
      starts > 0
    ) %>%
    group_by(family) %>%
    summarise(
      unique_providers_202425 = n_distinct(provider_ukprn),
      provider_standard_starts_202425 = sum(starts, na.rm = TRUE),
      .groups = "drop"
    )
} else {
  family_provider_counts <- tibble(
    family = character(),
    unique_providers_202425 = integer(),
    provider_standard_starts_202425 = numeric()
  )
}

standards_family_summary <- standards_family_long %>%
  group_by(family) %>%
  summarise(
    standards = n_distinct(standard_ref),
    total_starts = sum(starts, na.rm = TRUE),
    median_starts = median(starts, na.rm = TRUE),
    below_50 = sum(starts < 50, na.rm = TRUE),
    below_100 = sum(starts < 100, na.rm = TRUE),
    below_200 = sum(starts < 200, na.rm = TRUE),
    below_250 = sum(starts < 250, na.rm = TRUE),
    pct_below_50 = pct(below_50, standards),
    pct_below_100 = pct(below_100, standards),
    pct_below_200 = pct(below_200, standards),
    pct_below_250 = pct(below_250, standards),
    median_funding = median(max_funding, na.rm = TRUE),
    example_titles = paste(
      head(sort(unique(standard_name_clean)), 8),
      collapse = "; "
    ),
    .groups = "drop"
  ) %>%
  left_join(
    family_provider_counts,
    by = "family"
  ) %>%
  arrange(desc(standards), desc(pct_below_250))

standards_family_summary

write_exec_csv(
  standards_family_long,
  "table_standards_family_long.csv"
)

write_exec_csv(
  standards_family_summary,
  "table_standards_family_summary.csv"
)

######################
# 8. Maintenance / engineering candidate family
######################

maintenance_engineering_terms <- paste(
  c(
    "maintenance",
    "engineering",
    "technician",
    "aerospace",
    "manufacturing",
    "mechanical",
    "electrical",
    "mechatronics",
    "automotive",
    "machining",
    "machinist",
    "fabrication",
    "welding",
    "robotics",
    "rail engineering"
  ),
  collapse = "|"
)

maintenance_engineering_candidate_standards <- thin_market_funding %>%
  mutate(
    standard_name_clean = clean_standard_name(std_fwk_name_stcode),
    candidate_match_terms = str_extract_all(
      str_to_lower(standard_name_clean),
      maintenance_engineering_terms
    ),
    candidate_match_terms = map_chr(
      candidate_match_terms,
      ~ paste(unique(.x), collapse = "; ")
    )
  ) %>%
  filter(
    str_detect(
      str_to_lower(
        paste(
          standard_name_clean,
          route,
          sep = " "
        )
      ),
      maintenance_engineering_terms
    )
  ) %>%
  select(
    standard_ref,
    standard_name_clean,
    candidate_match_terms,
    starts,
    providers,
    route,
    level,
    market_type,
    max_funding,
    status,
    regulated_standard,
    integrated_degree,
    typical_duration
  ) %>%
  arrange(
    starts,
    providers,
    route,
    level
  )

maintenance_engineering_candidate_standards

write_exec_csv(
  maintenance_engineering_candidate_standards,
  "table_maintenance_engineering_candidate_family.csv"
)

######################
# 9. Candidate review classification
######################

# Persistent low-start history, where available, using full-year standards data.
persistent_low_start_history <- standards_subject_base %>%
  filter(
    release_type == "Full year"
  ) %>%
  mutate(
    standard_ref = extract_standard_ref(std_fwk_name_stcode),
    standard_name_clean = clean_standard_name(std_fwk_name_stcode)
  ) %>%
  filter(
    !is.na(standard_ref),
    !is.na(starts)
  ) %>%
  group_by(standard_ref) %>%
  summarise(
    years_observed = n_distinct(time_period),
    years_below_50 = n_distinct(time_period[starts < 50]),
    years_below_100 = n_distinct(time_period[starts < 100]),
    latest_year_in_history = max(time_period, na.rm = TRUE),
    .groups = "drop"
  )

family_counts_by_standard <- standards_family_long %>%
  group_by(standard_ref) %>%
  summarise(
    matched_families = paste(sort(unique(family)), collapse = "; "),
    family_count_for_standard = n_distinct(family),
    .groups = "drop"
  )

candidate_review_list <- thin_market_funding %>%
  mutate(
    standard_name_clean = clean_standard_name(std_fwk_name_stcode)
  ) %>%
  left_join(
    persistent_low_start_history,
    by = "standard_ref"
  ) %>%
  left_join(
    family_counts_by_standard,
    by = "standard_ref"
  ) %>%
  mutate(
    matched_families = replace_na(matched_families, ""),
    family_count_for_standard = replace_na(family_count_for_standard, 0L),
    low_volume_250 = starts < 250,
    low_volume_100 = starts < 100,
    low_volume_50 = starts < 50,
    few_provider_fixed = providers < 5,
    strategic_route = route %in% technical_routes,
    high_funding = max_funding >= funding_p75,
    safety_or_regulated_proxy =
      regulated_standard == "Yes" |
      str_detect(
        str_to_lower(standard_name_clean),
        "nuclear|rail|aerospace|aircraft|electrical|electricity|power|gas|defence|marine|maritime|safety|clinical|medical|pharma|pharmaceutical"
      ),
    possible_overlap_family = family_count_for_standard > 0,
    persistent_low_50 = years_below_50 >= 3,
    candidate_review_category = case_when(
      low_volume_250 & (safety_or_regulated_proxy | high_funding) & strategic_route ~ "protect/steward",
      low_volume_250 & possible_overlap_family & family_count_for_standard > 0 ~ "merge/modularise",
      low_volume_250 & strategic_route & providers >= 5 ~ "aggregate",
      persistent_low_50 & low_volume_50 & few_provider_fixed & !strategic_route & !safety_or_regulated_proxy ~ "retire/withdraw candidate",
      TRUE ~ "investigate"
    ),
    classification_note = case_when(
      candidate_review_category == "protect/steward" ~ "Low-volume but strategic, safety/regulatory or high-funding proxy suggests stewardship rather than removal.",
      candidate_review_category == "merge/modularise" ~ "Low-volume and in a named occupational family; possible overlap should be reviewed qualitatively.",
      candidate_review_category == "aggregate" ~ "Low-volume but provider footprint suggests dispersed demand that may need aggregation.",
      candidate_review_category == "retire/withdraw candidate" ~ "Persistent very low starts, few providers, no clear strategic/safety proxy; review before drawing conclusions.",
      TRUE ~ "Insufficient evidence for a stronger rule-based classification."
    )
  ) %>%
  select(
    standard_ref,
    standard_name_clean,
    route,
    level,
    status,
    starts,
    providers,
    market_type,
    max_funding,
    regulated_standard,
    typical_duration,
    years_observed,
    years_below_50,
    years_below_100,
    matched_families,
    strategic_route,
    high_funding,
    safety_or_regulated_proxy,
    candidate_review_category,
    classification_note
  ) %>%
  arrange(
    factor(
      candidate_review_category,
      levels = c(
        "protect/steward",
        "aggregate",
        "merge/modularise",
        "investigate",
        "retire/withdraw candidate"
      )
    ),
    starts,
    providers
  )

candidate_review_summary <- candidate_review_list %>%
  count(candidate_review_category, name = "standards") %>%
  mutate(
    denominator = sum(standards),
    pct_standards = pct(standards, denominator)
  ) %>%
  arrange(desc(standards))

candidate_review_summary
candidate_review_list %>% print(n = 50, width = Inf)

write_exec_csv(
  candidate_review_summary,
  "table_candidate_review_summary.csv"
)

write_exec_csv(
  candidate_review_list,
  "table_candidate_review_list.csv"
)

######################
# 10. Scaled markets and observable institutional features
######################

scaled_markets_top_standards <- thin_market_funding %>%
  filter(
    market_type == "High starts + many providers"
  ) %>%
  mutate(
    standard_name_clean = clean_standard_name(std_fwk_name_stcode),
    manual_institutional_flag_needed = TRUE,
    manual_flag_note = "Recognition, union links, licence-to-practise and employer representative body strength are not observed in these datasets."
  ) %>%
  select(
    standard_ref,
    standard_name_clean,
    starts,
    providers,
    route,
    level,
    market_type,
    leavers,
    max_funding,
    regulated_standard,
    integrated_degree,
    manual_institutional_flag_needed,
    manual_flag_note
  ) %>%
  arrange(desc(starts)) %>%
  slice_head(n = 50)

scaled_markets_top_standards

write_exec_csv(
  scaled_markets_top_standards,
  "table_scaled_markets_top_standards.csv"
)

######################
# 11. IS8 and fragile-standards summaries under latest thresholds
######################

# Define IS8 proxy function here if it has not already been sourced.
if (!exists("assign_is8_proxy")) {
  assign_is8_proxy <- function(standard_name, route) {
    
    standard_lower <- str_to_lower(standard_name)
    route_lower <- str_to_lower(route)
    
    case_when(
      str_detect(
        standard_lower,
        "cyber|artificial intelligence|\\bai\\b|machine learning|software|data|digital|network|telecom|telecommunications|cloud|quantum|semiconductor|coding|programmer|systems engineer|ux|user experience"
      ) |
        str_detect(route_lower, "digital") ~ "Digital and Technologies",
      
      str_detect(
        standard_lower,
        "clinical|biotech|bioinformatics|pharma|pharmaceutical|laboratory|lab scientist|healthcare science|medical|genomics|nursing|midwife|dietitian|podiatrist|radiographer|physiotherapist|orthotist|prosthetist|paramedic"
      ) |
        str_detect(route_lower, "health and science") ~ "Life Sciences",
      
      str_detect(
        standard_lower,
        "nuclear|wind|hydrogen|low carbon|heat pump|heating|carbon|energy|power|utilities|gas network|electricity|electrical power|smart meter|water process|water network|environmental|sustainability|retrofit"
      ) ~ "Clean Energy",
      
      str_detect(
        standard_lower,
        "defence|defense|ordnance|munitions|explosives|aviation|aerospace|aircraft|air traffic|marine|maritime|naval|army|royal navy|royal air force|security|survival equipment"
      ) ~ "Defence",
      
      str_detect(
        standard_lower,
        "manufacturing|manufacturer|engineering|robot|robotics|automotive|aerospace|materials|composites|battery|machining|machinist|welding|welder|fabrication|metal|foundry|casting|toolmaker|maintenance technician|rail engineering|mechatronics|process operative"
      ) |
        str_detect(route_lower, "engineering and manufacturing") ~ "Advanced Manufacturing",
      
      str_detect(
        standard_lower,
        "finance|financial|investment|insurance|actuary|actuarial|mortgage|banking|pensions|tax|audit|accounting|accountancy|risk"
      ) |
        str_detect(route_lower, "legal, finance and accounting") ~ "Financial Services",
      
      str_detect(
        standard_lower,
        "creative|media|broadcast|game|gaming|animation|film|design|designer|advertising|visual|arts|curator|museum|gallery|archive"
      ) |
        str_detect(route_lower, "creative and design") ~ "Creative Industries",
      
      str_detect(
        standard_lower,
        "consultant|consulting|business analyst|management|manager|project|legal|solicitor|paralegal|hr|human resources|procurement|marketing|sales|operations|leadership"
      ) |
        str_detect(route_lower, "business and administration") ~ "Professional and Business Services",
      
      TRUE ~ "Other / not clearly IS8"
    )
  }
}

fragile_threshold_sensitivity <- tibble(
  starts_threshold = c(50, 100, 200, 250, median_starts_threshold)
) %>%
  mutate(
    starts_threshold_label = case_when(
      starts_threshold == median_starts_threshold ~ "Median starts threshold",
      TRUE ~ paste0("<", starts_threshold, " starts")
    ),
    fragile_data = map(
      starts_threshold,
      ~ thin_market_funding %>%
        mutate(
          high_funding = max_funding >= funding_p75,
          priority_route = route %in% technical_routes,
          level_3_5 = level %in% 3:5,
          few_providers = providers <= providers_threshold,
          is8_proxy_sector = assign_is8_proxy(std_fwk_name_stcode, route),
          is8_proxy_flag = is8_proxy_sector != "Other / not clearly IS8"
        ) %>%
        filter(
          priority_route,
          high_funding,
          starts < .x,
          few_providers
        )
    )
  )

fragile_threshold_summary <- fragile_threshold_sensitivity %>%
  mutate(
    potentially_fragile_standards = map_int(fragile_data, nrow),
    linked_to_is8_proxy = map_int(
      fragile_data,
      ~ sum(.x$is8_proxy_flag, na.rm = TRUE)
    )
  ) %>%
  select(
    starts_threshold_label,
    starts_threshold,
    potentially_fragile_standards,
    linked_to_is8_proxy
  )

fragile_is8_breakdown_by_threshold <- fragile_threshold_sensitivity %>%
  select(starts_threshold_label, starts_threshold, fragile_data) %>%
  unnest(fragile_data) %>%
  count(
    starts_threshold_label,
    starts_threshold,
    is8_proxy_sector,
    name = "standards"
  ) %>%
  group_by(starts_threshold_label, starts_threshold) %>%
  mutate(
    denominator = sum(standards),
    pct_standards = pct(standards, denominator)
  ) %>%
  ungroup() %>%
  arrange(starts_threshold, desc(standards))

fragile_current_is8_summary <- potentially_fragile_standards %>%
  mutate(
    is8_proxy_sector = assign_is8_proxy(std_fwk_name_stcode, route),
    is8_proxy_flag = is8_proxy_sector != "Other / not clearly IS8"
  ) %>%
  summarise(
    potentially_fragile_standards = n(),
    linked_to_is8_proxy = sum(is8_proxy_flag, na.rm = TRUE),
    pct_linked_to_is8_proxy = pct(linked_to_is8_proxy, potentially_fragile_standards)
  )

fragile_current_is8_breakdown <- potentially_fragile_standards %>%
  mutate(
    is8_proxy_sector = assign_is8_proxy(std_fwk_name_stcode, route)
  ) %>%
  count(is8_proxy_sector, name = "standards", sort = TRUE) %>%
  mutate(
    denominator = sum(standards),
    pct_standards = pct(standards, denominator)
  )

fragile_current_is8_summary
fragile_current_is8_breakdown
fragile_threshold_summary
fragile_is8_breakdown_by_threshold

write_exec_csv(
  fragile_current_is8_summary,
  "table_fragile_current_is8_summary.csv"
)

write_exec_csv(
  fragile_current_is8_breakdown,
  "table_fragile_current_is8_breakdown.csv"
)

write_exec_csv(
  fragile_threshold_summary,
  "table_fragile_threshold_sensitivity_summary.csv"
)

write_exec_csv(
  fragile_is8_breakdown_by_threshold,
  "table_fragile_threshold_sensitivity_is8_breakdown.csv"
)

######################
# 12. Executive summary facts table
######################

top_100_share <- observable_standards_latest %>%
  arrange(desc(starts)) %>%
  mutate(
    rank = row_number(),
    cumulative_starts = cumsum(starts),
    cumulative_share = cumulative_starts / sum(starts, na.rm = TRUE)
  ) %>%
  filter(rank == 100) %>%
  pull(cumulative_share) %>%
  `*`(100) %>%
  round(1)

engineering_low_volume_250 <- observable_standards_latest %>%
  summarise(
    standards = sum(starts < 250 & route_final == "Engineering and manufacturing", na.rm = TRUE),
    denominator = sum(starts < 250, na.rm = TRUE),
    pct = pct(standards, denominator)
  )

engineering_thin_market <- thin_market_funding %>%
  summarise(
    standards = sum(market_type == "Low starts + few providers" & route == "Engineering and manufacturing", na.rm = TRUE),
    denominator = sum(market_type == "Low starts + few providers", na.rm = TRUE),
    pct = pct(standards, denominator)
  )

exec_summary_facts <- bind_rows(
  tibble(
    metric = "Total observable standards with starts",
    value = observable_denominator,
    denominator = NA_real_,
    percent = NA_real_,
    source_object = "observable_standards_latest",
    denominator_note = "One row per standard with positive starts"
  ),
  tibble(
    metric = "Total starts across observable standards",
    value = sum(observable_standards_latest$starts, na.rm = TRUE),
    denominator = NA_real_,
    percent = NA_real_,
    source_object = "observable_standards_latest",
    denominator_note = "Sum of starts across one-row-per-standard denominator"
  ),
  standards_volume_summary %>%
    transmute(
      metric = statement,
      value = standards,
      denominator = denominator,
      percent = pct_standards,
      source_object = "observable_standards_latest",
      denominator_note
    ),
  tibble(
    metric = "Top 100 standards' share of starts",
    value = 100,
    denominator = observable_denominator,
    percent = top_100_share,
    source_object = "observable_standards_latest",
    denominator_note = "Top 100 ranked by starts"
  ),
  thin_market_current_summary %>%
    transmute(
      metric = paste0("Current quadrant: ", as.character(market_type)),
      value = standards,
      denominator = denominator,
      percent = pct_standards,
      source_object = "thin_market_funding",
      denominator_note
    ),
  tibble(
    metric = "Low-start + few-provider standards",
    value = sum(thin_market_funding$market_type == "Low starts + few providers", na.rm = TRUE),
    denominator = nrow(thin_market_funding),
    percent = pct(value, denominator),
    source_object = "thin_market_funding",
    denominator_note = "Current thin-market matched standards denominator"
  ),
  tibble(
    metric = "Potentially fragile standards",
    value = nrow(potentially_fragile_standards),
    denominator = nrow(thin_market_funding),
    percent = pct(value, denominator),
    source_object = "potentially_fragile_standards",
    denominator_note = "Rule-based fragile standards definition"
  ),
  tibble(
    metric = "Potentially fragile standards linked to IS8 proxy sectors",
    value = fragile_current_is8_summary$linked_to_is8_proxy,
    denominator = fragile_current_is8_summary$potentially_fragile_standards,
    percent = fragile_current_is8_summary$pct_linked_to_is8_proxy,
    source_object = "potentially_fragile_standards",
    denominator_note = "Rule-based IS8 proxy mapping"
  ),
  tibble(
    metric = "Engineering/manufacturing share of below-250 observable standards",
    value = engineering_low_volume_250$standards,
    denominator = engineering_low_volume_250$denominator,
    percent = engineering_low_volume_250$pct,
    source_object = "observable_standards_latest",
    denominator_note = "Standards below 250 starts"
  ),
  tibble(
    metric = "Engineering/manufacturing share of low-start + few-provider standards",
    value = engineering_thin_market$standards,
    denominator = engineering_thin_market$denominator,
    percent = engineering_thin_market$pct,
    source_object = "thin_market_funding",
    denominator_note = "Current low-start + few-provider quadrant"
  )
) %>%
  mutate(
    value = as.numeric(value),
    denominator = as.numeric(denominator),
    percent = as.numeric(percent)
  )

exec_summary_facts %>%
  print(n = Inf, width = Inf)

write_exec_csv(
  exec_summary_facts,
  "table_exec_summary_facts.csv"
)

######################
# 13. Excel workbook export
######################

exec_tables <- list(
  "standards_volume" = standards_volume_summary,
  "thin_market_current" = thin_market_current_summary,
  "thin_market_sensitivity" = thin_market_sensitivity,
  "majority_claims" = majority_claims_summary,
  "approved_route_level" = approved_standards_route_level,
  "observable_route_level" = observable_standards_route_level,
  "route_volume" = route_volume_summary,
  "family_summary" = standards_family_summary,
  "maintenance_engineering" = maintenance_engineering_candidate_standards,
  "candidate_review_summary" = candidate_review_summary,
  "candidate_review_list" = candidate_review_list,
  "scaled_markets_top" = scaled_markets_top_standards,
  "fragile_is8_current" = fragile_current_is8_breakdown,
  "fragile_thresholds" = fragile_threshold_summary,
  "exec_summary_facts" = exec_summary_facts
)

write_exec_xlsx(
  exec_tables,
  "exec_summary_facts_tables.xlsx"
)

################################################################################
# End executive summary facts section
################################################################################