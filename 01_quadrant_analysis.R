######################
# Thin-market quadrant: rebuilt on 2024/25 dashboard data (replaces NARTS)
######################
#
# Both axes (starts and provider count) come from provider_standard_starts_keyed
# (2024_25-starts---subjects-and-standards.csv), confirmed full-year 2024/25
# (353,503 starts vs 353,500 official - rounding noise only).
#
# standard_ref is obtained by joining to skills_england_standards on
# lower-cased name, after deduplicating Skills England to one row per name
# (three standards had two versions; latest/active version kept).
# 38 unmatched names (1.3% of starts) are frameworks or minor name variants
# and are dropped - same intent as the previous NARTS logic.

# 1. Build name -> standard_ref lookup from Skills England, deduplicated.
skills_england_names <- skills_england_standards %>%
  filter(programme_type == "Apprenticeship standard") %>%
  mutate(name_lower = str_to_lower(str_squish(standard_name))) %>%
  arrange(
    name_lower,
    desc(is_available_for_starts),
    desc(is_active),
    desc(version_number)
  ) %>%
  distinct(name_lower, .keep_all = TRUE) %>%
  select(name_lower, standard_ref)

# 2. Add standard_ref to provider_standard_starts_keyed via name join.
# Drop the existing standard_ref column first (was all NA from a broken
# str_extract attempt earlier in the script).
provider_standard_starts_keyed <- provider_standard_starts_keyed %>%
  select(-standard_ref) %>%
  mutate(name_lower = str_to_lower(str_squish(standard_name))) %>%
  left_join(skills_england_names, by = "name_lower") %>%
  select(-name_lower)

# 3. Standard-level starts: one row per standard_ref, summed across all
# providers. Frameworks and unmatched names (1.3% of starts) dropped via
# filter(!is.na(standard_ref)).
standards_202425_dashboard_keyed <- provider_standard_starts_keyed %>%
  filter(!is.na(standard_ref)) %>%
  group_by(standard_ref) %>%
  summarise(
    std_fwk_name_stcode = first(standard_name),
    ssa_tier_1 = first(ssa_tier_1),
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  )

# Integrity check
standards_202425_duplicate_check <- standards_202425_dashboard_keyed %>%
  count(standard_ref, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(standards_202425_duplicate_check) > 0) {
  warning(nrow(standards_202425_duplicate_check), " non-unique standard_ref(s) in standards_202425_dashboard_keyed.")
} else {
  message("standards_202425_dashboard_keyed: standard_ref unique. OK.")
}

standards_202425_dashboard_keyed %>%
  summarise(standards = n(), total_starts = sum(starts, na.rm = TRUE))

# 4. Provider count by standard: distinct providers per standard_ref.
providers_per_standard_202425 <- provider_standard_starts_keyed %>%
  filter(
    !is.na(standard_ref),
    !is.na(provider_ukprn),
    starts > 0
  ) %>%
  group_by(standard_ref) %>%
  summarise(
    providers = n_distinct(provider_ukprn),
    .groups = "drop"
  )

# Integrity check
providers_202425_duplicate_check <- providers_per_standard_202425 %>%
  count(standard_ref, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(providers_202425_duplicate_check) > 0) {
  warning(nrow(providers_202425_duplicate_check), " non-unique standard_ref(s) in providers_per_standard_202425.")
} else {
  message("providers_per_standard_202425: standard_ref unique. OK.")
}

# 5. Join starts to provider counts.
n_before <- nrow(standards_202425_dashboard_keyed)

thin_market_base <- standards_202425_dashboard_keyed %>%
  left_join(providers_per_standard_202425, by = "standard_ref")

if (nrow(thin_market_base) != n_before) {
  warning("Row count changed during join: ", n_before, " before, ", nrow(thin_market_base), " after.")
}

# 6. Match quality check.
thin_market_match_quality <- thin_market_base %>%
  summarise(
    standards = n(),
    matched_provider_data = sum(!is.na(providers)),
    unmatched_provider_data = sum(is.na(providers)),
    match_rate = round(100 * matched_provider_data / standards, 1)
  )

thin_market_match_quality

# 7. Quadrant classification on median thresholds.
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
    starts_group = if_else(starts < starts_threshold, "Low starts", "High starts"),
    provider_group = if_else(providers < providers_threshold, "Few providers", "Many providers"),
    market_type = case_when(
      starts_group == "High starts" & provider_group == "Many providers" ~ "High starts + many providers",
      starts_group == "High starts" & provider_group == "Few providers"  ~ "High starts + few providers",
      starts_group == "Low starts"  & provider_group == "Many providers" ~ "Low starts + many providers",
      starts_group == "Low starts"  & provider_group == "Few providers"  ~ "Low starts + few providers"
    )
  )

thin_market_segments <- thin_market_summary %>%
  count(market_type, name = "standards") %>%
  mutate(pct_standards = round(100 * standards / sum(standards), 1))

thin_market_segments

# Fixed-threshold quadrant (<50 starts, <=3 providers)
starts_threshold_fixed <- 50
providers_threshold_fixed <- 3

thin_market_summary_fixed <- thin_market_base %>%
  filter(!is.na(providers)) %>%
  mutate(
    starts_group = if_else(starts < starts_threshold_fixed, "Low starts", "High starts"),
    provider_group = if_else(providers <= providers_threshold_fixed, "Few providers", "Many providers"),
    market_type_fixed = case_when(
      starts_group == "High starts" & provider_group == "Many providers" ~ "High starts + many providers",
      starts_group == "High starts" & provider_group == "Few providers"  ~ "High starts + few providers",
      starts_group == "Low starts"  & provider_group == "Many providers" ~ "Low starts + many providers",
      starts_group == "Low starts"  & provider_group == "Few providers"  ~ "Low starts + few providers"
    )
  )

thin_market_segments_fixed <- thin_market_summary_fixed %>%
  count(market_type_fixed, name = "standards") %>%
  mutate(pct_standards = round(100 * standards / sum(standards), 1))

thin_market_segments_fixed

# Side-by-side comparison
bind_rows(
  thin_market_segments %>% mutate(method = "Median"),
  thin_market_segments_fixed %>% rename(market_type = market_type_fixed) %>% mutate(method = "Fixed (<50 starts, <=3 providers)")
) %>%
  select(method, market_type, standards, pct_standards) %>%
  arrange(market_type, method)














