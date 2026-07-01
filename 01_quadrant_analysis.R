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


#########################

# Tiered thin-market classification
# Based on ChatGPT/QA recommendation: three fixed-threshold tiers plus
# a provider-dependency category for high-starts/few-provider standards.

thin_market_tiered <- thin_market_base %>%
  filter(!is.na(providers)) %>%
  mutate(
    tier = case_when(
      # Severe: very fragile, likely below viable cohort scale
      starts < 20  & providers <= 2 ~ "Severe thin market",
      # Thin: strong coordination-risk group (primary headline definition)
      starts < 50  & providers <= 3 ~ "Thin market",
      # Potential: wider watchlist
      starts < 100 & providers <= 5 ~ "Potential thin market",
      # Provider dependency: demand exists but provision structurally exposed
      starts >= 250 & providers <= 3 ~ "Provider dependency market",
      # Low volume: not necessarily thin, but warrants review
      starts < 250 ~ "Low-volume review group",
      # Scaled and competitive
      TRUE ~ "Scaled market"
    ),
    # Order tiers for charts
    tier = factor(tier, levels = c(
      "Severe thin market",
      "Thin market",
      "Potential thin market",
      "Low-volume review group",
      "Provider dependency market",
      "Scaled market"
    ))
  )

thin_market_tier_summary <- thin_market_tiered %>%
  group_by(tier) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts, na.rm = TRUE),
    median_starts = median(starts, na.rm = TRUE),
    median_providers = median(providers, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_standards = round(100 * standards / sum(standards), 1),
    pct_starts = round(100 * total_starts / sum(total_starts), 1)
  )

thin_market_tier_summary

#############











######################
# Sensitivity analysis: starts threshold
# Holds provider threshold fixed at <=3 and sweeps starts threshold
######################

starts_sensitivity_results <- tibble(
  starts_threshold_value = c(20, 50, 75, 100, 150, 200)
) %>%
  mutate(
    starts_threshold_label = paste0("<", starts_threshold_value, " starts"),
    total_standards = nrow(thin_market_base %>% filter(!is.na(providers))),
    low_starts_standards = map_int(
      starts_threshold_value,
      ~ thin_market_base %>%
        filter(!is.na(providers)) %>%
        summarise(n = sum(starts < .x)) %>%
        pull(n)
    ),
    pct_low_starts = round(100 * low_starts_standards / total_standards, 1),
    low_starts_few_providers = map_int(
      starts_threshold_value,
      ~ thin_market_base %>%
        filter(!is.na(providers)) %>%
        summarise(n = sum(starts < .x & providers <= 3)) %>%
        pull(n)
    ),
    pct_low_starts_few_providers = round(
      100 * low_starts_few_providers / total_standards, 1
    )
  ) %>%
  select(
    starts_threshold_label,
    starts_threshold_value,
    low_starts_standards,
    pct_low_starts,
    low_starts_few_providers,
    pct_low_starts_few_providers,
    total_standards
  )

starts_sensitivity_results

######################
# Sensitivity analysis: provider threshold
# Holds starts threshold fixed at <50 and sweeps provider threshold
######################

providers_sensitivity_results <- tibble(
  providers_threshold_value = c(1, 2, 3, 4, 5, 7, 10)
) %>%
  mutate(
    providers_threshold_label = paste0("<=", providers_threshold_value, " providers"),
    total_standards = nrow(thin_market_base %>% filter(!is.na(providers))),
    few_provider_standards = map_int(
      providers_threshold_value,
      ~ thin_market_base %>%
        filter(!is.na(providers)) %>%
        summarise(n = sum(providers <= .x)) %>%
        pull(n)
    ),
    pct_few_providers = round(100 * few_provider_standards / total_standards, 1),
    low_starts_few_providers = map_int(
      providers_threshold_value,
      ~ thin_market_base %>%
        filter(!is.na(providers)) %>%
        summarise(n = sum(starts < 50 & providers <= .x)) %>%
        pull(n)
    ),
    pct_low_starts_few_providers = round(
      100 * low_starts_few_providers / total_standards, 1
    )
  ) %>%
  select(
    providers_threshold_label,
    providers_threshold_value,
    few_provider_standards,
    pct_few_providers,
    low_starts_few_providers,
    pct_low_starts_few_providers,
    total_standards
  )

providers_sensitivity_results

######################
# Chart 1: Tiered market summary
######################

p_thin_market_tiers <- thin_market_tier_summary %>%
  ggplot(aes(x = tier, y = standards)) +
  geom_col(
    fill = colour_primary,
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(standards, "\n(", pct_standards, "%)")),
    vjust = -0.3,
    size = 3.2,
    colour = colour_text,
    lineheight = 0.9
  ) +
  labs(
    title = "Apprenticeship standards by thin-market tier",
    subtitle = "England, 2024/25. Tiered classification using fixed starts and provider thresholds.",
    x = NULL,
    y = "Number of standards"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
  theme_illuminate() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))

p_thin_market_tiers

######################
# Chart 2: Starts threshold sensitivity
######################

p_starts_sensitivity <- starts_sensitivity_results %>%
  select(
    starts_threshold_label,
    starts_threshold_value,
    `Low starts (any providers)` = pct_low_starts,
    `Low starts + <=3 providers` = pct_low_starts_few_providers
  ) %>%
  pivot_longer(
    cols = c(`Low starts (any providers)`, `Low starts + <=3 providers`),
    names_to = "measure",
    values_to = "pct_standards"
  ) %>%
  mutate(
    starts_threshold_label = fct_reorder(
      starts_threshold_label,
      starts_threshold_value
    ),
    measure = factor(
      measure,
      levels = c("Low starts (any providers)", "Low starts + <=3 providers")
    )
  ) %>%
  ggplot(aes(x = starts_threshold_label, y = pct_standards, fill = measure)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(pct_standards, "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.2,
    colour = colour_text
  ) +
  scale_fill_manual(
    values = c(
      "Low starts (any providers)" = colour_secondary,
      "Low starts + <=3 providers" = colour_primary
    )
  ) +
  labs(
    title = "Sensitivity of thin-market classification to starts threshold",
    subtitle = "Provider threshold fixed at <=3. England, 2024/25.",
    x = "Starts threshold",
    y = "Share of standards",
    fill = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(legend.position = "right")

p_starts_sensitivity

######################
# Chart 3: Provider threshold sensitivity
######################

p_providers_sensitivity <- providers_sensitivity_results %>%
  select(
    providers_threshold_label,
    providers_threshold_value,
    `Few providers (any starts)` = pct_few_providers,
    `<50 starts + few providers` = pct_low_starts_few_providers
  ) %>%
  pivot_longer(
    cols = c(`Few providers (any starts)`, `<50 starts + few providers`),
    names_to = "measure",
    values_to = "pct_standards"
  ) %>%
  mutate(
    providers_threshold_label = fct_reorder(
      providers_threshold_label,
      providers_threshold_value
    ),
    measure = factor(
      measure,
      levels = c("Few providers (any starts)", "<50 starts + few providers")
    )
  ) %>%
  ggplot(aes(x = providers_threshold_label, y = pct_standards, fill = measure)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(pct_standards, "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.2,
    colour = colour_text
  ) +
  scale_fill_manual(
    values = c(
      "Few providers (any starts)" = colour_secondary,
      "<50 starts + few providers" = colour_primary
    )
  ) +
  labs(
    title = "Sensitivity of thin-market classification to provider threshold",
    subtitle = "Starts threshold fixed at <50. England, 2024/25.",
    x = "Provider threshold",
    y = "Share of standards",
    fill = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(legend.position = "right")

p_providers_sensitivity

######################
# Chart 4: Side-by-side median vs fixed
######################

p_quadrant_comparison <- bind_rows(
  thin_market_segments %>%
    mutate(method = "Median thresholds"),
  thin_market_segments_fixed %>%
    rename(market_type = market_type_fixed) %>%
    mutate(method = "Fixed (<50 starts, <=3 providers)")
) %>%
  mutate(
    market_type = factor(market_type, levels = c(
      "High starts + many providers",
      "High starts + few providers",
      "Low starts + many providers",
      "Low starts + few providers"
    )),
    method = factor(method, levels = c(
      "Median thresholds",
      "Fixed (<50 starts, <=3 providers)"
    ))
  ) %>%
  ggplot(aes(x = market_type, y = pct_standards, fill = method)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(pct_standards, "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.2,
    colour = colour_text
  ) +
  scale_fill_manual(
    values = c(
      "Median thresholds" = colour_secondary,
      "Fixed (<50 starts, <=3 providers)" = colour_primary
    )
  ) +
  labs(
    title = "How threshold choice affects the quadrant distribution",
    subtitle = "Share of matched standards by quadrant, England 2024/25",
    x = NULL,
    y = "Share of standards",
    fill = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 65),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(
    axis.text.x = element_text(angle = 15, hjust = 1),
    legend.position = "right"
  )

p_quadrant_comparison

######################
# Save charts
######################

ggsave(
  file.path(output_folder, "annex_sensitivity_tier_summary.png"),
  p_thin_market_tiers, width = 9, height = 5, dpi = 300
)
ggsave(
  file.path(output_folder, "annex_sensitivity_starts_threshold.png"),
  p_starts_sensitivity, width = 9, height = 5, dpi = 300
)
ggsave(
  file.path(output_folder, "annex_sensitivity_providers_threshold.png"),
  p_providers_sensitivity, width = 9, height = 5, dpi = 300
)
ggsave(
  file.path(output_folder, "annex_sensitivity_quadrant_comparison.png"),
  p_quadrant_comparison, width = 9, height = 5, dpi = 300
)

message("Sensitivity analysis complete. Run the Word doc script next.")


######################
# Tiered thin-market scatter plot
# Replaces the four-quadrant scatter in 03_visualisation.R
# Uses thin_market_tiered from the dashboard quadrant rebuild
######################

# Colour palette for six tiers - distinct and ordered by severity
tier_colours <- c(
  "Severe thin market"        = "#CC0000",
  "Thin market"               = "#E07B00",
  "Potential thin market"     = "#E8C400",
  "Low-volume review group"   = "#6BB5E8",
  "Provider dependency market"= "#8E44AD",
  "Scaled market"             = "#CCCCCC"
)

# Reference lines: primary fixed thresholds
fixed_starts_line    <- 50
fixed_providers_line <- 3

p_thin_market_tiered_scatter <- thin_market_tiered %>%
  # Plot scaled market last so it sits behind other tiers
  arrange(desc(tier)) %>%
  ggplot(
    aes(
      x     = providers,
      y     = starts,
      colour = tier,
      alpha  = tier
    )
  ) +
  # Fixed threshold lines (primary definition)
  geom_vline(
    xintercept = fixed_providers_line,
    linetype   = "dashed",
    colour     = "#444444",
    linewidth  = 0.5
  ) +
  geom_hline(
    yintercept = fixed_starts_line,
    linetype   = "dashed",
    colour     = "#444444",
    linewidth  = 0.5
  ) +
  # Median threshold lines (secondary reference)
  geom_vline(
    xintercept = providers_threshold,
    linetype   = "dotted",
    colour     = "#888888",
    linewidth  = 0.4
  ) +
  geom_hline(
    yintercept = starts_threshold,
    linetype   = "dotted",
    colour     = "#888888",
    linewidth  = 0.4
  ) +
  geom_point() +
  # Threshold line labels
  annotate(
    "label",
    x = max(thin_market_tiered$providers, na.rm = TRUE) * 0.75,
    y = fixed_starts_line,
    label    = "<50 starts",
    size     = 2.8,
    colour   = "#444444",
    fill     = "white",
    label.size = 0.15,
    label.r  = unit(0.1, "lines"),
    vjust    = -0.3
  ) +
  annotate(
    "label",
    x     = fixed_providers_line,
    y     = max(thin_market_tiered$starts, na.rm = TRUE) * 0.9,
    label = "<=3 providers",
    size  = 2.8,
    colour  = "#444444",
    fill  = "white",
    label.size = 0.15,
    label.r  = unit(0.1, "lines"),
    hjust = -0.1
  ) +
  scale_x_log10(
    breaks = c(1, 2, 3, 5, 10, 25, 50, 100, 250, 500)
  ) +
  scale_y_log10(
    breaks = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000),
    labels = scales::comma
  ) +
  scale_colour_manual(
    values = tier_colours,
    breaks = names(tier_colours)
  ) +
  scale_alpha_manual(
    values = c(
      "Severe thin market"         = 1.0,
      "Thin market"                = 1.0,
      "Potential thin market"      = 0.9,
      "Low-volume review group"    = 0.7,
      "Provider dependency market" = 1.0,
      "Scaled market"              = 0.4
    ),
    breaks = names(tier_colours)
  ) +
  labs(
    title    = "Apprenticeship standards by market tier",
    subtitle = "England, 2024/25. Dashed lines show primary thresholds (<50 starts, \u22643 providers); dotted lines show medians.",
    x        = "Number of active providers (log scale)",
    y        = "Annual starts (log scale)",
    colour   = NULL,
    size     = NULL,
    alpha    = NULL,
    caption  = "Source: 2024/25 provider-standard starts dashboard file. Each point is one standard.\nProvider dependency market: \u2265250 starts, \u22643 providers."
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 3, alpha = 1)),
    alpha  = "none"
  ) +
  theme_illuminate_line() +
  theme(
    plot.caption = element_text(size = 8, colour = colour_text, hjust = 0)
  )

p_thin_market_tiered_scatter

ggsave(
  filename = file.path(output_folder, "chart_thin_market_tiered_scatter.png"),
  plot     = p_thin_market_tiered_scatter,
  width    = 10,
  height   = 6.5,
  dpi      = 300
)



######################
# IS8 cross-tabulation against six-tier market classification
# Requires in memory:
#   thin_market_tiered    (from dashboard quadrant rebuild)
#   skills_england_standards
#   assign_is8_proxy()    (from 02_analysis.R or defined inline below)
######################

# Define assign_is8_proxy() if not already in memory
if (!exists("assign_is8_proxy")) {
  assign_is8_proxy <- function(standard_name, route) {
    standard_lower <- str_to_lower(standard_name)
    route_lower    <- str_to_lower(coalesce(route, ""))
    case_when(
      str_detect(standard_lower,
                 "cyber|artificial intelligence|\\bai\\b|machine learning|software|data|digital|network|telecom|cloud|quantum|semiconductor|coding|programmer|systems engineer|ux|user experience") |
        str_detect(route_lower, "digital") ~ "Digital and Technologies",
      str_detect(standard_lower,
                 "clinical|biotech|bioinformatics|pharma|pharmaceutical|laboratory|lab scientist|healthcare science|medical|genomics|nursing|midwife|dietitian|podiatrist|radiographer|physiotherapist|orthotist|prosthetist|paramedic") |
        str_detect(route_lower, "health and science") ~ "Life Sciences",
      str_detect(standard_lower,
                 "nuclear|wind|hydrogen|low carbon|heat pump|heating|carbon|energy|power|utilities|gas network|electricity|electrical power|smart meter|water process|water network|environmental|sustainability|retrofit") ~ "Clean Energy Industries",
      str_detect(standard_lower,
                 "defence|defense|ordnance|munitions|explosives|aviation|aerospace|aircraft|air traffic|marine|maritime|naval|army|royal navy|royal air force|security|survival equipment") ~ "Defence",
      str_detect(standard_lower,
                 "manufacturing|manufacturer|engineering|robot|robotics|automotive|aerospace|materials|composites|battery|machining|machinist|welding|welder|fabrication|metal|foundry|casting|toolmaker|maintenance technician|rail engineering|mechatronics|process operative") |
        str_detect(route_lower, "engineering and manufacturing") ~ "Advanced Manufacturing",
      str_detect(standard_lower,
                 "finance|financial|investment|insurance|actuary|actuarial|mortgage|banking|pensions|tax|audit|accounting|accountancy|risk") |
        str_detect(route_lower, "legal, finance and accounting") ~ "Financial Services",
      str_detect(standard_lower,
                 "creative|media|broadcast|game|gaming|animation|film|design|designer|advertising|visual|arts|curator|museum|gallery|archive") |
        str_detect(route_lower, "creative and design") ~ "Creative Industries",
      str_detect(standard_lower,
                 "consultant|consulting|business analyst|management|manager|project|legal|solicitor|paralegal|hr|human resources|procurement|marketing|sales|operations|leadership") |
        str_detect(route_lower, "business and administration") ~ "Professional and Business Services",
      TRUE ~ "Other / not clearly IS8"
    )
  }
}

######################
# 1. Join route from Skills England so assign_is8_proxy() has what it needs
######################

se_route_lookup <- skills_england_standards %>%
  filter(programme_type == "Apprenticeship standard", !is.na(standard_ref)) %>%
  arrange(standard_ref, desc(is_available_for_starts), desc(is_active), desc(version_number)) %>%
  distinct(standard_ref, .keep_all = TRUE) %>%
  select(standard_ref, route, level, max_funding, regulated_standard)

thin_market_tiered_is8 <- thin_market_tiered %>%
  left_join(se_route_lookup, by = "standard_ref") %>%
  mutate(
    is8_sector = assign_is8_proxy(std_fwk_name_stcode, route),
    is8_flag   = is8_sector != "Other / not clearly IS8"
  )

######################
# 2. Note on multi-sector assignment
# assign_is8_proxy() is a case_when() - each standard receives EXACTLY ONE
# sector (first matching rule wins). Standards are NOT counted multiple times.
# The order of rules means Defence < Clean Energy < Advanced Manufacturing etc
# can overlap conceptually (e.g. nuclear could be Defence or Clean Energy;
# it hits Clean Energy first). This is a known limitation of the proxy - see
# the IS8 multi-label sensitivity note in the QA log.
######################

multi_sector_note <- tibble(
  note = paste(
    "IS8 mapping uses a single first-match case_when().",
    "Each standard appears in exactly one sector.",
    "No deduplication needed. Conceptual overlaps exist",
    "(e.g. nuclear hits Clean Energy before Defence;",
    "aerospace hits Defence before Advanced Manufacturing).",
    "See IS8 proxy QA for multi-label sensitivity."
  )
)

######################
# 3. Headline replacement figure: severe + thin across all IS8 sectors
######################

headline_is8_thin <- thin_market_tiered_is8 %>%
  filter(
    is8_flag,
    tier %in% c("Severe thin market", "Thin market")
  ) %>%
  summarise(
    standards = n(),
    total_starts = sum(starts, na.rm = TRUE)
  )

headline_is8_thin

# The old figure of 58 came from:
# - NARTS 2023/24 provider counts (lagged, leaver-based)
# - Binary quadrant (below-median starts AND below-median providers)
# - Filtered by high funding (>= p75 of max_funding) AND priority route
# The new figure uses:
# - Dashboard 2024/25 provider counts (current, starts-based)
# - Tiered thresholds (<50 starts <=3 providers for thin; <20 starts <=2 for severe)
# - IS8 proxy applied to the full tiered population, no funding filter

######################
# 4. Cross-tabulation: IS8 sector x tier
######################

tier_levels <- c(
  "Severe thin market",
  "Thin market",
  "Potential thin market",
  "Low-volume review group",
  "Provider dependency market",
  "Scaled market"
)

is8_sectors_ordered <- c(
  "Advanced Manufacturing",
  "Clean Energy Industries",
  "Defence",
  "Digital and Technologies",
  "Life Sciences",
  "Financial Services",
  "Creative Industries",
  "Professional and Business Services"
)

is8_tier_crosstab <- thin_market_tiered_is8 %>%
  filter(is8_flag) %>%
  count(is8_sector, tier) %>%
  complete(
    is8_sector = is8_sectors_ordered,
    tier = factor(tier_levels, levels = tier_levels),
    fill = list(n = 0)
  ) %>%
  filter(is8_sector %in% is8_sectors_ordered) %>%
  mutate(
    is8_sector = factor(is8_sector, levels = is8_sectors_ordered),
    tier       = factor(tier, levels = tier_levels)
  ) %>%
  arrange(is8_sector, tier)

is8_tier_crosstab %>% print(n = Inf)

######################
# 5. Summary table (one row per IS8 sector)
######################

is8_summary_table <- thin_market_tiered_is8 %>%
  filter(is8_flag) %>%
  group_by(is8_sector) %>%
  summarise(
    total_standards          = n(),
    severe_thin              = sum(tier == "Severe thin market"),
    thin                     = sum(tier == "Thin market"),
    severe_plus_thin         = severe_thin + thin,
    potential_thin           = sum(tier == "Potential thin market"),
    provider_dependency      = sum(tier == "Provider dependency market"),
    low_volume_review        = sum(tier == "Low-volume review group"),
    scaled                   = sum(tier == "Scaled market"),
    .groups = "drop"
  ) %>%
  mutate(
    pct_severe_thin          = round(100 * severe_thin        / total_standards, 1),
    pct_thin                 = round(100 * thin               / total_standards, 1),
    pct_severe_plus_thin     = round(100 * severe_plus_thin   / total_standards, 1),
    pct_potential_thin       = round(100 * potential_thin     / total_standards, 1),
    pct_provider_dependency  = round(100 * provider_dependency/ total_standards, 1)
  ) %>%
  filter(is8_sector %in% is8_sectors_ordered) %>%
  mutate(is8_sector = factor(is8_sector, levels = is8_sectors_ordered)) %>%
  arrange(is8_sector) %>%
  select(
    is8_sector,
    total_standards,
    severe_thin,
    pct_severe_thin,
    thin,
    pct_thin,
    severe_plus_thin,
    pct_severe_plus_thin,
    potential_thin,
    pct_potential_thin,
    provider_dependency,
    pct_provider_dependency
  )

is8_summary_table %>% print(n = Inf, width = Inf)

######################
# 6. Cross-check against old Table A8 figures
# Old figures (NARTS binary quadrant, low-start + few-provider):
#   Advanced Manufacturing: 50
#   Clean Energy:           10
#   Defence:                 8
#   Digital Technologies:   17
######################

old_a8 <- tibble(
  is8_sector    = c("Advanced Manufacturing", "Clean Energy Industries",
                    "Defence", "Digital and Technologies"),
  old_a8_count  = c(50, 10, 8, 17),
  old_method    = "NARTS 2023/24, binary quadrant (below-median starts + below-median providers), IS8 proxy"
)

crosscheck <- is8_summary_table %>%
  select(is8_sector, total_standards, severe_plus_thin, pct_severe_plus_thin) %>%
  left_join(old_a8, by = "is8_sector") %>%
  mutate(
    difference = severe_plus_thin - coalesce(old_a8_count, NA_real_),
    pct_change = round(100 * difference / coalesce(old_a8_count, NA_real_), 1),
    explanation = case_when(
      is.na(old_a8_count) ~ "No old A8 figure to compare",
      severe_plus_thin > old_a8_count ~ paste0(
        "+", difference, " (", pct_change, "%). ",
        "New method uses 2024/25 starts (not 2023/24 leavers), ",
        "fixed thresholds (<50 starts, <=3 providers) vs below-median, ",
        "no high-funding filter, broader IS8 keyword matching."
      ),
      severe_plus_thin < old_a8_count ~ paste0(
        difference, " (", pct_change, "%). ",
        "Fixed threshold (<50, <=3) is tighter than median-based threshold; ",
        "some standards that were below-median on both axes are now above ",
        "the fixed threshold."
      ),
      TRUE ~ "No material change"
    )
  )

crosscheck %>% print(n = Inf, width = Inf)

######################
# 7. Totals row
######################

is8_totals <- is8_summary_table %>%
  summarise(
    is8_sector              = "ALL IS8 SECTORS (total)",
    total_standards         = sum(total_standards),
    severe_thin             = sum(severe_thin),
    pct_severe_thin         = round(100 * sum(severe_thin) / sum(total_standards), 1),
    thin                    = sum(thin),
    pct_thin                = round(100 * sum(thin) / sum(total_standards), 1),
    severe_plus_thin        = sum(severe_plus_thin),
    pct_severe_plus_thin    = round(100 * sum(severe_plus_thin) / sum(total_standards), 1),
    potential_thin          = sum(potential_thin),
    pct_potential_thin      = round(100 * sum(potential_thin) / sum(total_standards), 1),
    provider_dependency     = sum(provider_dependency),
    pct_provider_dependency = round(100 * sum(provider_dependency) / sum(total_standards), 1)
  )

is8_totals %>% print(width = Inf)

######################
# 8. Headline replacement note
######################

cat(paste0(
  "\n--- HEADLINE REPLACEMENT FIGURE ---\n",
  "Old figure: 58 'potentially fragile' standards linked to IS8 sectors.\n",
  "New figure: ", headline_is8_thin$standards,
  " standards in IS8-mapped sectors classified as severe thin market or thin market.\n\n",
  "Methodological differences:\n",
  "  Old: NARTS 2023/24 leavers data; binary below-median thresholds;\n",
  "       filtered to priority routes AND high funding bands (>= p75).\n",
  "  New: Dashboard 2024/25 starts data; fixed thresholds\n",
  "       (severe: <20 starts and <=2 providers; thin: <50 starts and <=3 providers);\n",
  "       no funding-band filter; IS8 proxy applied to all tiered standards.\n",
  "       Standards in provider dependency market (high starts, <=3 providers)\n",
  "       are reported separately and not included in this headline count.\n"
))





tier_examples <- thin_market_tiered_is8 %>%
  mutate(
    standard_name_clean = std_fwk_name_stcode %>%
      str_remove("\\s*\\(ST[0-9]+\\)$") %>%
      str_squish()
  ) %>%
  group_by(tier) %>%
  arrange(desc(starts), .by_group = TRUE) %>%
  slice_head(n = 20) %>%
  ungroup() %>%
  select(
    tier,
    standard_ref,
    standard_name_clean,
    route,
    level,
    starts,
    providers,
    max_funding,
    regulated_standard,
    is8_sector
  ) %>%
  arrange(tier, desc(starts))

tier_examples %>% print(n = Inf, width = Inf)

# Export
write_csv(
  tier_examples,
  file.path(output_folder, "table_tier_examples.csv")
)




######################
# Funding band and commercial viability analysis
# Testing the MakeUK hypothesis: thin markets are partly a provider
# viability problem, not just a demand aggregation problem.
# Requires in memory: thin_market_tiered_is8, colour_* from config.R,
#                     theme_illuminate(), theme_illuminate_line()
######################

library(tidyverse)
library(scales)

tier_order <- c(
  "Severe thin market",
  "Thin market",
  "Potential thin market",
  "Low-volume review group",
  "Provider dependency market",
  "Scaled market"
)

tier_colours_funding <- c(
  "Severe thin market"         = "#CC0000",
  "Thin market"                = "#E07B00",
  "Potential thin market"      = "#E8C400",
  "Low-volume review group"    = "#6BB5E8",
  "Provider dependency market" = "#8E44AD",
  "Scaled market"              = "#AAAAAA"
)

# Base dataset: exclude rows with missing funding or duration
funding_base <- thin_market_tiered_is8 %>%
  left_join(
    skills_england_standards %>%
      filter(programme_type == "Apprenticeship standard") %>%
      arrange(standard_ref, desc(is_available_for_starts), desc(version_number)) %>%
      distinct(standard_ref, .keep_all = TRUE) %>%
      select(standard_ref, typical_duration),
    by = "standard_ref"
  ) %>%
  filter(
    !is.na(max_funding),
    !is.na(starts),
    !is.na(providers)
  ) %>%
  mutate(
    tier = factor(tier, levels = tier_order),
    # Analysis 2: total market revenue proxy
    # = what all providers on this standard could earn collectively per year
    # (ceiling figure only - actual price may be below the funding band)
    total_market_revenue = max_funding * starts,
    # Revenue per provider (average)
    revenue_per_provider = total_market_revenue / providers,
    # Analysis 3: monthly funding rate per learner
    monthly_rate_per_learner = ifelse(
      !is.na(typical_duration) & typical_duration > 0,
      max_funding / typical_duration,
      NA_real_
    ),
    # Monthly revenue per provider (average learners per provider x monthly rate)
    learners_per_provider = starts / providers,
    monthly_revenue_per_provider = monthly_rate_per_learner * learners_per_provider,
    # Flag engineering/manufacturing for overlay
    is_engineering = str_detect(
      coalesce(route, ""),
      regex("engineering and manufacturing", ignore_case = TRUE)
    )
  )

######################
# Analysis 1: Funding band distribution by tier
######################

funding_by_tier <- funding_base %>%
  group_by(tier) %>%
  summarise(
    standards         = n(),
    median_funding    = median(max_funding, na.rm = TRUE),
    p25_funding       = quantile(max_funding, 0.25, na.rm = TRUE),
    p75_funding       = quantile(max_funding, 0.75, na.rm = TRUE),
    mean_funding      = round(mean(max_funding, na.rm = TRUE)),
    pct_above_18k     = round(100 * mean(max_funding >= 18000, na.rm = TRUE), 1),
    pct_above_21k     = round(100 * mean(max_funding >= 21000, na.rm = TRUE), 1),
    .groups = "drop"
  )

funding_by_tier %>% print(width = Inf)

# Chart 1: Median funding band by tier
p_funding_by_tier <- funding_by_tier %>%
  mutate(tier = fct_rev(factor(tier, levels = tier_order))) %>%
  ggplot(aes(x = tier, y = median_funding, fill = tier)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(ymin = p25_funding, ymax = p75_funding),
    width = 0.25, colour = "#444444", linewidth = 0.6
  ) +
  geom_text(
    aes(label = scales::dollar(median_funding, prefix = "£", accuracy = 1000)),
    hjust = -0.15, size = 3.2, colour = colour_text
  ) +
  scale_fill_manual(values = tier_colours_funding, guide = "none") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "£"),
    expand = expansion(mult = c(0, 0.25))
  ) +
  coord_flip() +
  labs(
    title    = "Maximum funding band by market tier",
    subtitle = "Median (bar) and interquartile range (whiskers). England, 2024/25.",
    x        = NULL,
    y        = "Maximum funding band",
    caption  = "Note: max_funding is the government funding ceiling per apprentice, not actual delivery cost."
  ) +
  theme_illuminate()

p_funding_by_tier

######################
# Analysis 2: Total market revenue proxy by tier
######################

revenue_by_tier <- funding_base %>%
  group_by(tier) %>%
  summarise(
    standards                    = n(),
    median_total_market_revenue  = median(total_market_revenue, na.rm = TRUE),
    p25_total_market_revenue     = quantile(total_market_revenue, 0.25, na.rm = TRUE),
    p75_total_market_revenue     = quantile(total_market_revenue, 0.75, na.rm = TRUE),
    median_revenue_per_provider  = median(revenue_per_provider, na.rm = TRUE),
    p25_revenue_per_provider     = quantile(revenue_per_provider, 0.25, na.rm = TRUE),
    p75_revenue_per_provider     = quantile(revenue_per_provider, 0.75, na.rm = TRUE),
    pct_below_100k_per_provider  = round(100 * mean(revenue_per_provider < 100000, na.rm = TRUE), 1),
    pct_below_50k_per_provider   = round(100 * mean(revenue_per_provider < 50000, na.rm = TRUE), 1),
    .groups = "drop"
  )

revenue_by_tier %>% print(width = Inf)

# Chart 2a: Median total market revenue by tier
p_market_revenue_by_tier <- revenue_by_tier %>%
  mutate(tier = fct_rev(factor(tier, levels = tier_order))) %>%
  ggplot(aes(x = tier, y = median_total_market_revenue / 1e6, fill = tier)) +
  geom_col(width = 0.7) +
  geom_text(
    aes(label = paste0("£", round(median_total_market_revenue / 1000), "k")),
    hjust = -0.15, size = 3.2, colour = colour_text
  ) +
  scale_fill_manual(values = tier_colours_funding, guide = "none") +
  scale_y_continuous(
    labels = function(x) paste0("£", x, "m"),
    expand = expansion(mult = c(0, 0.3))
  ) +
  coord_flip() +
  labs(
    title    = "Median total market revenue proxy by tier",
    subtitle = "Max funding band × annual starts. England, 2024/25.",
    x        = NULL,
    y        = "Median total market revenue (£ millions)",
    caption  = "Note: ceiling estimate only. Actual revenue depends on take-up of the full funding band."
  ) +
  theme_illuminate()

p_market_revenue_by_tier

# Chart 2b: Median revenue per provider by tier — the commercial viability test
p_revenue_per_provider_by_tier <- revenue_by_tier %>%
  mutate(tier = fct_rev(factor(tier, levels = tier_order))) %>%
  ggplot(aes(x = tier, y = median_revenue_per_provider, fill = tier)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(ymin = p25_revenue_per_provider, ymax = p75_revenue_per_provider),
    width = 0.25, colour = "#444444", linewidth = 0.6
  ) +
  geom_text(
    aes(label = paste0("£", round(median_revenue_per_provider / 1000), "k")),
    hjust = -0.15, size = 3.2, colour = colour_text
  ) +
  scale_fill_manual(values = tier_colours_funding, guide = "none") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "£"),
    expand = expansion(mult = c(0, 0.3))
  ) +
  coord_flip() +
  labs(
    title    = "Median annual revenue per provider by market tier",
    subtitle = "Max funding band × starts ÷ providers. England, 2024/25.",
    x        = NULL,
    y        = "Median annual revenue per provider",
    caption  = paste0(
      "Note: ceiling estimate. Revenue per provider in severe thin markets is ",
      "typically a small fraction of scaled markets, even where funding bands are higher."
    )
  ) +
  theme_illuminate()

p_revenue_per_provider_by_tier

######################
# Analysis 3: Monthly funding rate per learner and per provider
######################

monthly_by_tier <- funding_base %>%
  filter(!is.na(monthly_rate_per_learner)) %>%
  group_by(tier) %>%
  summarise(
    standards                          = n(),
    median_monthly_rate_per_learner    = median(monthly_rate_per_learner, na.rm = TRUE),
    median_monthly_revenue_per_provider= median(monthly_revenue_per_provider, na.rm = TRUE),
    median_learners_per_provider       = median(learners_per_provider, na.rm = TRUE),
    p25_monthly_revenue_per_provider   = quantile(monthly_revenue_per_provider, 0.25, na.rm = TRUE),
    p75_monthly_revenue_per_provider   = quantile(monthly_revenue_per_provider, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

monthly_by_tier %>% print(width = Inf)

# Chart 3: Monthly revenue per provider - the cash-flow viability picture
p_monthly_revenue_by_tier <- monthly_by_tier %>%
  mutate(tier = fct_rev(factor(tier, levels = tier_order))) %>%
  ggplot(aes(x = tier, y = median_monthly_revenue_per_provider, fill = tier)) +
  geom_col(width = 0.7) +
  geom_errorbar(
    aes(
      ymin = p25_monthly_revenue_per_provider,
      ymax = p75_monthly_revenue_per_provider
    ),
    width = 0.25, colour = "#444444", linewidth = 0.6
  ) +
  geom_text(
    aes(label = paste0("£", round(median_monthly_revenue_per_provider))),
    hjust = -0.15, size = 3.2, colour = colour_text
  ) +
  scale_fill_manual(values = tier_colours_funding, guide = "none") +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "£"),
    expand = expansion(mult = c(0, 0.3))
  ) +
  coord_flip() +
  labs(
    title    = "Estimated monthly funding revenue per provider by market tier",
    subtitle = "(Max funding ÷ typical duration) × (starts ÷ providers). England, 2024/25.",
    x        = NULL,
    y        = "Estimated monthly revenue per provider",
    caption  = paste0(
      "Note: ceiling estimate based on full take-up of funding band. ",
      "Providers with fewer learners receive proportionally less monthly income ",
      "regardless of the per-learner funding rate."
    )
  ) +
  theme_illuminate()

p_monthly_revenue_by_tier

######################
# Analysis 4: Double jeopardy scatter
# Low starts AND low (or high) funding — the commercial viability space
######################

# Reference lines
low_starts_line  <- 50
low_funding_line <- median(funding_base$max_funding, na.rm = TRUE)

# Label quadrant zones
quadrant_labels <- tibble(
  x     = c(8,    350,   8,    350),
  y     = c(8000, 8000,  22000, 22000),
  label = c(
    "Low volume\nLow funding\n(commercially weakest)",
    "High volume\nLow funding\n(volume may offset low rate)",
    "Low volume\nHigh funding\n(rate looks adequate\nbut cohort too small)",
    "High volume\nHigh funding\n(scaled and well-funded)"
  )
)

p_double_jeopardy <- funding_base %>%
  arrange(desc(tier)) %>%
  ggplot(aes(
    x      = starts,
    y      = max_funding,
    colour = tier,
    shape  = is_engineering
  )) +
  geom_vline(xintercept = low_starts_line,  linetype = "dashed", colour = "#666666", linewidth = 0.5) +
  geom_hline(yintercept = low_funding_line, linetype = "dashed", colour = "#666666", linewidth = 0.5) +
  geom_point(size = 1.8, alpha = 0.75) +
  annotate(
    "label",
    x = low_starts_line, y = max(funding_base$max_funding, na.rm = TRUE) * 0.97,
    label = "<50 starts", size = 2.6, colour = "#666666",
    fill = "white", label.size = 0.1, hjust = -0.1
  ) +
  annotate(
    "label",
    x = max(funding_base$starts, na.rm = TRUE) * 0.6,
    y = low_funding_line,
    label = paste0("Median funding: £", scales::comma(low_funding_line)),
    size = 2.6, colour = "#666666",
    fill = "white", label.size = 0.1, vjust = -0.3
  ) +
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000),
    labels = scales::comma
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "£"),
    breaks = seq(0, 30000, 5000)
  ) +
  scale_colour_manual(values = tier_colours_funding, breaks = tier_order) +
  scale_shape_manual(
    values = c("TRUE" = 17, "FALSE" = 16),
    labels = c("TRUE" = "Engineering and manufacturing", "FALSE" = "Other routes"),
    name   = NULL
  ) +
  labs(
    title    = "Commercial viability space: starts vs funding band by market tier",
    subtitle = "Each point is one standard. Triangles = engineering and manufacturing route. England, 2024/25.",
    x        = "Annual starts (log scale)",
    y        = "Maximum funding band",
    colour   = NULL,
    caption  = paste0(
      "Dashed lines show <50 starts threshold and median funding band (£",
      scales::comma(low_funding_line), "). ",
      "Standards in the bottom-left quadrant face the greatest commercial viability challenge."
    )
  ) +
  guides(
    colour = guide_legend(override.aes = list(size = 3, alpha = 1)),
    shape  = guide_legend(override.aes = list(size = 3, alpha = 1))
  ) +
  theme_illuminate_line()

p_double_jeopardy

######################
# Summary stats for the double jeopardy zone
######################

double_jeopardy_summary <- funding_base %>%
  mutate(
    low_volume  = starts < 50,
    low_funding = max_funding < low_funding_line,
    zone = case_when(
      low_volume  & low_funding  ~ "Low volume + low funding (double jeopardy)",
      low_volume  & !low_funding ~ "Low volume + high funding",
      !low_volume & low_funding  ~ "High volume + low funding",
      TRUE                       ~ "High volume + high funding (scaled)"
    )
  ) %>%
  group_by(zone) %>%
  summarise(
    standards              = n(),
    engineering_pct        = round(100 * mean(is_engineering, na.rm = TRUE), 1),
    median_starts          = median(starts),
    median_providers       = median(providers),
    median_funding         = median(max_funding),
    .groups = "drop"
  ) %>%
  arrange(desc(standards))

double_jeopardy_summary %>% print(width = Inf)

######################
# Save charts
######################

ggsave(file.path(output_folder, "chart_funding_by_tier.png"),
       p_funding_by_tier,              width = 9, height = 5.5, dpi = 300)
ggsave(file.path(output_folder, "chart_market_revenue_by_tier.png"),
       p_market_revenue_by_tier,       width = 9, height = 5.5, dpi = 300)
ggsave(file.path(output_folder, "chart_revenue_per_provider_by_tier.png"),
       p_revenue_per_provider_by_tier, width = 9, height = 5.5, dpi = 300)
ggsave(file.path(output_folder, "chart_monthly_revenue_by_tier.png"),
       p_monthly_revenue_by_tier,      width = 9, height = 5.5, dpi = 300)
ggsave(file.path(output_folder, "chart_double_jeopardy_scatter.png"),
       p_double_jeopardy,              width = 10, height = 6.5, dpi = 300)

message("Funding analysis complete.")




######################
# Geographic concentration by tier
# Recreates the starts-band table using tier classification instead
# Joins standard_region_concentration_proxy to thin_market_tiered by standard name
######################

# standard_region_concentration_proxy has standard_name (plain, from dashboard)
# thin_market_tiered has std_fwk_name_stcode (= first(standard_name) from dashboard)
# Both should be the same plain name — join on that

tier_region_concentration <- standard_region_concentration_proxy %>%
  filter(standard_or_framework == "Standard") %>%
  mutate(standard_name_clean = str_squish(standard_name)) %>%
  left_join(
    thin_market_tiered %>%
      mutate(standard_name_clean = str_squish(std_fwk_name_stcode)) %>%
      select(standard_name_clean, tier),
    by = "standard_name_clean"
  ) %>%
  mutate(
    tier = coalesce(tier, "Unmatched / not in tiered population"),
    tier = factor(tier, levels = c(tier_order, "Unmatched / not in tiered population")),
    concentrated_flag = top_1_region_share >= 75,
    dispersed_flag    = regions_with_estimated_starts >= 5 & top_1_region_share < 50
  )

# Match quality check
tier_region_match_check <- tier_region_concentration %>%
  summarise(
    total_standards    = n(),
    matched_to_tier    = sum(tier != "Unmatched / not in tiered population"),
    unmatched          = sum(tier == "Unmatched / not in tiered population"),
    match_rate         = round(100 * matched_to_tier / total_standards, 1)
  )

tier_region_match_check

# Summary table by tier — mirrors the starts-band table in the report
tier_region_summary <- tier_region_concentration %>%
  filter(tier != "Unmatched / not in tiered population") %>%
  group_by(tier) %>%
  summarise(
    standards                      = n(),
    median_estimated_starts        = round(median(estimated_starts, na.rm = TRUE)),
    median_regions                 = round(median(regions_with_estimated_starts, na.rm = TRUE), 1),
    median_top_1_region_share      = round(median(top_1_region_share, na.rm = TRUE), 1),
    median_top_3_region_share      = round(median(top_3_region_share, na.rm = TRUE), 1),
    pct_concentrated               = round(100 * mean(concentrated_flag, na.rm = TRUE), 1),
    pct_dispersed                  = round(100 * mean(dispersed_flag, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>%
  arrange(tier)

tier_region_summary %>% print(n = Inf, width = Inf)

# Chart: geographic concentration by tier
p_geo_concentration_by_tier <- tier_region_summary %>%
  filter(tier != "Provider dependency market") %>%  # small n, can distort
  mutate(tier = fct_rev(factor(tier, levels = tier_order))) %>%
  select(tier, pct_concentrated, pct_dispersed) %>%
  pivot_longer(
    cols      = c(pct_concentrated, pct_dispersed),
    names_to  = "metric",
    values_to = "pct"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_concentrated" ~ "Concentrated (75%+ in one region)",
      metric == "pct_dispersed"    ~ "Dispersed (5+ regions, <50% in top region)"
    ),
    metric = factor(metric, levels = c(
      "Concentrated (75%+ in one region)",
      "Dispersed (5+ regions, <50% in top region)"
    ))
  ) %>%
  ggplot(aes(x = tier, y = pct, fill = metric)) +
  geom_col(position = position_dodge(width = 0.75), width = 0.65) +
  geom_text(
    aes(label = paste0(pct, "%")),
    position = position_dodge(width = 0.75),
    hjust = -0.15, size = 3.0, colour = colour_text
  ) +
  scale_fill_manual(
    values = c(
      "Concentrated (75%+ in one region)"              = colour_primary,
      "Dispersed (5+ regions, <50% in top region)"     = colour_secondary
    )
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    labels = function(x) paste0(x, "%")
  ) +
  coord_flip() +
  labs(
    title    = "Geographic concentration of estimated delivery by market tier",
    subtitle = "Provider-region proxy, standards-only. England, 2024/25.",
    x        = NULL,
    y        = "Share of standards",
    fill     = NULL,
    caption  = paste0(
      "Note: estimated delivery geography using provider level-specific regional profiles. ",
      "Provider dependency market excluded (n=7). ",
      "Concentrated = 75%+ of estimated starts in one region. ",
      "Dispersed = 5+ regions with <50% in top region."
    )
  ) +
  theme_illuminate() +
  theme(legend.position = "right")

p_geo_concentration_by_tier

ggsave(file.path(output_folder, "chart_geo_concentration_by_tier.png"),
       p_geo_concentration_by_tier, width = 10, height = 5.5, dpi = 300)

message("Geographic concentration by tier complete.")




######################
# Institutional provider flag
#
# Some standards in thin-market and provider-dependency tiers appear fragile
# because they are delivered by public sector bodies or armed forces for their
# own workforce — not because of genuine market failure. These are
# employer-as-provider arrangements where commercial market logic does not
# apply in the same way.
#
# This block flags standards where the dominant provider (by starts) matches
# known institutional patterns, produces cleaned tier counts excluding them,
# and adds the flag to the examples table.
#
# Requires in memory:
#   provider_standard_starts_keyed  (dashboard file, with standard_ref)
#   thin_market_tiered_is8          (tiered classification with IS8 mapping)
######################

######################
# 1. Identify dominant provider per standard
######################

dominant_provider_per_standard <- provider_standard_starts_keyed %>%
  filter(!is.na(standard_ref), starts > 0) %>%
  group_by(standard_ref, provider_name) %>%
  summarise(provider_starts = sum(starts, na.rm = TRUE), .groups = "drop") %>%
  group_by(standard_ref) %>%
  arrange(desc(provider_starts), .by_group = TRUE) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  rename(
    dominant_provider_name   = provider_name,
    dominant_provider_starts = provider_starts
  )

######################
# 2. Institutional provider flag
#
# Patterns cover:
#   - NHS, hospital trusts, ambulance services, health bodies
#   - Armed forces (Army, Navy, RAF, MoD)
#   - Police forces
#   - Prison service / HMPPS
#   - Fire and rescue services
#   - Local government / councils (where they are dominant employer-providers)
#
# This is a keyword flag on the dominant provider name. It is a heuristic,
# not an exhaustive list. Check flagged standards manually before citing.
######################

institutional_pattern <- regex(
  paste(
    "NHS|NATIONAL HEALTH SERVICE|AMBULANCE|HOSPITAL|FOUNDATION TRUST|HEALTHCARE|",
    "ARMY|ROYAL NAVY|ROYAL AIR FORCE|MINISTRY OF DEFENCE|\\bRAF\\b|HM FORCES|",
    "POLICE|CONSTABULARY|",
    "HMPPS|PRISON|HER MAJESTY|HIS MAJESTY'S PRISON|",
    "FIRE AND RESCUE|FIRE SERVICE|",
    "METROPOLITAN POLICE|BRITISH TRANSPORT POLICE",
    sep = ""
  ),
  ignore_case = TRUE
)

dominant_provider_per_standard <- dominant_provider_per_standard %>%
  mutate(
    institutional_provider_flag = str_detect(
      dominant_provider_name,
      institutional_pattern
    ),
    institutional_provider_type = case_when(
      str_detect(dominant_provider_name,
                 regex("NHS|NATIONAL HEALTH SERVICE|AMBULANCE|HOSPITAL|FOUNDATION TRUST|HEALTHCARE",
                       ignore_case = TRUE)) ~ "NHS / health body",
      str_detect(dominant_provider_name,
                 regex("ARMY|ROYAL NAVY|ROYAL AIR FORCE|MINISTRY OF DEFENCE|\\bRAF\\b|HM FORCES",
                       ignore_case = TRUE)) ~ "Armed forces",
      str_detect(dominant_provider_name,
                 regex("POLICE|CONSTABULARY|BRITISH TRANSPORT POLICE",
                       ignore_case = TRUE)) ~ "Police",
      str_detect(dominant_provider_name,
                 regex("HMPPS|PRISON|HER MAJESTY|HIS MAJESTY'S PRISON",
                       ignore_case = TRUE)) ~ "Prison service",
      str_detect(dominant_provider_name,
                 regex("FIRE AND RESCUE|FIRE SERVICE", ignore_case = TRUE)) ~ "Fire service",
      TRUE ~ NA_character_
    )
  )

# Check: how many standards are flagged?
dominant_provider_per_standard %>%
  count(institutional_provider_flag, institutional_provider_type, sort = TRUE) %>%
  print(n = Inf)

######################
# 3. Join flag to tiered classification
######################

thin_market_tiered_is8 <- thin_market_tiered_is8 %>%
  left_join(
    dominant_provider_per_standard %>%
      select(standard_ref, dominant_provider_name, dominant_provider_starts,
             institutional_provider_flag, institutional_provider_type),
    by = "standard_ref"
  ) %>%
  mutate(
    institutional_provider_flag = replace_na(institutional_provider_flag, FALSE)
  )

# Spot check: which flagged standards are in thin / severe / provider dependency?
thin_market_tiered_is8 %>%
  filter(
    institutional_provider_flag,
    tier %in% c("Severe thin market", "Thin market", "Provider dependency market")
  ) %>%
  select(
    tier, std_fwk_name_stcode, starts, providers,
    dominant_provider_name, institutional_provider_type, is8_sector
  ) %>%
  arrange(tier, desc(starts)) %>%
  print(n = Inf, width = Inf)

######################
# 4. Tier summary: raw vs commercially meaningful counts
######################

tier_summary_raw <- thin_market_tiered_is8 %>%
  count(tier, name = "standards_all") %>%
  mutate(pct_all = round(100 * standards_all / sum(standards_all), 1))

tier_summary_commercial <- thin_market_tiered_is8 %>%
  filter(!institutional_provider_flag) %>%
  count(tier, name = "standards_commercial") %>%
  mutate(pct_commercial = round(100 * standards_commercial / sum(standards_commercial), 1))

tier_summary_institutional <- thin_market_tiered_is8 %>%
  filter(institutional_provider_flag) %>%
  count(tier, name = "standards_institutional")

tier_comparison <- tier_summary_raw %>%
  left_join(tier_summary_commercial,   by = "tier") %>%
  left_join(tier_summary_institutional, by = "tier") %>%
  mutate(
    standards_institutional = replace_na(standards_institutional, 0),
    tier = factor(tier, levels = c(
      "Severe thin market", "Thin market", "Potential thin market",
      "Low-volume review group", "Provider dependency market", "Scaled market"
    ))
  ) %>%
  arrange(tier)

tier_comparison %>% print(width = Inf)

######################
# 5. IS8 thin-market headline: commercially meaningful figure
######################

headline_is8_thin_commercial <- thin_market_tiered_is8 %>%
  filter(
    is8_flag,
    !institutional_provider_flag,
    tier %in% c("Severe thin market", "Thin market")
  ) %>%
  summarise(
    standards    = n(),
    total_starts = sum(starts, na.rm = TRUE)
  )

cat(paste0(
  "\n--- COMMERCIALLY MEANINGFUL IS8 THIN-MARKET FIGURE ---\n",
  "All IS8 thin/severe thin standards:         ", headline_is8_thin$standards, "\n",
  "Excluding institutional-provider standards:  ", headline_is8_thin_commercial$standards, "\n",
  "Institutional-provider standards removed:    ",
  headline_is8_thin$standards - headline_is8_thin_commercial$standards, "\n"
))

######################
# 6. Update examples table with institutional flag
######################

tier_examples_flagged <- tier_examples %>%
  left_join(
    dominant_provider_per_standard %>%
      select(standard_ref, dominant_provider_name,
             institutional_provider_flag, institutional_provider_type),
    by = "standard_ref"
  ) %>%
  mutate(
    institutional_provider_flag = replace_na(institutional_provider_flag, FALSE),
    delivery_context = case_when(
      institutional_provider_flag ~ paste0("Institutional (", institutional_provider_type, ")"),
      TRUE ~ "Open market"
    )
  )



# tier_examples_flagged <- tier_examples %>%
#   left_join(
#     dominant_provider_per_standard %>%
#       select(standard_ref, dominant_provider_name,
#              institutional_provider_flag, institutional_provider_type),
#     by = "standard_ref"
#   ) %>%
#   mutate(
#     institutional_provider_flag = replace_na(institutional_provider_flag, FALSE),
#     delivery_context = case_when(
#       institutional_provider_flag ~ paste0("Institutional (", institutional_provider_type, ")"),
#       TRUE ~ "Open market"
#     )
#   ) %>%
#   select(
#     tier, standard_ref, standard_name_clean, route, level,
#     starts, providers, max_funding, regulated_standard,
#     is8_sector, dominant_provider_name, delivery_context
#   )

# Print flagged examples in policy-relevant tiers
tier_examples_flagged %>%
  filter(
    tier %in% c("Severe thin market", "Thin market", "Provider dependency market"),
    institutional_provider_flag
  ) %>%
  select(tier, standard_name_clean, starts, providers,
         dominant_provider_name, delivery_context) %>%
  print(n = Inf, width = Inf)

write_csv(
  tier_examples_flagged,
  file.path(output_folder, "table_tier_examples_flagged.csv")
)

message("Institutional provider flagging complete.")

