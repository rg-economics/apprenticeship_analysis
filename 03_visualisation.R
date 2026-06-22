################################################################################
## Apprenticeship Analysis
## 03_visualisation.R
##
## Covers:
##   - Visualisation theme definitions (theme_illuminate, _line, _map)
##   - SSA tier-1 short-label lookup vector
##   - All ggplot() chart objects (~40 charts)
##   - Industry analysis charts (IS8 proxy sectors, provider type by level,
##     spatial spread of technical starts, fragile-standard LAD maps,
##     LSIP thin-market maps, Industrial Strategy cluster overlays)
##   - All ggsave() calls
##   - All CSV write calls
##
## Depends on:
##   - config.R         (sourced below — colours, output_folder)
##   - 01_exploratory.R (run first — produces output_folder/data/*.rds)
##   - 02_analysis.R    (run second — produces map objects in output_folder/data/)
##
## Run order: config.R is sourced automatically; run 01 then 02 before this file.
################################################################################

source("config.R")
library(tidyverse)
library(sf)
library(scales)

# Load all analytical objects from 01_exploratory.R
data_path <- file.path("data")

standards_base                               <- readRDS(file.path(data_path, "standards_base.rds"))
standards_subject_trends                     <- readRDS(file.path(data_path, "standards_subject_trends.rds"))
standards_concentration_trends               <- readRDS(file.path(data_path, "standards_concentration_trends.rds"))
standards_dist                               <- readRDS(file.path(data_path, "standards_dist.rds"))
low_start_subject_share_trends               <- readRDS(file.path(data_path, "low_start_subject_share_trends.rds"))
route_summary                                <- readRDS(file.path(data_path, "route_summary.rds"))
route_compare                                <- readRDS(file.path(data_path, "route_compare.rds"))
providers_base                               <- readRDS(file.path(data_path, "providers_base.rds"))
providers_dist                               <- readRDS(file.path(data_path, "providers_dist.rds"))
provider_concentration                       <- readRDS(file.path(data_path, "provider_concentration.rds"))
provider_concentration_trends                <- readRDS(file.path(data_path, "provider_concentration_trends.rds"))
provider_subject_concentration               <- readRDS(file.path(data_path, "provider_subject_concentration.rds"))
thin_market_summary                          <- readRDS(file.path(data_path, "thin_market_summary.rds"))
thin_market_funding                          <- readRDS(file.path(data_path, "thin_market_funding.rds"))
potentially_fragile_standards                <- readRDS(file.path(data_path, "potentially_fragile_standards.rds"))
age_trends                                   <- readRDS(file.path(data_path, "age_trends.rds"))
level_trends                                 <- readRDS(file.path(data_path, "level_trends.rds"))
higher_25_trend                              <- readRDS(file.path(data_path, "higher_25_trend.rds"))
lsip_base                                    <- readRDS(file.path(data_path, "lsip_base.rds"))
lsip_level_variation                         <- readRDS(file.path(data_path, "lsip_level_variation.rds"))
lsip_age_variation                           <- readRDS(file.path(data_path, "lsip_age_variation.rds"))
lsip_age_level_variation                     <- readRDS(file.path(data_path, "lsip_age_level_variation.rds"))
lsip_age_level_concentration                 <- readRDS(file.path(data_path, "lsip_age_level_concentration.rds"))
regional_engineering                         <- readRDS(file.path(data_path, "regional_engineering.rds"))
technical_standard_region_concentration_summary <- readRDS(file.path(data_path, "technical_standard_region_concentration_summary.rds"))
lad_concentration_by_quadrant                <- readRDS(file.path(data_path, "lad_concentration_by_quadrant.rds"))
same_lad_chart_data                          <- readRDS(file.path(data_path, "same_lad_chart_data.rds"))
funding_by_quadrant                          <- readRDS(file.path(data_path, "funding_by_quadrant.rds"))
historical_starts                            <- readRDS(file.path(data_path, "historical_starts.rds"))
starts_threshold                             <- readRDS(file.path(data_path, "starts_threshold.rds"))
providers_threshold                          <- readRDS(file.path(data_path, "providers_threshold.rds"))
lad_engineering_top_30                       <- readRDS(file.path(data_path, "lad_engineering_top_30.rds"))
standard_region_concentration_proxy          <- readRDS(file.path(data_path, "standard_region_concentration_proxy.rds"))
provider_type_detailed_by_level              <- readRDS(file.path(data_path, "provider_type_detailed_by_level.rds"))

# Load industry analysis objects from 02_analysis.R
is8_proxy_summary                            <- readRDS(file.path(data_path, "is8_proxy_summary.rds"))
potentially_fragile_standards_is8            <- readRDS(file.path(data_path, "potentially_fragile_standards_is8.rds"))
spatial_spread_table                         <- readRDS(file.path(data_path, "spatial_spread_table.rds"))
lad_engineering_concentration                <- readRDS(file.path(data_path, "lad_engineering_concentration.rds"))
lad_priority_concentration                   <- readRDS(file.path(data_path, "lad_priority_concentration.rds"))

# Load map objects from 02_analysis.R
lsip_map_base                                <- readRDS(file.path(data_path, "lsip_map_base.rds"))
lsip_under19_map_base                        <- readRDS(file.path(data_path, "lsip_under19_map_base.rds"))
lsip_intermediate_map_base                   <- readRDS(file.path(data_path, "lsip_intermediate_map_base.rds"))
region_engineering_map_base                  <- readRDS(file.path(data_path, "region_engineering_map_base.rds"))
lad_engineering_map_base                     <- readRDS(file.path(data_path, "lad_engineering_map_base.rds"))
lad_low_starts_few_providers_map_base        <- readRDS(file.path(data_path, "lad_low_starts_few_providers_map_base.rds"))
lad_low_starts_few_providers_share_map_base  <- readRDS(file.path(data_path, "lad_low_starts_few_providers_share_map_base.rds"))
lad_fragile_map_base                         <- readRDS(file.path(data_path, "lad_fragile_map_base.rds"))
lsip_thin_market_map_base                    <- readRDS(file.path(data_path, "lsip_thin_market_map_base.rds"))
is_clusters_sf                               <- readRDS(file.path(data_path, "is_clusters_sf.rds"))

# Shared helper (needed for academic_year labels in some charts)
make_academic_year <- function(time_period) {
  paste0(
    substr(as.character(time_period), 1, 4),
    "/",
    substr(as.character(time_period), 5, 6)
  )
}

######################
# SSA tier-1 short labels
######################

# Named vector used to shorten SSA tier-1 labels in chart pipelines.
# Replace the two inline case_when blocks (Charts 8 and 12) with
# recode(ssa_tier_1, !!!ssa_short_labels).
ssa_short_labels <- c(
  "Engineering and Manufacturing Technologies"        = "Engineering & manufacturing",
  "Construction, Planning and the Built Environment"  = "Construction & built environment",
  "Health, Public Services and Care"                  = "Health, public services & care",
  "Business, Administration and Law"                  = "Business, admin & law",
  "Agriculture, Horticulture and Animal Care"         = "Agriculture & animal care",
  "Retail and Commercial Enterprise"                  = "Retail & commercial enterprise",
  "Arts, Media and Publishing"                        = "Arts, media & publishing"
)

######################
# 14 Visualisation theme
######################

theme_illuminate <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      panel.border = element_rect(
        colour = "black",
        fill = NA,
        linewidth = 0.6
      ),
      plot.title = element_text(
        face = "bold",
        size = 14,
        colour = colour_text
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = colour_text
      ),
      axis.title = element_text(
        colour = colour_text
      ),
      axis.text = element_text(
        colour = colour_text
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = element_rect(
        fill = colour_panel,
        colour = NA
      ),
      legend.position = "bottom"
    )
}

# Use this for line charts with legends.
# It keeps legends on the right and helps line labels match chart order.
theme_illuminate_line <- function() {
  theme_illuminate() +
    theme(
      legend.position = "right"
    )
}

theme_illuminate_map <- function() {
  theme_void(base_size = 12) +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 14,
        colour = colour_text
      ),
      plot.subtitle = element_text(
        size = 11,
        colour = colour_text
      ),
      plot.caption = element_text(
        size = 9,
        colour = colour_text,
        hjust = 0
      ),
      legend.position = "right",
      legend.title = element_text(
        colour = colour_text
      ),
      legend.text = element_text(
        colour = colour_text
      ),
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      panel.background = element_rect(
        fill = "white",
        colour = NA
      )
    )
}

######################
# 15 Chart objects
######################

# Chart 1: Apprenticeship starts over time
p_historical_starts <- historical_starts %>%
  mutate(
    period_type = case_when(
      time_period <= 201617 ~ "Pre-levy",
      time_period >= 201718 ~ "Post-levy"
    )
  ) %>%
  ggplot(
    aes(x = academic_year, y = starts, group = 1)
  ) +
  geom_line(
    colour = colour_primary,
    linewidth = 1.2
  ) +
  geom_point(
    aes(fill = period_type),
    shape = 21,
    colour = "white",
    stroke = 0.7,
    size = 2.8
  ) +
  geom_vline(
    xintercept = which(historical_starts$academic_year == "2017/18") - 0.5,
    linetype = "dashed",
    colour = "#444444",
    linewidth = 0.6
  ) +
  annotate(
    "label",
    x = which(historical_starts$academic_year == "2011/12"),
    y = 540000,
    label = "Peak: 520,600",
    size = 3.3,
    colour = colour_text,
    fill = "white",
    label.size = 0.2
  ) +
  annotate(
    "label",
    x = which(historical_starts$academic_year == "2024/25"),
    y = 375000,
    label = "2024/25: 353,500",
    size = 3.3,
    colour = colour_text,
    fill = "white",
    label.size = 0.2
  ) +
  scale_fill_manual(
    values = c(
      "Pre-levy" = colour_secondary,
      "Post-levy" = colour_primary
    )
  ) +
  labs(
    title = "Apprenticeship starts remain below their pre-levy peak",
    subtitle = "England, academic years 2002/03 to 2024/25",
    x = "Academic year",
    y = "Starts",
    fill = NULL
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate_line() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

p_historical_starts


# Chart 2: Share of starts by age group over time
p_age_share_trend <- age_trends %>%
  mutate(
    age_summary = factor(
      age_summary,
      levels = c("25+", "19-24", "Under 19")
    )
  ) %>%
  ggplot(
    aes(
      x = academic_year,
      y = pct_starts,
      colour = age_summary,
      group = age_summary
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  scale_colour_manual(
    values = c(
      "25+" = colour_primary,
      "19-24" = colour_secondary,
      "Under 19" = colour_accent
    ),
    breaks = c("25+", "19-24", "Under 19")
  ) +
  labs(
    title = "Apprenticeship starts have shifted towards older learners",
    subtitle = "Share of starts by age group, England",
    x = "Academic year",
    y = "Share of starts",
    colour = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate_line()

p_age_share_trend


# Chart 3: Share of starts by apprenticeship level over time
p_level_share_trend <- level_trends %>%
  mutate(
    apps_level = factor(
      apps_level,
      levels = c(
        "Advanced Apprenticeship",
        "Higher Apprenticeship",
        "Intermediate Apprenticeship"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = academic_year,
      y = pct_starts,
      colour = apps_level,
      group = apps_level
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  scale_colour_manual(
    values = c(
      "Advanced Apprenticeship" = colour_primary,
      "Higher Apprenticeship" = colour_secondary,
      "Intermediate Apprenticeship" = colour_accent
    ),
    breaks = c(
      "Advanced Apprenticeship",
      "Higher Apprenticeship",
      "Intermediate Apprenticeship"
    )
  ) +
  labs(
    title = "Higher apprenticeships now account for almost 40% of starts",
    subtitle = "Share of starts by apprenticeship level, England",
    x = "Academic year",
    y = "Share of starts",
    colour = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 50),
    breaks = seq(0, 50, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate_line()

p_level_share_trend


# Chart 4: 25+ Higher apprenticeships as a share of all starts
p_higher_25_share_trend <- higher_25_trend %>%
  ggplot(
    aes(x = academic_year, y = pct_total_starts, group = 1)
  ) +
  geom_line(
    colour = colour_primary,
    linewidth = 1.2
  ) +
  geom_point(
    colour = colour_primary,
    size = 2.8
  ) +
  geom_text(
    aes(label = paste0(pct_total_starts, "%")),
    vjust = -0.8,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "25+ Higher apprenticeships have grown sharply",
    subtitle = "Share of all apprenticeship starts, England",
    x = "Academic year",
    y = "Share of all starts"
  ) +
  scale_y_continuous(
    limits = c(0, 35),
    breaks = seq(0, 35, 5),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate()

p_higher_25_share_trend


# Chart 5: Distribution of standards by annual starts
p_standards_distribution <- ggplot(
  standards_dist,
  aes(x = starts_band, y = standards)
) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = standards),
    vjust = -0.4,
    size = 4,
    colour = colour_text
  ) +
  labs(
    title = "Distribution of apprenticeship standards by annual starts",
    subtitle = "England, 2024/25",
    x = "Annual starts",
    y = "Number of standards"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate()

p_standards_distribution


# Chart 5b: Frequency distribution of starts per standard
# This shows the long-tail structure directly.
# X-axis is log-scaled because starts are highly skewed.

p_standards_frequency_log <- standards_base %>%
  ggplot(
    aes(x = starts)
  ) +
  geom_histogram(
    bins = 40,
    fill = colour_primary,
    colour = "white",
    linewidth = 0.2,
    alpha = 0.9
  ) +
  geom_vline(
    xintercept = 50,
    linetype = "dashed",
    colour = "#444444",
    linewidth = 0.7
  ) +
  geom_vline(
    xintercept = median(standards_base$starts, na.rm = TRUE),
    linetype = "dotted",
    colour = "#444444",
    linewidth = 0.7
  ) +
  annotate(
    "label",
    x = 50,
    y = Inf,
    label = "<50 starts threshold",
    vjust = 1.4,
    hjust = -0.05,
    size = 3.2,
    colour = colour_text,
    fill = "white",
    label.size = 0.2
  ) +
  annotate(
    "label",
    x = median(standards_base$starts, na.rm = TRUE),
    y = Inf,
    label = paste0("Median: ", median(standards_base$starts, na.rm = TRUE)),
    vjust = 3.2,
    hjust = -0.05,
    size = 3.2,
    colour = colour_text,
    fill = "white",
    label.size = 0.2
  ) +
  scale_x_log10(
    breaks = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000, 25000),
    labels = scales::comma
  ) +
  labs(
    title = "Starts per standard are highly skewed",
    subtitle = "Frequency distribution of annual starts by standard, England, 2024/25",
    x = "Annual starts per standard, log scale",
    y = "Number of standards"
  ) +
  theme_illuminate()

p_standards_frequency_log

# Chart 6: Concentration of starts in the largest standards over time
p_concentration_trend <- standards_concentration_trends %>%
  filter(
    release_type == "Full year",
    top_n_standards %in% c(10, 50, 100)
  ) %>%
  mutate(
    academic_year = make_academic_year(time_period),
    top_n_label = factor(
      paste0("Top ", top_n_standards),
      levels = c("Top 100", "Top 50", "Top 10")
    )
  ) %>%
  ggplot(
    aes(
      x = academic_year,
      y = cumulative_share,
      colour = top_n_label,
      group = top_n_label
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  scale_colour_manual(
    values = c(
      "Top 100" = colour_primary,
      "Top 50" = colour_secondary,
      "Top 10" = colour_accent
    ),
    breaks = c("Top 100", "Top 50", "Top 10")
  ) +
  labs(
    title = "Share of starts accounted for by the largest standards",
    subtitle = "England, full-year releases only",
    x = "Academic year",
    y = "Share of starts",
    colour = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 100),
    breaks = seq(0, 100, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate_line()

p_concentration_trend


# Chart 7: Share of standards with fewer than 50 starts over time
p_low_start_trend <- standards_subject_trends %>%
  filter(release_type == "Full year") %>%
  mutate(
    academic_year = make_academic_year(time_period)
  ) %>%
  ggplot(
    aes(x = academic_year, y = pct_low_start, group = 1)
  ) +
  geom_line(
    colour = colour_primary,
    linewidth = 1.2
  ) +
  geom_point(
    colour = colour_primary,
    size = 2.8
  ) +
  geom_text(
    aes(label = paste0(pct_low_start, "%")),
    vjust = -0.8,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "Share of standards with fewer than 50 annual starts",
    subtitle = "England, full-year releases only",
    x = "Academic year",
    y = "Share of standards"
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate()

p_low_start_trend


# Chart 8: Subject areas accounting for low-start standards over time
p_low_start_subject_trend <- low_start_subject_share_trends %>%
  mutate(
    ssa_tier_1_short = recode(ssa_tier_1, !!!ssa_short_labels),
    ssa_tier_1_short = factor(
      ssa_tier_1_short,
      levels = c(
        "Engineering & manufacturing",
        "Health, public services & care",
        "Construction & built environment",
        "Arts, media & publishing",
        "Agriculture & animal care"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = academic_year,
      y = pct_low_start,
      colour = ssa_tier_1_short,
      group = ssa_tier_1_short
    )
  ) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.3) +
  scale_colour_manual(
    values = c(
      "Engineering & manufacturing" = colour_primary,
      "Health, public services & care" = colour_accent,
      "Construction & built environment" = colour_secondary,
      "Arts, media & publishing" = "#6C6C6C",
      "Agriculture & animal care" = "#AFAFAF"
    ),
    breaks = c(
      "Engineering & manufacturing",
      "Health, public services & care",
      "Construction & built environment",
      "Arts, media & publishing",
      "Agriculture & animal care"
    )
  ) +
  labs(
    title = "Engineering and manufacturing consistently dominates the low-start tail",
    subtitle = "Share of standards with fewer than 50 starts by subject area, England",
    x = "Academic year",
    y = "Share of low-start standards",
    colour = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 45),
    breaks = seq(0, 45, 5),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate_line()

p_low_start_subject_trend


# Chart 9: Median starts per standard by route
p_median_starts_route <- route_summary %>%
  mutate(
    route = fct_reorder(route, median_starts)
  ) %>%
  ggplot(
    aes(x = route, y = median_starts)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = round(median_starts, 0)),
    hjust = -0.2,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Median annual starts per standard by route",
    subtitle = "England, 2024/25",
    x = NULL,
    y = "Median starts"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_median_starts_route


# Chart 10: Distribution of providers by annual starts
p_providers_distribution <- ggplot(
  providers_dist,
  aes(x = starts_band, y = providers)
) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = providers),
    vjust = -0.4,
    size = 4,
    colour = colour_text
  ) +
  labs(
    title = "Distribution of apprenticeship providers by annual starts",
    subtitle = "England, 2024/25",
    x = "Annual starts",
    y = "Number of providers"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate()

p_providers_distribution


# Chart 11: Provider concentration over time
p_provider_concentration_trend <- provider_concentration_trends %>%
  filter(
    release_type == "Full year",
    top_n_providers %in% c(10, 50, 100)
  ) %>%
  mutate(
    academic_year = make_academic_year(time_period),
    top_n_label = factor(
      paste0("Top ", top_n_providers),
      levels = c("Top 100", "Top 50", "Top 10")
    )
  ) %>%
  ggplot(
    aes(
      x = academic_year,
      y = cumulative_share,
      colour = top_n_label,
      group = top_n_label
    )
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.6) +
  scale_colour_manual(
    values = c(
      "Top 100" = colour_primary,
      "Top 50" = colour_secondary,
      "Top 10" = colour_accent
    ),
    breaks = c("Top 100", "Top 50", "Top 10")
  ) +
  labs(
    title = "Provider concentration has increased modestly",
    subtitle = "Share of starts accounted for by the largest providers, England",
    x = "Academic year",
    y = "Share of starts",
    colour = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate_line()

p_provider_concentration_trend


# Chart 12: Provider concentration by subject area
p_provider_subject_concentration <- provider_subject_concentration %>%
  filter(top_n_providers == 10) %>%
  mutate(
    ssa_tier_1_short = recode(ssa_tier_1, !!!ssa_short_labels),
    ssa_tier_1_short = fct_reorder(ssa_tier_1_short, cumulative_share)
  ) %>%
  ggplot(
    aes(x = ssa_tier_1_short, y = cumulative_share)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(cumulative_share, "%")),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Provider concentration varies substantially by subject area",
    subtitle = "Share of starts accounted for by the top 10 providers, England, 2024/25",
    x = NULL,
    y = "Share of subject starts"
  ) +
  scale_y_continuous(
    limits = c(0, 85),
    breaks = seq(0, 80, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate()

p_provider_subject_concentration


# Chart 13: Thin-market scatterplot
p_thin_market_scatter <- thin_market_summary %>%
  mutate(
    market_type = factor(
      market_type,
      levels = c(
        "High starts + many providers",
        "High starts + few providers",
        "Low starts + many providers",
        "Low starts + few providers"
      )
    )
  ) %>%
  ggplot(
    aes(
      x = providers,
      y = starts,
      colour = market_type
    )
  ) +
  geom_vline(
    xintercept = providers_threshold,
    linetype = "dashed",
    colour = "#444444",
    linewidth = 0.6
  ) +
  geom_hline(
    yintercept = starts_threshold,
    linetype = "dashed",
    colour = "#444444",
    linewidth = 0.6
  ) +
  geom_point(
    alpha = 0.75,
    size = 2.2
  ) +
  annotate(
    "label",
    x = 1.2,
    y = 2500,
    label = "High starts\nfew providers",
    hjust = 0,
    size = 3.5,
    colour = "#222222",
    fill = "white",
    label.size = 0.2,
    label.r = unit(0.15, "lines")
  ) +
  annotate(
    "label",
    x = 25,
    y = 2500,
    label = "High starts\nmany providers",
    hjust = 0,
    size = 3.5,
    colour = "#222222",
    fill = "white",
    label.size = 0.2,
    label.r = unit(0.15, "lines")
  ) +
  annotate(
    "label",
    x = 1.2,
    y = 20,
    label = "Low starts\nfew providers",
    hjust = 0,
    size = 3.5,
    fontface = "bold",
    colour = "#222222",
    fill = "white",
    label.size = 0.2,
    label.r = unit(0.15, "lines")
  ) +
  annotate(
    "label",
    x = 25,
    y = 20,
    label = "Low starts\nmany providers",
    hjust = 0,
    size = 3.5,
    colour = "#222222",
    fill = "white",
    label.size = 0.2,
    label.r = unit(0.15, "lines")
  ) +
  scale_x_log10(
    breaks = c(1, 2, 5, 10, 25, 50, 100, 250, 500)
  ) +
  scale_y_log10(
    breaks = c(10, 25, 50, 100, 250, 500, 1000, 2500, 5000, 10000)
  ) +
  scale_colour_manual(
    values = c(
      "High starts + many providers" = colour_primary,
      "High starts + few providers" = colour_secondary,
      "Low starts + many providers" = colour_accent,
      "Low starts + few providers" = "#222222"
    ),
    breaks = c(
      "High starts + many providers",
      "High starts + few providers",
      "Low starts + many providers",
      "Low starts + few providers"
    )
  ) +
  labs(
    title = "Many standards sit in thin markets with low starts and few providers",
    subtitle = "Matched standards, England, 2023/24",
    x = "Providers with leavers, log scale",
    y = "Starts, log scale",
    colour = NULL
  ) +
  theme_illuminate_line()

p_thin_market_scatter


# Chart 14: Overall apprenticeship starts rate by LSIP area
p_lsip_starts_rate <- lsip_base %>%
  mutate(
    lsip_name = fct_reorder(lsip_name, starts_rate_per_100000_population)
  ) %>%
  ggplot(
    aes(x = lsip_name, y = starts_rate_per_100000_population)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  coord_flip() +
  labs(
    title = "Apprenticeship starts rates vary substantially across LSIP areas",
    subtitle = "Starts per 100,000 population, England, 2024/25",
    x = NULL,
    y = "Starts per 100,000 population"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_illuminate()

p_lsip_starts_rate


# Chart 15: Variation in apprenticeship starts rates across LSIPs by level
p_lsip_level_variation <- lsip_level_variation %>%
  mutate(
    apps_level = factor(
      apps_level,
      levels = c("Intermediate", "Advanced", "Higher")
    )
  ) %>%
  ggplot(
    aes(y = apps_level)
  ) +
  geom_segment(
    aes(
      x = min_rate,
      xend = max_rate,
      yend = apps_level
    ),
    colour = "#6C6C6C",
    linewidth = 1
  ) +
  geom_point(
    aes(x = median_rate),
    colour = colour_primary,
    size = 3
  ) +
  geom_point(
    aes(x = min_rate),
    colour = "#AFAFAF",
    size = 2.5
  ) +
  geom_point(
    aes(x = max_rate),
    colour = "#AFAFAF",
    size = 2.5
  ) +
  labs(
    title = "Intermediate apprenticeship rates vary most across LSIP areas",
    subtitle = "Starts per 100,000 population, England, 2024/25",
    x = "Starts per 100,000 population",
    y = NULL
  ) +
  theme_illuminate()

p_lsip_level_variation


# Chart 16: Variation in apprenticeship starts rates across LSIPs by age group
p_lsip_age_variation <- lsip_age_variation %>%
  mutate(
    age_summary = factor(
      age_summary,
      levels = c("Under 19", "19-24", "25+")
    )
  ) %>%
  ggplot(
    aes(y = age_summary)
  ) +
  geom_segment(
    aes(
      x = min_rate,
      xend = max_rate,
      yend = age_summary
    ),
    colour = "#6C6C6C",
    linewidth = 1
  ) +
  geom_point(
    aes(x = median_rate),
    colour = colour_primary,
    size = 3
  ) +
  geom_point(
    aes(x = min_rate),
    colour = "#AFAFAF",
    size = 2.5
  ) +
  geom_point(
    aes(x = max_rate),
    colour = "#AFAFAF",
    size = 2.5
  ) +
  labs(
    title = "Young apprenticeship starts vary most across LSIP areas",
    subtitle = "Starts per 100,000 population, England, 2024/25",
    x = "Starts per 100,000 population",
    y = NULL
  ) +
  theme_illuminate()

p_lsip_age_variation


# Chart 17: Spatial variation in starts rates by age and level
p_lsip_age_level_variation <- lsip_age_level_variation %>%
  ggplot(
    aes(x = age_level, y = max_min_ratio)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = max_min_ratio),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Spatial variation is highest for younger and lower-level apprenticeships",
    subtitle = "Ratio of highest to lowest LSIP starts rate, England, 2024/25",
    x = NULL,
    y = "Highest LSIP rate divided by lowest LSIP rate"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_lsip_age_level_variation

# Chart 18: Engineering and manufacturing starts by region
p_regional_engineering <- regional_engineering %>%
  mutate(
    region_name = fct_reorder(region_name, starts)
  ) %>%
  ggplot(
    aes(x = region_name, y = starts)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = scales::comma(starts)),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Engineering and manufacturing starts are unevenly distributed by region",
    subtitle = "Apprenticeship starts, England, 2024/25",
    x = NULL,
    y = "Starts"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_regional_engineering

# Chart: Geographic concentration by age and level
p_lsip_age_level_concentration <- lsip_age_level_concentration %>%
  mutate(
    segment = fct_reorder(segment, top_5_share)
  ) %>%
  ggplot(
    aes(x = segment, y = top_5_share)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(top_5_share, "%")),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Some age-level apprenticeship routes are more geographically concentrated",
    subtitle = "Share of starts accounted for by the top five LSIP areas, England, 2024/25",
    x = NULL,
    y = "Top five LSIP share of starts"
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_lsip_age_level_concentration

# Chart 18 (map): Apprenticeship starts rate by LSIP area
p_map_lsip_starts_rate <- ggplot(lsip_map_base) +
  geom_sf(
    aes(fill = starts_rate_per_100000_population),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Apprenticeship starts rates vary across LSIP areas",
    subtitle = "Starts per 100,000 population, England, 2024/25",
    fill = "Starts per\n100,000"
  ) +
  theme_illuminate_map()

p_map_lsip_starts_rate

# Chart 20: Geographic concentration by standard volume
# Proxy analysis: standard starts allocated to delivery regions using each
# provider's level-specific regional delivery profile.
technical_standard_region_concentration_long <- technical_standard_region_concentration_summary %>%
  select(
    estimated_starts_band,
    pct_concentrated,
    pct_dispersed
  ) %>%
  pivot_longer(
    cols = c(pct_concentrated, pct_dispersed),
    names_to = "metric",
    values_to = "pct_standards"
  ) %>%
  mutate(
    metric = case_when(
      metric == "pct_concentrated" ~ "Concentrated: 75%+ in one region",
      metric == "pct_dispersed" ~ "Dispersed: 5+ regions and <50% in top region",
      TRUE ~ metric
    ),
    metric = factor(
      metric,
      levels = c(
        "Concentrated: 75%+ in one region",
        "Dispersed: 5+ regions and <50% in top region"
      )
    ),
    estimated_starts_band = factor(
      estimated_starts_band,
      levels = c(
        "<50 starts",
        "50-99 starts",
        "100-249 starts",
        "250-499 starts",
        "500+ starts"
      )
    )
  )

p_technical_standard_region_concentration <- technical_standard_region_concentration_long %>%
  ggplot(
    aes(
      x = estimated_starts_band,
      y = pct_standards,
      fill = metric
    )
  ) +
  geom_col(
    position = position_dodge(width = 0.75),
    width = 0.65
  ) +
  geom_text(
    aes(label = paste0(pct_standards, "%")),
    position = position_dodge(width = 0.75),
    vjust = -0.35,
    size = 3.3,
    colour = colour_text
  ) +
  scale_fill_manual(
    values = c(
      "Concentrated: 75%+ in one region" = colour_primary,
      "Dispersed: 5+ regions and <50% in top region" = colour_secondary
    )
  ) +
  labs(
    title = "Very low-volume technical standards are more likely to be regionally concentrated",
    subtitle = "Provider-region proxy, technical-priority standards, England, 2024/25",
    x = "Estimated annual starts per standard",
    y = "Share of standards",
    fill = NULL
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(
    legend.position = "right"
  )

p_technical_standard_region_concentration

# Clean up temporary objects
rm(technical_standard_region_concentration_long)

######################
# Regional maps
######################

# Chart 19: Engineering and manufacturing apprenticeship starts by region
p_map_regional_engineering <- ggplot(region_engineering_map_base) +
  geom_sf(
    aes(fill = starts),
    colour = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Engineering and manufacturing apprenticeship starts are regionally uneven",
    subtitle = "Starts by region, England, 2024/25",
    fill = "Starts"
  ) +
  theme_illuminate_map()

p_map_regional_engineering

# Annex Map: Engineering and manufacturing as a share of regional starts
p_map_regional_engineering_share <- ggplot(region_engineering_map_base) +
  geom_sf(
    aes(fill = pct_region_starts),
    colour = "white",
    linewidth = 0.25
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = function(x) paste0(x, "%"),
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Engineering and manufacturing accounts for a different share of starts by region",
    subtitle = "Engineering and manufacturing starts as a share of regional apprenticeship starts, 2024/25",
    fill = "Share of\nstarts"
  ) +
  theme_illuminate_map()

p_map_regional_engineering_share

######################
# LAD maps
######################

# Annex Map: Engineering and manufacturing starts by local authority district
p_map_lad_engineering <- ggplot(lad_engineering_map_base) +
  geom_sf(
    aes(fill = starts),
    colour = "white",
    linewidth = 0.05
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Engineering and manufacturing starts are clustered in some local authority districts",
    subtitle = "Apprenticeship starts, England, 2024/25",
    fill = "Starts"
  ) +
  theme_illuminate_map()

p_map_lad_engineering


# Chart: Funding bands by thin-market quadrant
p_funding_by_quadrant <- funding_by_quadrant %>%
  mutate(
    market_type = factor(
      market_type,
      levels = c(
        "High starts + many providers",
        "High starts + few providers",
        "Low starts + many providers",
        "Low starts + few providers"
      )
    )
  ) %>%
  ggplot(
    aes(x = market_type, y = median_funding)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = scales::dollar(median_funding, prefix = "£", accuracy = 1)),
    vjust = -0.35,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "Median funding bands vary modestly across thin-market quadrants",
    subtitle = "Matched standards, England, 2023/24 starts and Skills England funding data",
    x = NULL,
    y = "Median maximum funding band"
  ) +
  scale_y_continuous(
    labels = scales::dollar_format(prefix = "£"),
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

p_funding_by_quadrant

# Chart: Share of high-funding standards by thin-market quadrant
p_high_funding_share_by_quadrant <- funding_by_quadrant %>%
  mutate(
    market_type = factor(
      market_type,
      levels = c(
        "High starts + many providers",
        "High starts + few providers",
        "Low starts + many providers",
        "Low starts + few providers"
      )
    )
  ) %>%
  ggplot(
    aes(x = market_type, y = pct_high_funding)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(pct_high_funding, "%")),
    vjust = -0.35,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "High-funding standards are present across all market types",
    subtitle = "Share of standards with maximum funding bands at or above the 75th percentile (£18,000)",
    x = NULL,
    y = "Share of standards"
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    breaks = seq(0, 40, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

p_high_funding_share_by_quadrant

# Chart: same-LAD vs cross-LAD apprenticeship delivery
# (data object produced in 01_exploratory.R)
p_same_lad_delivery <- same_lad_chart_data %>%
  ggplot(
    aes(x = delivery_type, y = pct_starts)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.7
  ) +
  geom_text(
    aes(label = paste0(pct_starts, "%")),
    vjust = -0.35,
    size = 4,
    colour = colour_text
  ) +
  labs(
    title = "Most apprenticeship starts cross local authority boundaries",
    subtitle = "Share of starts by learner home LAD and delivery LAD relationship, England, 2024/25",
    x = NULL,
    y = "Share of starts"
  ) +
  scale_y_continuous(
    limits = c(0, 70),
    breaks = seq(0, 70, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate()

p_same_lad_delivery

# Chart: LAD concentration by thin-market quadrant
p_lad_concentration_by_quadrant <- lad_concentration_by_quadrant %>%
  mutate(
    market_type = factor(
      market_type,
      levels = c(
        "High starts + many providers",
        "High starts + few providers",
        "Low starts + many providers",
        "Low starts + few providers"
      )
    )
  ) %>%
  ggplot(
    aes(x = market_type, y = median_top_5_lad_share)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(median_top_5_lad_share, "%")),
    vjust = -0.35,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "Thin-market standards are more locally concentrated",
    subtitle = "Median share of estimated starts in the top five delivery LADs, provider-LAD proxy, 2024/25",
    x = NULL,
    y = "Median top-five LAD share of estimated starts"
  ) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1)
  )

p_lad_concentration_by_quadrant

######################
# Annex chart objects
######################

# Annex Chart: Examples of concentrated low-volume technical standards
# Standards with 50-249 estimated starts and high top-region share.

concentrated_low_volume_technical_examples <- standard_region_concentration_proxy %>%
  filter(
    estimated_starts >= 50,
    estimated_starts < 250,
    top_1_region_share >= 50,
    ssa_tier_1 %in% c(
      "Engineering and Manufacturing Technologies",
      "Construction, Planning and the Built Environment",
      "Digital Technology",
      "Science and Mathematics",
      "Health, Public Services and Care"
    )
  ) %>%
  arrange(desc(top_1_region_share), estimated_starts) %>%
  slice_head(n = 20) %>%
  mutate(
    standard_name_short = str_wrap(standard_name, width = 35),
    standard_name_short = fct_reorder(standard_name_short, top_1_region_share)
  )

p_concentrated_low_volume_technical_examples <- concentrated_low_volume_technical_examples %>%
  ggplot(
    aes(
      x = standard_name_short,
      y = top_1_region_share
    )
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(top_1_region_share, "%")),
    hjust = -0.15,
    size = 3.2,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Some low-volume technical standards appear highly regionally concentrated",
    subtitle = "Provider-region proxy, standards with 50-249 estimated starts, England, 2024/25",
    x = NULL,
    y = "Estimated share in top delivery region"
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    breaks = seq(0, 100, 20),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate()

p_concentrated_low_volume_technical_examples

# Annex Chart A1: Single-year provider concentration
p_provider_concentration <- provider_concentration %>%
  mutate(
    top_n_label = factor(
      paste0("Top ", top_n_providers),
      levels = c("Top 10", "Top 25", "Top 50", "Top 100")
    )
  ) %>%
  ggplot(
    aes(x = top_n_label, y = cumulative_share)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(cumulative_share, "%")),
    vjust = -0.4,
    size = 4,
    colour = colour_text
  ) +
  labs(
    title = "Share of starts accounted for by the largest providers",
    subtitle = "England, 2024/25",
    x = NULL,
    y = "Share of starts"
  ) +
  scale_y_continuous(
    limits = c(0, 60),
    breaks = seq(0, 60, 10),
    labels = function(x) paste0(x, "%"),
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate()

p_provider_concentration


# Annex Chart A2: Standards by starts-provider footprint quadrant
p_thin_market_matrix <- thin_market_summary %>%
  count(market_type) %>%
  mutate(
    market_type = factor(
      market_type,
      levels = c(
        "High starts + many providers",
        "High starts + few providers",
        "Low starts + many providers",
        "Low starts + few providers"
      )
    )
  ) %>%
  ggplot(
    aes(x = market_type, y = n)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = n),
    vjust = -0.4,
    size = 4,
    colour = colour_text
  ) +
  labs(
    title = "Standards by starts and provider footprint",
    subtitle = "Matched standards, England, 2023/24",
    x = NULL,
    y = "Number of standards"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.08))
  ) +
  theme_illuminate() +
  theme(
    axis.text.x = element_text(angle = 25, hjust = 1)
  )

p_thin_market_matrix


# Annex Chart A3: Number of observable standards over time
p_standards_count_trend <- standards_subject_trends %>%
  filter(release_type == "Full year") %>%
  mutate(
    academic_year = make_academic_year(time_period)
  ) %>%
  ggplot(
    aes(x = academic_year, y = standards, group = 1)
  ) +
  geom_line(
    colour = colour_primary,
    linewidth = 1.2
  ) +
  geom_point(
    colour = colour_primary,
    size = 2.8
  ) +
  geom_text(
    aes(label = standards),
    vjust = -0.8,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "Number of observable apprenticeship standards",
    subtitle = "England, full-year releases only",
    x = "Academic year",
    y = "Number of standards"
  ) +
  scale_y_continuous(
    limits = c(450, 650),
    breaks = seq(450, 650, 50)
  ) +
  theme_illuminate()

p_standards_count_trend


# Annex Chart A4: Median starts per standard over time
p_median_starts_trend <- standards_subject_trends %>%
  filter(release_type == "Full year") %>%
  mutate(
    academic_year = make_academic_year(time_period)
  ) %>%
  ggplot(
    aes(x = academic_year, y = median_starts, group = 1)
  ) +
  geom_line(
    colour = colour_primary,
    linewidth = 1.2
  ) +
  geom_point(
    colour = colour_primary,
    size = 2.8
  ) +
  geom_text(
    aes(label = median_starts),
    vjust = -0.8,
    size = 3.5,
    colour = colour_text
  ) +
  labs(
    title = "Median annual starts per standard",
    subtitle = "England, full-year releases only",
    x = "Academic year",
    y = "Median starts"
  ) +
  scale_y_continuous(
    limits = c(0, 140),
    breaks = seq(0, 140, 20)
  ) +
  theme_illuminate()

p_median_starts_trend


# Annex Chart A5: Share of low-start standards by route
p_low_start_route <- route_compare %>%
  mutate(
    route = fct_reorder(route, pct_low)
  ) %>%
  ggplot(
    aes(x = route, y = pct_low)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(round(pct_low, 1), "%")),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Share of low-start standards by route",
    subtitle = "Standards with fewer than 50 starts, England, 2024/25",
    x = NULL,
    y = "Share of low-start standards"
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_low_start_route

# Annex Chart: Local authority districts with the most engineering and manufacturing starts
p_lad_engineering_top_30 <- lad_engineering_top_30 %>%
  mutate(
    lad_name = fct_reorder(lad_name, starts)
  ) %>%
  ggplot(
    aes(x = lad_name, y = starts)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  coord_flip() +
  labs(
    title = "Top local authority districts for engineering and manufacturing starts",
    subtitle = "Apprenticeship starts, England, 2024/25",
    x = NULL,
    y = "Starts"
  ) +
  scale_y_continuous(
    labels = scales::comma,
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_illuminate()

p_lad_engineering_top_30

# Annex Map: Under-19 apprenticeship starts rate by LSIP area
p_map_lsip_under19_rate <- ggplot(lsip_under19_map_base) +
  geom_sf(
    aes(fill = starts_rate_per_100000_population),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Under-19 apprenticeship starts rates vary sharply across LSIP areas",
    subtitle = "Starts per 100,000 relevant population, England, 2024/25",
    fill = "Starts per\n100,000"
  ) +
  theme_illuminate_map()

p_map_lsip_under19_rate

# Annex Map: Intermediate apprenticeship starts rate by LSIP area
p_map_lsip_intermediate_rate <- ggplot(lsip_intermediate_map_base) +
  geom_sf(
    aes(fill = starts_rate_per_100000_population),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Intermediate apprenticeship starts rates vary across LSIP areas",
    subtitle = "Starts per 100,000 population, England, 2024/25",
    fill = "Starts per\n100,000"
  ) +
  theme_illuminate_map()

p_map_lsip_intermediate_rate

# Map: estimated LAD delivery of low-start + few-provider standards
p_map_lad_low_starts_few_providers <- ggplot(lad_low_starts_few_providers_map_base) +
  geom_sf(
    aes(fill = estimated_starts),
    colour = "white",
    linewidth = 0.05
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = scales::comma,
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Estimated local delivery of low-start, few-provider standards",
    subtitle = "Provider-LAD proxy, delivery LADs, England, 2024/25",
    fill = "Estimated\nstarts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile."
  ) +
  theme_illuminate_map()

p_map_lad_low_starts_few_providers

# Map: share of estimated delivery starts in low-start + few-provider standards
p_map_lad_low_starts_few_providers_share <- ggplot(lad_low_starts_few_providers_share_map_base) +
  geom_sf(
    aes(fill = pct_matched_estimated_starts),
    colour = "white",
    linewidth = 0.05
  ) +
  scale_fill_gradient(
    low = "#F0EEFF",
    high = colour_primary,
    labels = function(x) paste0(x, "%"),
    na.value = "#F2F2F2"
  ) +
  labs(
    title = "Where are low-start, few-provider standards more prominent?",
    subtitle = "Estimated share of matched apprenticeship delivery, provider-LAD proxy, England, 2024/25",
    fill = "Share of\nestimated starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile."
  ) +
  theme_illuminate_map()

p_map_lad_low_starts_few_providers_share

######################
# Industry analysis: IS8 proxy sector charts
######################

# Chart: potentially fragile standards by IS8 proxy sector
fragile_is8_chart_data <- potentially_fragile_standards_is8 %>%
  count(is8_proxy_sector, sort = TRUE) %>%
  mutate(
    is8_proxy_sector = fct_reorder(is8_proxy_sector, n)
  )

p_fragile_standards_by_is8 <- fragile_is8_chart_data %>%
  ggplot(
    aes(x = is8_proxy_sector, y = n)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = n),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Potentially fragile standards are concentrated in IS8-relevant sectors",
    subtitle = "Rule-based IS8 proxy mapping of fragile standards, England, 2024/25",
    x = NULL,
    y = "Number of potentially fragile standards"
  ) +
  scale_y_continuous(
    breaks = scales::pretty_breaks(),
    expand = expansion(mult = c(0, 0.12))
  ) +
  theme_illuminate()

p_fragile_standards_by_is8

# Chart: share of standards in low-start + few-provider quadrant by IS8 proxy sector
p_low_start_few_provider_share_by_is8 <- is8_proxy_summary %>%
  mutate(
    is8_proxy_sector = fct_reorder(
      is8_proxy_sector,
      pct_low_starts_few_providers
    )
  ) %>%
  ggplot(
    aes(x = is8_proxy_sector, y = pct_low_starts_few_providers)
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.75
  ) +
  geom_text(
    aes(label = paste0(pct_low_starts_few_providers, "%")),
    hjust = -0.15,
    size = 3.5,
    colour = colour_text
  ) +
  coord_flip() +
  labs(
    title = "Thin-market standards are more common in some IS8 proxy sectors",
    subtitle = "Share of standards in the low-start, few-provider quadrant, rule-based IS8 proxy mapping",
    x = NULL,
    y = "Share of standards"
  ) +
  scale_y_continuous(
    limits = c(0, 90),
    breaks = seq(0, 90, 10),
    labels = function(x) paste0(x, "%")
  ) +
  theme_illuminate()

p_low_start_few_provider_share_by_is8

# Clean up temporary objects
rm(fragile_is8_chart_data)

######################
# Provider type by level chart
######################

provider_type_level_chart_data <- provider_type_detailed_by_level %>%
  mutate(
    provider_type_chart = case_when(
      provider_type_detailed %in% c(
        "Private training provider",
        "FE college",
        "University / HE",
        "Armed forces / defence"
      ) ~ provider_type_detailed,
      TRUE ~ "Other provider type"
    )
  ) %>%
  group_by(apps_level_clean, provider_type_chart) %>%
  summarise(
    starts = sum(starts, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(apps_level_clean) %>%
  mutate(
    pct_level_starts = round(100 * starts / sum(starts, na.rm = TRUE), 1)
  ) %>%
  ungroup() %>%
  mutate(
    apps_level_clean = factor(
      apps_level_clean,
      levels = c("Intermediate", "Advanced", "Higher")
    ),
    provider_type_chart = factor(
      provider_type_chart,
      levels = c(
        "Private training provider",
        "FE college",
        "University / HE",
        "Armed forces / defence",
        "Other provider type"
      )
    )
  )

p_provider_type_by_level <- provider_type_level_chart_data %>%
  ggplot(
    aes(
      x = apps_level_clean,
      y = pct_level_starts,
      fill = provider_type_chart
    )
  ) +
  geom_col(
    width = 0.7,
    colour = "white",
    linewidth = 0.2
  ) +
  geom_text(
    aes(
      label = if_else(pct_level_starts >= 5, paste0(pct_level_starts, "%"), "")
    ),
    position = position_stack(vjust = 0.5),
    size = 3.2,
    colour = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c(
      "Private training provider" = colour_primary,
      "FE college" = colour_blue_mid,
      "University / HE" = colour_blue_light,
      "Armed forces / defence" = colour_teal,
      "Other provider type" = colour_grey
    )
  ) +
  labs(
    title = "Provider type differs sharply by apprenticeship level",
    subtitle = "Share of starts by provider type and apprenticeship level, England, 2024/25",
    x = NULL,
    y = "Share of starts",
    fill = NULL
  ) +
  scale_y_continuous(
    labels = function(x) paste0(x, "%"),
    limits = c(0, 100),
    breaks = seq(0, 100, 20),
    expand = expansion(mult = c(0, 0))
  ) +
  theme_illuminate() +
  theme(
    legend.position = "right"
  )

p_provider_type_by_level

# Clean up temporary objects
rm(provider_type_level_chart_data)

######################
# Spatial spread of technical starts
######################

# Optional gt table version of the spatial spread summary
if (requireNamespace("gt", quietly = TRUE)) {

  spatial_spread_gt <- spatial_spread_table %>%
    gt::gt() %>%
    gt::tab_header(
      title = "Technical apprenticeship demand has local peaks, but is widely spread",
      subtitle = "Engineering and priority technical starts across English regions and LADs, 2024/25"
    ) %>%
    gt::cols_label(
      apprenticeship_area = "Apprenticeship area",
      regions_with_starts = "Regions with starts",
      largest_region_share = "Largest region share",
      effective_region_spread = "Effective regional spread",
      lads_with_starts = "LADs with starts",
      largest_lad_share = "Largest LAD share",
      effective_lad_spread = "Effective LAD spread"
    ) %>%
    gt::tab_source_note(
      source_note = "Note: effective spread converts the HHI into the number of equally sized areas that would produce the same level of concentration. It does not mean starts occur only in that number of areas."
    )

  spatial_spread_gt
}

# Callout text for the report
spatial_spread_callout_text <- paste0(
  "Technical apprenticeship demand has local peaks, but is spread across many places. ",
  "Engineering and manufacturing starts appear in ",
  lad_engineering_concentration$geographies,
  " LADs; the largest LAD accounts for ",
  lad_engineering_concentration$top_1_share,
  "% of starts, and the overall distribution is equivalent to a market spread across around ",
  round(lad_engineering_concentration$effective_geographies, 0),
  " LADs. Priority technical starts are even more widely spread, appearing in ",
  lad_priority_concentration$geographies,
  " LADs, with an effective spread of around ",
  round(lad_priority_concentration$effective_geographies, 0),
  " LADs. This means local coordination can help identify where demand is strongest, but many technical markets are too dispersed for local action alone."
)

cat(spatial_spread_callout_text)

# Chart: spatial spread of technical starts
spatial_spread_chart_data <- tibble::tibble(
  apprenticeship_area = c(
    "Engineering and\nmanufacturing",
    "Priority technical\nsubjects"
  ),
  effective_lad_spread = c(
    lad_engineering_concentration$effective_geographies,
    lad_priority_concentration$effective_geographies
  ),
  lads_with_starts = c(
    lad_engineering_concentration$geographies,
    lad_priority_concentration$geographies
  ),
  largest_lad_share = c(
    lad_engineering_concentration$top_1_share,
    lad_priority_concentration$top_1_share
  )
) %>%
  mutate(
    effective_lad_spread = round(effective_lad_spread, 0),
    label = paste0(
      effective_lad_spread,
      " effective LADs\n",
      lads_with_starts,
      " LADs with starts; largest LAD ",
      largest_lad_share,
      "%"
    ),
    apprenticeship_area = forcats::fct_reorder(
      apprenticeship_area,
      effective_lad_spread
    )
  )

p_spatial_spread_technical_starts <- spatial_spread_chart_data %>%
  ggplot(
    aes(
      x = apprenticeship_area,
      y = effective_lad_spread
    )
  ) +
  geom_col(
    fill = colour_primary,
    width = 0.65
  ) +
  geom_text(
    aes(label = label),
    hjust = -0.05,
    size = 3.4,
    colour = colour_text,
    lineheight = 0.95
  ) +
  coord_flip() +
  labs(
    title = "Technical apprenticeship demand is spread across many local areas",
    subtitle = "Effective LAD spread of starts, England, 2024/25",
    x = NULL,
    y = "Effective number of LADs"
  ) +
  scale_y_continuous(
    limits = c(0, max(spatial_spread_chart_data$effective_lad_spread) * 1.35),
    breaks = scales::pretty_breaks()
  ) +
  theme_illuminate()

p_spatial_spread_technical_starts

# Clean up temporary objects
rm(spatial_spread_chart_data)

######################
# Fragile-standard LAD maps
######################

# These use the provider-LAD proxy built in 02_analysis.R. They show
# estimated delivery exposure to potentially fragile standards and should be
# read as an indicative proxy, not observed standard-by-place starts.

# Map: estimated fragile-standard starts by delivery LAD
p_lad_fragile_standards_map <- ggplot(lad_fragile_map_base) +
  geom_sf(
    aes(fill = fragile_estimated_starts),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    labels = scales::comma,
    na.value = "grey90"
  ) +
  labs(
    title = "Estimated delivery of potentially fragile apprenticeship standards",
    subtitle = "Estimated starts in potentially fragile standards by delivery LAD, 2024/25",
    fill = "Estimated starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile."
  ) +
  theme_illuminate_map()

p_lad_fragile_standards_map

# Map: local share of starts in fragile standards
# This is often more revealing than raw starts.
p_lad_fragile_standards_share_map <- ggplot(lad_fragile_map_base) +
  geom_sf(
    aes(fill = pct_fragile),
    colour = NA
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    labels = function(x) paste0(round(x, 1), "%"),
    na.value = "grey90"
  ) +
  labs(
    title = "Estimated local exposure to potentially fragile standards",
    subtitle = "Estimated share of apprenticeship starts in fragile standards by delivery LAD, 2024/25",
    fill = "Share of starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile."
  ) +
  theme_illuminate_map()

p_lad_fragile_standards_share_map

######################
# LSIP thin-market maps
######################

# LSIP-level versions of the thin-market / fragile-standard maps, aggregated
# from the LAD proxy via the LAD-to-LSIP lookup built in 02_analysis.R.
# LSIPs are closer to the skills-planning geography, so these are intended
# for the main report; the LAD maps are for the analytical annex.

# Map: estimated delivery of low-start, few-provider standards by LSIP
p_lsip_low_start_few_provider_starts_map <- ggplot(lsip_thin_market_map_base) +
  geom_sf(
    aes(fill = low_start_few_provider_estimated_starts),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    labels = scales::comma,
    na.value = "grey90"
  ) +
  labs(
    title = "Estimated delivery of low-start, few-provider standards by LSIP",
    subtitle = "Estimated starts based on provider-standard LAD delivery profiles, 2024/25",
    fill = "Estimated starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile, aggregated to LSIPs."
  ) +
  theme_illuminate_map()

p_lsip_low_start_few_provider_starts_map

# Map: estimated LSIP exposure to low-start, few-provider standards
p_lsip_low_start_few_provider_share_map <- ggplot(lsip_thin_market_map_base) +
  geom_sf(
    aes(fill = pct_low_start_few_provider),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    labels = function(x) paste0(round(x, 1), "%"),
    na.value = "grey90"
  ) +
  labs(
    title = "Estimated LSIP exposure to low-start, few-provider standards",
    subtitle = "Estimated share of apprenticeship starts in the clearest thin-market quadrant, 2024/25",
    fill = "Share of starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile, aggregated to LSIPs."
  ) +
  theme_illuminate_map()

p_lsip_low_start_few_provider_share_map

# Map: estimated LSIP exposure to potentially fragile standards
p_lsip_fragile_share_map <- ggplot(lsip_thin_market_map_base) +
  geom_sf(
    aes(fill = pct_fragile),
    colour = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "magma",
    direction = -1,
    labels = function(x) paste0(round(x, 1), "%"),
    na.value = "grey90"
  ) +
  labs(
    title = "Estimated LSIP exposure to potentially fragile standards",
    subtitle = "Estimated share of apprenticeship starts in potentially fragile standards, 2024/25",
    fill = "Share of starts",
    caption = "Note: estimated by allocating provider-standard starts using each provider's overall delivery-LAD profile, aggregated to LSIPs."
  ) +
  theme_illuminate_map()

p_lsip_fragile_share_map

######################
# LSIP maps with Industrial Strategy cluster overlay
######################

# Overlays the sub-regional clusters named in the 2025 UK Industrial Strategy
# (cluster locations built in 02_analysis.R) on the LSIP exposure maps.
# These are triangulation maps: they identify places where strategic-sector
# activity and fragile apprenticeship provision may coincide. They do not
# show that a cluster lacks provision or is constrained by thin markets.

# Reusable cluster marker layer: white-filled dots read clearly against
# both ends of the magma fill scale.
is_cluster_points <- geom_sf(
  data = is_clusters_sf,
  shape = 21,
  size = 3,
  fill = "white",
  colour = colour_text,
  stroke = 0.7
)

# Optional repelled labels (used only if ggrepel is installed)
if (requireNamespace("ggrepel", quietly = TRUE)) {
  is_cluster_labels <- ggrepel::geom_text_repel(
    data = is_clusters_sf,
    aes(label = cluster_name, geometry = geometry),
    stat = "sf_coordinates",
    size = 2.6,
    colour = colour_text,
    bg.color = "white",
    bg.r = 0.12,
    min.segment.length = 0,
    segment.size = 0.25,
    seed = 42,
    max.overlaps = Inf
  )
} else {
  is_cluster_labels <- NULL
}

# Map: LSIP exposure to fragile standards + IS clusters
p_lsip_fragile_share_clusters_map <- p_lsip_fragile_share_map +
  is_cluster_points +
  is_cluster_labels +
  labs(
    title = "Industrial Strategy clusters and estimated fragile-standard exposure",
    subtitle = "Estimated share of starts in potentially fragile standards by LSIP; dots show 2025 Industrial Strategy sub-regional clusters (England)",
    caption = "Note: LSIP shares are an indicative provider-delivery proxy. Cluster points are approximate anchor locations from the 2025 UK Industrial Strategy."
  )

p_lsip_fragile_share_clusters_map

# Map: LSIP exposure to low-start, few-provider standards + IS clusters
p_lsip_low_start_few_provider_share_clusters_map <- p_lsip_low_start_few_provider_share_map +
  is_cluster_points +
  is_cluster_labels +
  labs(
    title = "Industrial Strategy clusters and estimated thin-market exposure",
    subtitle = "Estimated share of starts in low-start, few-provider standards by LSIP; dots show 2025 Industrial Strategy sub-regional clusters (England)",
    caption = "Note: LSIP shares are an indicative provider-delivery proxy. Cluster points are approximate anchor locations from the 2025 UK Industrial Strategy."
  )

p_lsip_low_start_few_provider_share_clusters_map

# Clean up temporary objects
rm(is_cluster_points, is_cluster_labels)

######################
# Top potentially fragile standards for report/annex
######################

fragile_standards_short <- potentially_fragile_standards %>%
  mutate(
    max_funding_label = scales::dollar(max_funding, prefix = "£", accuracy = 1),
    standard_short = str_remove(std_fwk_name_stcode, " \\(ST[0-9]+\\)$")
  ) %>%
  select(
    standard_ref,
    standard_short,
    route,
    level,
    market_type,
    starts,
    providers,
    max_funding_label,
    typical_duration,
    fragile_score
  ) %>%
  slice_head(n = 20)

fragile_standards_short %>%
  print(n = 20, width = Inf)


fragile_standards_google_docs <- potentially_fragile_standards %>%
  mutate(
    `Standard` = str_remove(std_fwk_name_stcode, " \\(ST[0-9]+\\)$"),
    `Maximum funding band` = scales::dollar(max_funding, prefix = "£", accuracy = 1),
    `Typical duration (months)` = typical_duration,
    `Fragility score` = fragile_score
  ) %>%
  select(
    `Standard reference` = standard_ref,
    `Standard`,
    `Route` = route,
    `Level` = level,
    `Market type` = market_type,
    `Starts` = starts,
    `Providers` = providers,
    `Leavers` = leavers,
    `Maximum funding band`,
    `Typical duration (months)`,
    `Regulated standard` = regulated_standard,
    `Integrated degree` = integrated_degree,
    `Fragility score`
  ) %>%
  slice_head(n = 20)

fragile_standards_google_docs %>%
  print(n = 20, width = Inf)

write_csv(
  fragile_standards_google_docs,
  file.path(output_folder, "table_fragile_standards_google_docs.csv")
)

funding_by_quadrant_google_docs <- funding_by_quadrant %>%
  mutate(
    `Median maximum funding band` = scales::dollar(median_funding, prefix = "£", accuracy = 1),
    `Mean maximum funding band` = scales::dollar(mean_funding, prefix = "£", accuracy = 1),
    `75th percentile funding band` = scales::dollar(p75_funding, prefix = "£", accuracy = 1),
    `Share high-funding standards` = paste0(pct_high_funding, "%")
  ) %>%
  select(
    `Market type` = market_type,
    `Standards` = standards,
    `Total starts` = total_starts,
    `Median starts per standard` = median_standard_starts,
    `Median providers per standard` = median_providers,
    `Median maximum funding band`,
    `Mean maximum funding band`,
    `75th percentile funding band`,
    `High-funding standards` = high_funding_standards,
    `Share high-funding standards`
  )

funding_by_quadrant_google_docs %>%
  print(n = Inf, width = Inf)

write_csv(
  funding_by_quadrant_google_docs,
  file.path(output_folder, "table_funding_by_quadrant_google_docs.csv")
)

technical_standard_region_concentration_google_docs <- technical_standard_region_concentration_summary %>%
  mutate(
    `Concentrated standards` = paste0(pct_concentrated, "%"),
    `Dispersed standards` = paste0(pct_dispersed, "%"),
    `Median top-region share` = paste0(median_top_1_region_share, "%"),
    `Median top-three-region share` = paste0(median_top_3_region_share, "%")
  ) %>%
  select(
    `Estimated starts band` = estimated_starts_band,
    `Standards` = standards,
    `Median estimated starts` = median_estimated_starts,
    `Median regions with estimated starts` = median_regions,
    `Median top-region share`,
    `Median top-three-region share`,
    `Concentrated standards`,
    `Dispersed standards`
  )

technical_standard_region_concentration_google_docs %>%
  print(n = Inf, width = Inf)

write_csv(
  technical_standard_region_concentration_google_docs,
  file.path(output_folder, "table_technical_geographic_concentration_google_docs.csv")
)

# Clean up temporary table objects (already written to CSV / printed)
rm(
  fragile_standards_google_docs,
  funding_by_quadrant_google_docs,
  technical_standard_region_concentration_google_docs
)

######################
# 16 Save selected outputs
######################

ggsave(
  filename = file.path(output_folder, "chart_01_historical_starts.png"),
  plot = p_historical_starts,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_02_age_share_trend.png"),
  plot = p_age_share_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_03_level_share_trend.png"),
  plot = p_level_share_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_04_higher_25_share_trend.png"),
  plot = p_higher_25_share_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_05_standards_distribution.png"),
  plot = p_standards_distribution,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_06_standards_concentration_trend.png"),
  plot = p_concentration_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_07_low_start_trend.png"),
  plot = p_low_start_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_08_low_start_subject_trend.png"),
  plot = p_low_start_subject_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_09_median_starts_route.png"),
  plot = p_median_starts_route,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_10_providers_distribution.png"),
  plot = p_providers_distribution,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_11_provider_concentration_trend.png"),
  plot = p_provider_concentration_trend,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_12_provider_subject_concentration.png"),
  plot = p_provider_subject_concentration,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_13_thin_market_scatter.png"),
  plot = p_thin_market_scatter,
  width = 9,
  height = 5.5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_14_lsip_starts_rate.png"),
  plot = p_lsip_starts_rate,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_15_lsip_level_variation.png"),
  plot = p_lsip_level_variation,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_16_lsip_age_variation.png"),
  plot = p_lsip_age_variation,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_17_lsip_age_level_variation.png"),
  plot = p_lsip_age_level_variation,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_18_map_lsip_starts_rate.png"),
  plot = p_map_lsip_starts_rate,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_19_map_regional_engineering.png"),
  plot = p_map_regional_engineering,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_lsip_under19_rate.png"),
  plot = p_map_lsip_under19_rate,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_lsip_intermediate_rate.png"),
  plot = p_map_lsip_intermediate_rate,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_regional_engineering_share.png"),
  plot = p_map_regional_engineering_share,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_lad_engineering.png"),
  plot = p_map_lad_engineering,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_20_technical_standard_region_concentration.png"),
  plot = p_technical_standard_region_concentration,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_concentrated_low_volume_technical_examples.png"),
  plot = p_concentrated_low_volume_technical_examples,
  width = 9,
  height = 7,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_lad_concentration_by_thin_market_quadrant.png"),
  plot = p_lad_concentration_by_quadrant,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_lad_low_starts_few_providers_proxy.png"),
  plot = p_map_lad_low_starts_few_providers,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "annex_map_lad_low_starts_few_providers_share_proxy.png"),
  plot = p_map_lad_low_starts_few_providers_share,
  width = 8,
  height = 9,
  dpi = 300
)

# Industry analysis charts
ggsave(
  filename = file.path(output_folder, "chart_fragile_standards_by_is8_proxy_sector.png"),
  plot = p_fragile_standards_by_is8,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_low_start_few_provider_share_by_is8_proxy_sector.png"),
  plot = p_low_start_few_provider_share_by_is8,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_provider_type_by_level.png"),
  plot = p_provider_type_by_level,
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "chart_spatial_spread_technical_starts.png"),
  plot = p_spatial_spread_technical_starts,
  width = 9,
  height = 4.8,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "map_lad_fragile_standards_estimated_starts.png"),
  plot = p_lad_fragile_standards_map,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "map_lad_fragile_standards_share.png"),
  plot = p_lad_fragile_standards_share_map,
  width = 8,
  height = 9,
  dpi = 300
)

# LSIP thin-market maps
ggsave(
  filename = file.path(output_folder, "map_lsip_low_start_few_provider_estimated_starts.png"),
  plot = p_lsip_low_start_few_provider_starts_map,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "map_lsip_low_start_few_provider_share.png"),
  plot = p_lsip_low_start_few_provider_share_map,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "map_lsip_fragile_standards_share.png"),
  plot = p_lsip_fragile_share_map,
  width = 8,
  height = 9,
  dpi = 300
)

# LSIP maps with Industrial Strategy cluster overlay
ggsave(
  filename = file.path(output_folder, "map_lsip_fragile_standards_share_is_clusters.png"),
  plot = p_lsip_fragile_share_clusters_map,
  width = 8,
  height = 9,
  dpi = 300
)

ggsave(
  filename = file.path(output_folder, "map_lsip_low_start_few_provider_share_is_clusters.png"),
  plot = p_lsip_low_start_few_provider_share_clusters_map,
  width = 8,
  height = 9,
  dpi = 300
)
