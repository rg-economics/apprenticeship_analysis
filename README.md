## Analysis guide

This repository contains R analysis exploring England’s apprenticeship system as a market. The analysis looks at how apprenticeship starts are distributed across standards, providers, levels, age groups and geographies, with a particular focus on whether parts of the system operate as “thin markets” where low learner volumes, narrow provider choice and specialist delivery requirements may create coordination problems.

The analysis is organised around several core questions:

1. **How has the apprenticeship system changed over time?**
   We analyse starts over time by age and level, including the shift towards older learners and higher apprenticeships.

2. **How concentrated is demand across standards?**
   We examine the distribution of starts by apprenticeship standard, the share of starts accounted for by the largest standards, and the number of low-volume standards.

3. **Where do thin markets appear?**
   We classify standards into four market types using starts and provider counts: high-start/many-provider, high-start/few-provider, low-start/many-provider and low-start/few-provider. This helps identify standards where weak demand and limited provider depth coincide.

4. **How is provision structured across provider types?**
   We analyse starts by provider type and apprenticeship level, showing how the provider mix differs between intermediate, advanced and higher apprenticeships.

5. **How geographically spread is apprenticeship delivery?**
   We analyse LSIP, regional and LAD-level patterns where data allow. We also use provider-region and provider-LAD delivery profiles to create proxy estimates of where standards are likely to be delivered. These proxy analyses are indicative only: standard-level geography is not directly observed in the main published starts data.

6. **Which standards may be most fragile?**
   We combine starts, provider counts, funding-band data, route information and technical-priority proxies to identify standards where low demand, narrow provider footprint and delivery complexity may overlap.

7. **How do fragile standards relate to strategic sectors?**
   We use a rule-based proxy mapping to link apprenticeship standards to Industrial Strategy sectors. This is not an official classification; it is used to test whether potentially fragile standards are plausibly linked to strategically important parts of the economy.

## Data sources

The analysis uses three main public data sources:

### Skills England apprenticeship standards data

Source: https://skillsengland.education.gov.uk/apprenticeships/

Used for apprenticeship standards metadata, including standard title, route, level, status, funding band, typical duration, regulated status and related fields.

Main file used:

* `Apprenticeships.csv`

### Explore Education Statistics apprenticeship data

Source: https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships/2024-25/explore

Used for apprenticeship starts, standards, subjects, routes, provider starts, learner age/level breakdowns, historical trends and geography files available through Explore Education Statistics.

Main files used:

* `app-routes-standards-202425-q4.csv`
* `app-subject-standards-202425-q4.csv`
* `app-provider-starts-202425-q4.csv`
* `app-narts-provider-level-fwk-std.csv`
* `app-learner-detailed-202425-q4.csv`
* `app-geography-population-202425-q4.csv`
* `app-geography-detailed-202425-q4.csv`
* `app-historical-summary-to-2425.csv`

### DfE apprenticeship provider dashboard data

Source: https://department-for-education.shinyapps.io/apprenticeships-provider-dashboard/

Used for additional provider-level and provider-geography analysis, including provider type, delivery region, learner home LAD and delivery LAD.

Main files used:

* `2024_25-alllevels-allagegroups-provider_breakdowns.csv`
* `2024_25-starts---subjects-and-standards.csv`
* `lad-2024_25.csv`

## Mapping data

Some scripts use boundary files for maps and spatial joins. These are not apprenticeship datasets, but are needed to reproduce the map outputs.

Boundary folders used:

* `LSIP_OCT_2025/`
* `Local_Authority_Districts_DEC_2025/`
* `Regions_December_2025/`

Each shapefile folder should include all supporting files, such as `.shp`, `.shx`, `.dbf` and `.prj`.

## Important caveats

Several analyses use proxies rather than directly observed variables.

First, “thin markets” are defined analytically using starts and provider counts. The classification is intended to identify relative market fragility, not to prove that a standard is failing or that any specific level of starts/providers is objectively sufficient.

Second, provider-region and provider-LAD standard-level analyses are estimated allocations. Standard-level starts are not directly observed by region, LAD or LSIP in the main published data. We therefore allocate provider-standard starts across geographies using provider delivery profiles. These results should be interpreted as indicative evidence about delivery geography, not precise counts of standard-level starts in each place.

Third, maximum funding bands are not actual delivery costs. They indicate the upper limit of funding available for training and assessment on a standard, not the price paid or whether the standard is underfunded.

Fourth, the Industrial Strategy sector mapping is a rule-based proxy using standard routes and title keywords. It is not an official sector classification, and some standards may cut across multiple sectors.
