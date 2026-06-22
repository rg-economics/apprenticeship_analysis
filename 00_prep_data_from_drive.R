################################################################################
## 00_prep_data_from_drive.R
##
## ONE-TIME (or rebuild-time) prep script, designed to run on Posit Cloud's
## basic plan (1,024 MiB session memory limit).
##
## WHY THIS SCRIPT EXISTS:
## 01_exploratory.R was originally written assuming all raw EES/Xplore CSVs
## sit in a local data/ folder and can be loaded together. On a 1GB-RAM Posit
## session, several of these files (100MB+ on disk, often 3-5x that once
## parsed into a data frame) cannot all be held in memory at once - and in
## some cases even ONE of them is tight against the limit on its own.
##
## HOW THIS SCRIPT WORKS:
## - Each source file gets its own self-contained block below.
## - You run ONE block at a time (select it and run, or Cmd/Ctrl+Enter
##   through it), watch the console for the "OK" message, THEN move to the
##   next block. Do not run the whole script in one go until every
##   individual block has been tested at least once.
## - Each block: downloads the raw CSV from Drive -> reads it -> cleans it
##   down to what 01_exploratory.R actually needs (small) -> saves that small
##   result as .rds -> uploads the .rds to your Drive output folder ->
##   deletes the raw CSV from Posit's disk -> removes the big object from
##   memory and garbage-collects.
## - After ALL blocks have been run once, output_folder/data/ (and your
##   Drive output folder) will contain small, clean .rds files. At that
##   point 01_exploratory.R itself can be rewritten to just readRDS() these
##   instead of reading raw CSVs - a separate, later step.
##
## CLEANING LOGIC:
## Each block's filtering/cleaning logic is copied as exactly as possible
## from the corresponding section of 01_exploratory.R, so the outputs match
## what 01_exploratory.R would have produced. Where 01_exploratory.R had a
## known bug (the thin-market quadrant double-counting fix), the CORRECTED
## logic is used here instead, with a comment marking the change.
##
## BEFORE RUNNING:
## 1. Fill in your Drive output folder ID below (drive_output_folder_id).
## 2. Fill in each file's Drive file ID in its block (search for
##    "FILL IN FILE ID" below).
## 3. Run the "00 Setup" block first, every session, before any file block.
################################################################################

######################
# 00 Setup (run this first, every session)
######################

if (!require(googledrive)) install.packages("googledrive")
if (!require(tidyverse)) install.packages("tidyverse")

library(googledrive)
library(tidyverse)

# AUTHENTICATION NOTE:
# drive_download() of files shared as "anyone with the link" works fine
# de-authenticated (drive_deauth()). drive_upload() does NOT - writing a new
# file into one of YOUR Drive folders requires a real, logged-in Google
# identity with edit access to that folder. An anonymous/de-authenticated
# session has no such identity, and Drive's API rejects the upload with a
# generic 400 Bad Request (this is what you hit).
#
# drive_auth() below will, the first time it runs in a session, open a
# browser tab / print a URL asking you to log in to Google and grant access.
# Complete that once per Posit session, then both download and upload will
# work for the rest of the session. If running non-interactively and a
# browser can't open, drive_auth() will print a URL you can open manually
# and a code to paste back in.
drive_auth()

# Note: an authenticated session can still download files that are shared
# as "anyone with the link" - authentication only adds capability (write
# access), it doesn't remove the ability to read public/shared files. So
# switching from drive_deauth() to drive_auth() does not break the download
# steps below.

source("config.R")

# Local folder for raw downloads (temporary - files are deleted after each
# block completes, so this folder should stay small/empty between blocks).
raw_download_folder <- "data_raw_temp"
if (!dir.exists(raw_download_folder)) {
  dir.create(raw_download_folder, recursive = TRUE)
}

# Local folder for the small cleaned .rds outputs.
prepped_data_folder <- file.path(output_folder, "data")
if (!dir.exists(prepped_data_folder)) {
  dir.create(prepped_data_folder, recursive = TRUE)
}

# Your Google Drive folder ID where cleaned .rds files should be uploaded.
# Get this from the folder's share link: drive.google.com/drive/folders/<THIS_PART>
drive_output_folder_id <- "1K-y95H_BzFdWK1JVlLZCM8uniVJ0YjLi"

# Shared cleaning helper, identical to 01_exploratory.R's clean_numeric().
clean_numeric <- function(df) {
  num_patterns <- c(
    "starts", "achievements", "participation", "enrolments", "vacancies",
    "adverts", "leavers", "completers", "achievers", "commitments",
    "redundancies", "population", "rate", "percent"
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

# Helper: download one file from Drive into the raw temp folder.
download_raw_csv <- function(file_id, local_name) {
  local_path <- file.path(raw_download_folder, local_name)
  drive_download(
    as_id(file_id),
    path = local_path,
    overwrite = TRUE
  )
  local_path
}

# Helper: save a cleaned object as .rds locally AND upload it to the Drive
# output folder. Prints a clear OK/size message so you can confirm success
# before moving to the next block.
save_and_upload_rds <- function(object, object_name) {
  local_rds_path <- file.path(prepped_data_folder, paste0(object_name, ".rds"))
  saveRDS(object, local_rds_path)
  
  drive_upload(
    media = local_rds_path,
    path = as_id(drive_output_folder_id),
    name = paste0(object_name, ".rds"),
    overwrite = TRUE
  )
  
  message(
    "OK: ", object_name, ".rds saved locally (",
    round(file.size(local_rds_path) / 1024, 1), " KB) and uploaded to Drive. ",
    "Rows: ", nrow(object), ", Cols: ", ncol(object)
  )
}

# Helper: delete a raw downloaded CSV and clean up memory. Call this at the
# end of every block, after the cleaned object has been saved.
cleanup_raw_file <- function(local_path, ...) {
  if (file.exists(local_path)) {
    file.remove(local_path)
  }
  # Remove the named large objects from the global environment (pass their
  # names as strings, e.g. cleanup_raw_file(path, "narts_raw")).
  big_object_names <- list(...)
  if (length(big_object_names) > 0) {
    rm(list = unlist(big_object_names), envir = .GlobalEnv)
  }
  gc()
  message("Raw file deleted and memory cleaned up.")
}

message("Setup complete. Proceed to the file blocks below, one at a time.")


######################
# BLOCK 1: app-narts-provider-level-fwk-std.csv  (provider x standard)
######################
# This is the file used for the thin-market quadrant's provider-count axis.
# Source: 01_exploratory.R, section 06, "providers_per_standard".
#
# Original file is large (100MB+). This block reads it once, immediately
# collapses it down to providers_per_standard (one row per standard_ref,
# tiny), and discards the raw object.

narts_file_id <- "1Y1ERXF7QRoeXS5EamuXBcnt2tp-65vfP"

narts_local_path <- download_raw_csv(
  narts_file_id,
  "app-narts-provider-level-fwk-std.csv"
)

narts_raw <- read_csv(narts_local_path, show_col_types = FALSE) %>%
  clean_numeric()

# Same filter logic as 01_exploratory.R's provider_standard_base, applied
# immediately so we never hold more of narts_raw in memory than necessary.
provider_standard_base <- narts_raw %>%
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

# providers_per_standard: one row per standard_ref. This is already
# correctly aggregated in the original script - the double-counting bug was
# never in this object, it was in how the STARTS side (standards_202324_keyed
# in 01_exploratory.R) failed to aggregate to one row per standard before
# this object got joined onto it. providers_per_standard is reproduced here
# unchanged.
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

# Integrity check: confirm standard_ref is unique here (it should be, by
# construction of the group_by above, but worth confirming explicitly since
# this is exactly the kind of assumption that broke before).
narts_duplicate_check <- providers_per_standard %>%
  filter(!is.na(standard_ref)) %>%
  count(standard_ref, name = "n_rows") %>%
  filter(n_rows > 1)

if (nrow(narts_duplicate_check) > 0) {
  warning(
    nrow(narts_duplicate_check),
    " standard_ref value(s) are non-unique in providers_per_standard. ",
    "Inspect narts_duplicate_check before using this file downstream."
  )
} else {
  message("providers_per_standard: standard_ref is unique. OK.")
}

save_and_upload_rds(providers_per_standard, "providers_per_standard")

cleanup_raw_file(
  narts_local_path,
  "narts_raw",
  "provider_standard_base"
)

# providers_per_standard itself is small and fine to keep in memory if you
# want to inspect it - but it has already been saved, so it's safe to rm()
# it too once you've confirmed the OK message above.
rm(providers_per_standard)


######################
# BLOCK 2: [next file - to be added once Block 1 is confirmed working]
######################







