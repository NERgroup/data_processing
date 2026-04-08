rm(list = ls())

################################################################################
# KURBS sample processing script
#
# This script processes raw KURBS sample-count data from the Google Form,
# summarizes counts to the site x collection_date level, joins the processed
# effort soak-time table, and calculates counts per unit effort ("daily_counts").
#
# Key logic:
# - The site effort table is treated as the authoritative source for collection
#   dates and soak-time metadata.
# - Raw sample-sheet dates are preserved in `collection_date_raw`.
# - Known data-entry errors in the sample sheet are corrected before summary.
# - Final joins to the effort table use the corrected authoritative date field.
#
################################################################################

################################################################################
# Prep workspace and load packages

librarian::shelf(
  "googlesheets4",
  "dplyr",
  "tidyr",
  "stringr",
  "janitor",
  "lubridate",
  "readr",
  "fs"
)

################################################################################
# Define paths

sample_sheet_id <- "1IQsGCYl4hXibI9KnnpSjxPLYulQjXGDP_MgT4lIOtsg"

effort_file <- "/Volumes/enhydra/data/KURBS/recruitment_data/site_table/processed/effort_soak_time_clean.csv"

output_dir  <- "/Volumes/enhydra/data/KURBS/recruitment_data/brushes/processed"
output_file <- fs::path(output_dir, "sample_counts_processed.csv")

################################################################################
# Read raw sample data from Google Sheet

ss <- as_sheets_id(sample_sheet_id)

dat_raw <- read_sheet(
  ss,
  sheet = 1
) %>%
  janitor::clean_names()

################################################################################
# Inspect raw data

glimpse(dat_raw)
names(dat_raw)

################################################################################
# Helper: recode site names to compact site codes used in effort table

recode_site <- function(x) {
  x <- str_squish(x)
  
  case_when(
    x %in% c("Kelp Forest 1 Deep", "KF1_deep", "KF1D") ~ "KF1D",
    x %in% c("Kelp Forest 1 Shallow", "KF1_shallow", "KF1S") ~ "KF1S",
    x %in% c("Kelp Forest 2 Deep", "KF2_deep", "KF2D") ~ "KF2D",
    x %in% c("Kelp Forest 2 Shallow", "KF2_shallow", "KF2S") ~ "KF2S",
    
    x %in% c("Barrens Reef 1 Deep", "Baren Reef 1 Deep", "Baren reef 1 Deep", "BR1_deep", "BR1D") ~ "BR1D",
    x %in% c("Barrens Reef 1 Shallow", "Baren Reef 1 Shallow", "BR1_shallow", "BR1S") ~ "BR1S",
    
    x %in% c("Barrens Reef 2 Deep", "Baren Reef 2 Deep", "Baren reef 2 Deep", "BR2_deep", "BR2D") ~ "BR2D",
    x %in% c("Barrens Reef 2 Shallow", "Baren Reef 2 Shallow", "BR2_shallow", "BR2S") ~ "BR2S",
    
    TRUE ~ x
  )
}

################################################################################
# Read processed effort soak-time table
# This is treated as the authoritative date table

effort_soak_time_clean <- readr::read_csv(
  effort_file,
  show_col_types = FALSE
) %>%
  janitor::clean_names() %>%
  mutate(
    date_deployed = as_date(date_deployed),
    date_collected = as_date(date_collected),
    site = str_squish(site),
    sample_type = str_squish(sample_type)
  )

################################################################################
# Create authoritative site-date lookup from effort table

effort_site_dates <- effort_soak_time_clean %>%
  distinct(site, date_collected) %>%
  arrange(site, date_collected)

################################################################################
# Clean sample data
# Preserve raw date, then apply known corrections so the effort table remains
# the authoritative date source.

dat_clean <- dat_raw %>%
  mutate(
    timestamp = as.POSIXct(timestamp),
    
    collection_date_raw = as_date(collection_date),
    sorting_date = as_date(sorting_date),
    
    site = recode_site(location),
    
    location = str_squish(location),
    substrate_type = str_squish(substrate_type),
    sorter_name = str_squish(name_of_person_sorting_first_last),
    
    notes = str_squish(notes),
    notes = na_if(notes, ""),
    
    total_urchins =
      coalesce(number_of_purple_urchins, 0) +
      coalesce(number_of_red_urchins, 0) +
      coalesce(number_of_unidentified_urchins, 0)
  ) %>%
  mutate(
    collection_date = case_when(
      site == "BR2S" & collection_date_raw == as_date("2026-02-27") ~ as_date("2026-02-26"),
      site == "BR1D" & collection_date_raw == as_date("2026-03-08") ~ as_date("2026-03-03"),
      TRUE ~ collection_date_raw
    ),
    
    collection_date_corrected = collection_date != collection_date_raw
  )

################################################################################
# Optional checks on recoding / corrections

dat_clean %>%
  count(location, site, sort = TRUE)

dat_clean %>%
  filter(collection_date_corrected) %>%
  distinct(site, collection_date_raw, collection_date)

################################################################################
# Summarize sample data to site x collection_date
#
# Key point:
# We are using site and corrected collection_date as the shared fields with the
# effort table. Total counts and total vials are summed across all rows within
# each site x collection_date.

sample_counts_summary <- dat_clean %>%
  group_by(site, collection_date) %>%
  summarise(
    n_rows = n(),
    n_unique_sample_numbers = n_distinct(sample_number, na.rm = TRUE),
    total_vials = sum(number_of_vials_in_sample, na.rm = TRUE),
    
    total_purple_urchins = sum(number_of_purple_urchins, na.rm = TRUE),
    total_red_urchins = sum(number_of_red_urchins, na.rm = TRUE),
    total_unidentified_urchins = sum(number_of_unidentified_urchins, na.rm = TRUE),
    total_urchins = sum(total_urchins, na.rm = TRUE),
    
    total_crabs = sum(number_of_crabs, na.rm = TRUE),
    total_gastropods = sum(number_of_gastropods, na.rm = TRUE),
    total_bivalves = sum(number_of_bivalves, na.rm = TRUE),
    total_plastic = sum(number_of_plastic, na.rm = TRUE),
    
    first_sorting_date = suppressWarnings(min(sorting_date, na.rm = TRUE)),
    last_sorting_date = suppressWarnings(max(sorting_date, na.rm = TRUE)),
    
    substrate_types = paste(sort(unique(na.omit(substrate_type))), collapse = " | "),
    sorters = paste(sort(unique(na.omit(sorter_name))), collapse = " | "),
    notes = paste(unique(na.omit(notes)), collapse = " | "),
    
    raw_collection_dates = paste(sort(unique(na.omit(as.character(collection_date_raw)))), collapse = " | "),
    any_collection_date_corrected = any(collection_date_corrected, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  mutate(
    first_sorting_date = if_else(is.infinite(first_sorting_date), as_date(NA), first_sorting_date),
    last_sorting_date = if_else(is.infinite(last_sorting_date), as_date(NA), last_sorting_date),
    substrate_types = na_if(substrate_types, ""),
    sorters = na_if(sorters, ""),
    notes = na_if(notes, ""),
    raw_collection_dates = na_if(raw_collection_dates, "")
  ) %>%
  arrange(site, collection_date)

################################################################################
# Summarize effort soak-time table to site x date_collected
#
# If there are multiple media rows for a site/date, this collapses them to the
# site-date level for joining.

effort_site_summary <- effort_soak_time_clean %>%
  group_by(site, date_collected) %>%
  summarise(
    date_deployed = suppressWarnings(min(date_deployed, na.rm = TRUE)),
    soak_time = suppressWarnings(max(soak_time, na.rm = TRUE)),
    sample_types = paste(sort(unique(na.omit(sample_type))), collapse = " | "),
    deploy_notes = paste(unique(na.omit(deploy_notes)), collapse = " | "),
    collect_notes = paste(unique(na.omit(collect_notes)), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    date_deployed = if_else(is.infinite(date_deployed), as_date(NA), date_deployed),
    soak_time = if_else(is.infinite(soak_time), as.numeric(NA), soak_time),
    sample_types = na_if(sample_types, ""),
    deploy_notes = na_if(deploy_notes, ""),
    collect_notes = na_if(collect_notes, "")
  )

################################################################################
# Join effort table to sample summary
#
# Join key:
#   site + corrected collection_date == date_collected

sample_counts_processed <- sample_counts_summary %>%
  left_join(
    effort_site_summary,
    by = c(
      "site",
      "collection_date" = "date_collected"
    )
  ) %>%
  mutate(
    daily_counts = total_urchins / soak_time,
    daily_purple_urchins = total_purple_urchins / soak_time,
    daily_red_urchins = total_red_urchins / soak_time,
    daily_unidentified_urchins = total_unidentified_urchins / soak_time
  ) %>%
  select(
    site,
    collection_date,
    raw_collection_dates,
    any_collection_date_corrected,
    date_deployed,
    soak_time,
    
    total_purple_urchins,
    total_red_urchins,
    total_unidentified_urchins,
    total_urchins,
    
    daily_purple_urchins,
    daily_red_urchins,
    daily_unidentified_urchins,
    daily_counts,
    
    total_crabs,
    total_gastropods,
    total_bivalves,
    total_plastic,
    
    total_vials,
    n_rows,
    n_unique_sample_numbers,
    first_sorting_date,
    last_sorting_date,
    substrate_types,
    sample_types,
    sorters,
    notes,
    deploy_notes,
    collect_notes
  ) %>%
  arrange(site, collection_date)

################################################################################
# Checks

glimpse(sample_counts_processed)

# Rows that still failed to match effort
sample_counts_processed %>%
  filter(is.na(soak_time)) %>%
  arrange(site, collection_date)

# Weird soak times
sample_counts_processed %>%
  filter(!is.na(soak_time) & soak_time <= 0)

# Check for duplicated site/date after join
sample_counts_processed %>%
  count(site, collection_date, sort = TRUE) %>%
  filter(n > 1)

################################################################################
# Save processed output

fs::dir_create(output_dir, recurse = TRUE)

readr::write_csv(sample_counts_processed, output_file)

output_file