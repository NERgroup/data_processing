#jogsmith@ucsc.edu

rm(list = ls())

################################################################################
# KURBS Site Effort Processing Script

# This script processes Google Form field entries documenting site-level
# sampling effort for KURBS, including:
#   (1) deployment and collection of brushes and recruitment plates
#   (2) monthly survey events
#
# The Google Form is structured such that each submission corresponds to a
# single site × event (Deployment, Collection, or Survey). For Deployment and
# Collection events, multiple media types (e.g., benthic brush, suspended brush,
# recruitment plates) are recorded as comma-separated entries.
#
# Inputs:
#   - Google Sheet (Form responses)
#     Contains:
#       * event metadata (timestamp, event_record)
#       * site identifiers (site_surveyed)
#       * event dates (date_deployed, date_collected)
#       * comma-separated media lists (samples_deployed, samples_collected)
#       * notes 
#
# Key Data Processing Steps:
#
# 1. Standardize form responses
#    - Harmonize site names to compact codes:
#        KF1D, KF1S, KF2D, KF2S, BR1D, BR1S, BR2D, BR2S
#    - Define a single event_date per row based on event type
#    - Consolidate Deployment, Collection, and Survey records
#
# 2. Separate survey data
#    - Survey events are independent of soak-time calculations
#    - Output as a standalone table (survey_table_clean.csv)
#
# 3. Parse media entries (brushes and plates)
#    - Expand comma-separated sample lists into long format
#    - Collapse replicate units:
#        "brush 1", "brush 2" → single category
#    - Classify media into:
#        benthic_brush
#        suspended_brush
#        benthic_plate
#        suspended_plate
#
# 4. Construct media event table
#    - Aggregate to one row per:
#        site × event_date × event_record × sample_type
#    - This reflects the true sampling unit (site-level media swap event)
#
# 5. Calculate soak time
#    - For each Collection event, identify the most recent prior Deployment
#      for the same:
#        site + sample_type
#    - Same-day deployments are excluded (they represent replacement gear)
#    - Soak time is calculated as:
#        soak_time = date_collected - date_deployed
#
# Outputs:
#   1. effort_table_clean.csv
#        Cleaned media event table (Deployment + Collection)
#
#   2. effort_soak_time_clean.csv
#        Collection events with associated deployment dates and soak times
#
#   3. survey_table_clean.csv
#        Independent survey records
#
# Notes:
#   - This script assumes that all media at a site are collected and redeployed
#     on the same field day.
#   - The pairing of deployment and collection events is based strictly on
#     temporal ordering (most recent prior deployment).
#   - Any missing deployment matches or anomalous soak times are flagged in
#     diagnostic checks at the end of the script.

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

# gs4_auth()

################################################################################
# Define input sheet and output paths

sheet_id <- "1WySnAi80tQBcRrE9teq9ayPyMI_0J6QzhSh7sPU2tAk"
gid <- 1372456336

output_dir <- "/Volumes/enhydra/data/KURBS/recruitment_data/site_table/processed"

media_events_file <- fs::path(output_dir, "effort_table_clean.csv")
soak_file        <- fs::path(output_dir, "effort_soak_time_clean.csv")
survey_file      <- fs::path(output_dir, "survey_table_clean.csv")

################################################################################
# Read Google Sheet

ss <- as_sheets_id(sheet_id)

meta <- gs4_get(ss)
tab_title <- meta$sheets$name[meta$sheets$id == gid]
tab_title

effort_raw <- read_sheet(ss, sheet = tab_title) %>%
  janitor::clean_names()

################################################################################
# Site recode helper

recode_site <- function(x) {
  x <- str_squish(x)
  
  case_when(
    x %in% c("KF1_deep", "KF1D")    ~ "KF1D",
    x %in% c("KF1_shallow", "KF1S") ~ "KF1S",
    x %in% c("KF2_deep", "KF2D")    ~ "KF2D",
    x %in% c("KF2_shallow", "KF2S") ~ "KF2S",
    x %in% c("BR1_deep", "BR1D")    ~ "BR1D",
    x %in% c("BR1_shallow", "BR1S") ~ "BR1S",
    x %in% c("BR2_deep", "BR2D")    ~ "BR2D",
    x %in% c("BR2_shallow", "BR2S") ~ "BR2S",
    TRUE ~ x
  )
}

effort_clean <- effort_raw %>%
  mutate(
    event_record = str_squish(event_record),
    
    event_date = case_when(
      event_record == "Deployment" ~ as_date(date_deployed),
      event_record == "Collection" ~ as_date(date_collected),
      event_record == "Survey"     ~ as_date(timestamp),
      TRUE                         ~ as_date(coalesce(date_collected, date_deployed, timestamp))
    ),
    
    site = recode_site(case_when(
      event_record == "Deployment" ~ site_9,
      event_record == "Collection" ~ site_5,
      event_record == "Survey"     ~ site_surveyed,
      TRUE                         ~ coalesce(site_5, site_9, site_surveyed)
    )),
    
    samples = case_when(
      event_record == "Deployment" ~ samples_deployed,
      event_record == "Collection" ~ samples_collected,
      TRUE                         ~ NA_character_
    ),
    
    notes = case_when(
      event_record == "Deployment" ~ notes_11,
      event_record == "Collection" ~ notes_7,
      event_record == "Survey"     ~ notes_13,
      TRUE                         ~ coalesce(notes_7, notes_11, notes_13)
    )
  ) %>%
  select(
    timestamp,
    email_address,
    event_record,
    event_date,
    site,
    samples,
    notes
  ) %>%
  mutate(
    across(c(email_address, event_record, site, samples, notes), ~ na_if(.x, "")),
    across(c(email_address, event_record, site, samples, notes), ~ str_squish(.x))
  ) %>%
  arrange(site, event_date, timestamp)

################################################################################
# Standardize raw Google Form export

effort_clean <- effort_raw %>%
  mutate(
    event_record = str_squish(event_record),
    
    event_date = case_when(
      event_record == "Deployment" ~ as_date(date_deployed),
      event_record == "Collection" ~ as_date(date_collected),
      event_record == "Survey"     ~ as_date(timestamp),
      TRUE                         ~ as_date(coalesce(date_collected, date_deployed, timestamp))
    ),
    
    site = recode_site(case_when(
      event_record == "Deployment" ~ site_9,
      event_record == "Collection" ~ site_5,
      event_record == "Survey"     ~ site_surveyed,
      TRUE                         ~ coalesce(site_5, site_9, site_surveyed)
    )),
    
    samples = case_when(
      event_record == "Deployment" ~ samples_deployed,
      event_record == "Collection" ~ samples_collected,
      TRUE                         ~ NA_character_
    ),
    
    notes = case_when(
      event_record == "Deployment" ~ notes_11,
      event_record == "Collection" ~ notes_7,
      event_record == "Survey"     ~ notes_13,
      TRUE                         ~ coalesce(notes_7, notes_11, notes_13)
    )
  ) %>%
  select(
    timestamp,
    email_address,
    event_record,
    event_date,
    site,
    samples,
    notes
  ) %>%
  mutate(
    across(c(email_address, event_record, site, samples, notes), ~ na_if(.x, "")),
    across(c(email_address, event_record, site, samples, notes), ~ str_squish(.x))
  ) %>%
  arrange(site, event_date, timestamp)

################################################################################
# Separate surveys into their own table

survey_table_clean <- effort_clean %>%
  filter(event_record == "Survey") %>%
  transmute(
    timestamp,
    email_address,
    event_date,
    site,
    notes
  ) %>%
  arrange(site, event_date, timestamp)

################################################################################
# Build media event table for brushes and plates only

media_events_long <- effort_clean %>%
  filter(event_record %in% c("Deployment", "Collection")) %>%
  separate_rows(samples, sep = ",") %>%
  mutate(
    samples = str_squish(samples),
    samples = na_if(samples, "")
  ) %>%
  filter(!is.na(samples)) %>%
  mutate(
    sample_type = case_when(
      str_detect(samples, regex("^benthic\\s+brush", ignore_case = TRUE)) ~ "benthic_brush",
      str_detect(samples, regex("^suspended\\s+brush", ignore_case = TRUE)) ~ "suspended_brush",
      str_detect(samples, regex("^benthic\\s+recruitment\\s+plate", ignore_case = TRUE)) ~ "benthic_plate",
      str_detect(samples, regex("^suspended\\s+recruitment\\s+plate", ignore_case = TRUE)) ~ "suspended_plate",
      TRUE ~ NA_character_
    )
  ) %>%
  select(
    timestamp,
    email_address,
    event_record,
    event_date,
    site,
    samples,
    sample_type,
    notes
  )

################################################################################
# Collapse brush 1 / brush 2 into one row per site x date x event x sample_type

effort_table_clean <- media_events_long %>%
  filter(!is.na(sample_type)) %>%
  group_by(site, event_date, event_record, sample_type) %>%
  summarise(
    timestamp = min(timestamp, na.rm = TRUE),
    email_address = dplyr::first(stats::na.omit(email_address)),
    notes = paste(unique(stats::na.omit(notes)), collapse = " | "),
    .groups = "drop"
  ) %>%
  mutate(
    email_address = na_if(email_address, ""),
    notes = na_if(notes, "")
  ) %>%
  arrange(site, event_date, event_record, sample_type, timestamp)

################################################################################
# Calculate soak time
# For each collection row, find the most recent prior deployment for the same
# site and sample_type. Same-day redeployment does NOT count.

deployments <- effort_table_clean %>%
  filter(event_record == "Deployment") %>%
  select(
    site,
    sample_type,
    deploy_timestamp = timestamp,
    date_deployed = event_date,
    deploy_notes = notes
  )

collections <- effort_table_clean %>%
  filter(event_record == "Collection") %>%
  select(
    site,
    sample_type,
    collect_timestamp = timestamp,
    date_collected = event_date,
    collect_notes = notes
  )

effort_soak_time_clean <- collections %>%
  left_join(
    deployments,
    by = c("site", "sample_type")
  ) %>%
  filter(is.na(date_deployed) | date_deployed < date_collected) %>%
  group_by(site, sample_type, collect_timestamp, date_collected) %>%
  arrange(date_deployed, deploy_timestamp, .by_group = TRUE) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    soak_time = as.numeric(date_collected - date_deployed)
  ) %>%
  select(
    site,
    sample_type,
    date_deployed,
    date_collected,
    soak_time,
    deploy_timestamp,
    collect_timestamp,
    deploy_notes,
    collect_notes
  ) %>%
  arrange(site, sample_type, date_collected, collect_timestamp)

################################################################################
# Checks

glimpse(effort_table_clean)
glimpse(effort_soak_time_clean)
glimpse(survey_table_clean)

effort_table_clean %>%
  count(event_record, sample_type, sort = TRUE)

effort_table_clean %>%
  count(site, event_date, event_record, sample_type, sort = TRUE) %>%
  filter(n > 1)

media_events_long %>%
  filter(is.na(sample_type)) %>%
  distinct(samples) %>%
  print(n = 100)

effort_soak_time_clean %>%
  filter(is.na(date_deployed)) %>%
  count(site, sample_type, date_collected, sort = TRUE)

effort_soak_time_clean %>%
  filter(!is.na(soak_time) & soak_time <= 0)

################################################################################
# Save outputs

fs::dir_create(output_dir, recurse = TRUE)

readr::write_csv(effort_table_clean, media_events_file)
readr::write_csv(effort_soak_time_clean, soak_file)
readr::write_csv(survey_table_clean, survey_file)

media_events_file
soak_file
survey_file
