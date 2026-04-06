



rm(list = ls())

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
# Define input sheet and output path

sheet_id <- "1WySnAi80tQBcRrE9teq9ayPyMI_0J6QzhSh7sPU2tAk"
gid <- 1372456336

output_dir <- "/Volumes/enhydra/data/KURBS/recruitment_data/site_table/processed"
output_file <- fs::path(output_dir, "effort_table_clean.csv")

################################################################################
# Read Google Sheet

ss <- as_sheets_id(sheet_id)

meta <- gs4_get(ss)
tab_title <- meta$sheets$name[meta$sheets$id == gid]
tab_title

effort_raw <- read_sheet(ss, sheet = tab_title) %>%
  janitor::clean_names()

################################################################################
# Inspect raw data

glimpse(effort_raw)
names(effort_raw)

################################################################################
# Standardize event-specific columns into one common structure
# This creates one row per form submission

effort_clean <- effort_raw %>%
  mutate(
    event_record = str_squish(event_record),
    
    event_date = case_when(
      event_record == "Deployment" ~ as_date(date_deployed),
      event_record == "Collection" ~ as_date(date_collected),
      event_record == "Survey" ~ as_date(timestamp),
      TRUE ~ as_date(coalesce(date_collected, date_deployed, timestamp))
    ),
    
    site = case_when(
      event_record == "Deployment" ~ site_9,
      event_record == "Collection" ~ site_5,
      event_record == "Survey" ~ site_surveyed,
      TRUE ~ coalesce(site_5, site_9, site_surveyed)
    ),
    
    samples = case_when(
      event_record == "Deployment" ~ samples_deployed,
      event_record == "Collection" ~ samples_collected,
      TRUE ~ NA_character_
    ),
    
    notes = case_when(
      event_record == "Deployment" ~ notes_11,
      event_record == "Collection" ~ notes_7,
      event_record == "Survey" ~ notes_13,
      TRUE ~ coalesce(notes_7, notes_11, notes_13)
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
  arrange(event_date, timestamp)

################################################################################
# Expand comma-separated sample field into one row per individual sample

effort_long <- effort_clean %>%
  separate_rows(samples, sep = ",") %>%
  mutate(
    samples = str_squish(samples),
    samples = na_if(samples, "")
  )

################################################################################
# Parse sample type and sample number
# Examples:
# "Suspended brush 1" -> sample_type = "suspended_brush", sample_number = 1
# "Benthic brush 2"   -> sample_type = "benthic_brush",   sample_number = 2

effort_long <- effort_long %>%
  mutate(
    sample_type = case_when(
      str_detect(samples, regex("^Suspended brush", ignore_case = TRUE)) ~ "suspended_brush",
      str_detect(samples, regex("^Benthic brush", ignore_case = TRUE)) ~ "benthic_brush",
      TRUE ~ NA_character_
    ),
    
    sample_number = str_extract(samples, "\\d+$") %>%
      as.integer()
  )

################################################################################
# Final clean effort table
# Keep one row per actual sampler, which is what we want for joins later

effort_table_clean <- effort_long %>%
  filter(!is.na(samples)) %>%
  transmute(
    timestamp,
    email_address,
    event_record,
    event_date,
    site,
    sample_label = samples,
    sample_type,
    sample_number,
    notes
  ) %>%
  arrange(site, event_date, event_record, sample_type, sample_number, timestamp)

################################################################################
# Optional checks

glimpse(effort_table_clean)

effort_table_clean %>%
  count(event_record, sample_type, sort = TRUE)

effort_table_clean %>%
  filter(is.na(sample_type) | is.na(sample_number))

################################################################################
# Save cleaned csv

fs::dir_create(output_dir, recurse = TRUE)

readr::write_csv(effort_table_clean, output_file)

output_file

