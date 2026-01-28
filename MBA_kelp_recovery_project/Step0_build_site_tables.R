


#This script is for processing the site waypoints spreadsheet. 
#the output is a cleaned metadata file

#Steps involved:

#1: read raw site lookup table from Google drive
#***Recovery survey metadata**
#site_long = official site name, site type, and zone (shallow or deep) 
#survey_type = recovery 
#region = Carmel or Monterey
#site_name_2024 = site name in year 2024
#site_type_2024 = site_type in 2024
#site_short_2024 = site name and site type in 2024 without zone
#site_long_2024 = site name, site type, and zone in 2024
#site_name_2025 = new site name in 2025. NOTE: sites were renanamed in 2025
#to follow a chronological order.
#site_type_2025 = new site type in 2025. NOTE: some sites sampled in 2024
#were clearly not the anticipated site type (some forests were barrens,
#barrens were forests, etc.). These were redesignated after the 2024 field
#season. The site_type_2025 is the official site type to use. 
#transect = shallow (5-12 meters, typically) or deep (12-20 meters, typically)
#old_latitude = planned latitude 
#old_longitude = planned longitude
#new_latitude = actual latitude surveyed
#new_longitude = actual longitude surveyed
#reprojected_coords = no means site was not relocated, yes = site was relocated
#target_depth_meters = target depth in meters
#uc_heading = upcoast heading for transects 1 and 2 
#dc_heading = downcoast heading for transects 3 and 4
#original_date_surveyed_2024 = date the site was first sampled in 2024
#resite_date_2024 = date the site was resampled, often due to incomplete
#sampling from original survey date. 
#original_survey_date_2025 = date the site was first sampled in 2024
#resite_date_2025 = date the site was first sampled in 2025
#in_stack = index of whether the physical data are catalogued. 
#notes = notes


################################################################################
#

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, readxl)
#gs4_auth()

#Set paths
datout <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/processed"
datin <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/raw"

#read margin lookup table
margin_orig <- readxl::read_excel(
  file.path(datin, "site_tables", "MBA_site_waypoints.xlsx"),
  sheet = 5
) %>% clean_names()

#read recovery sites
recovery_orig <- readxl::read_excel(
  file.path(datin, "site_tables", "MBA_site_waypoints.xlsx"),
  sheet = 3
) %>% clean_names()

################################################################################
#Step 1: process margin data

marge_build1 <- margin_orig %>%
  #set column types
  mutate(survey_type = factor(survey_type),
         region = factor(region),
         site_name_2024 = factor(site_name_2024),
         site_name_2025 = factor(site_name_2025),
         transect = as.numeric(transect),
         heading_out = as.numeric(heading_out),
         target_latitude = as.numeric(target_latitude),
         target_longitude = as.numeric(target_longitude),
         actual_latitude = as.numeric(actual_latitude),
         actual_longitude = as.numeric(actual_longitude),
         date_surveyed = as.Date(date_surveyed, format = "%Y-%m-%d"),
         resite_needed = factor(resite_needed),
         resite_date = as.Date(resite_date, format = "%Y-%m-%d"),
         in_stack = factor(in_stack),
         notes = as.character(notes)
  ) %>%
  #drop sites that were never surveyed or were bad margins
  filter(!(is.na(site_name_2025))) %>% #sites where in_stack == yes means they were in the raw data
  select(survey_type, region, site_name_2024, site_name_2025, transect,
         heading_out, target_latitude, target_longitude, actual_latitude,
         actual_longitude, date_surveyed_originally=date_surveyed, resite_needed, resite_date,
         notes)%>%
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site_name_2024 = str_replace(site_name_2024, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    }),
    site_name_2025 = str_replace(site_name_2025, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  mutate(
    # Add date_surveyed to resite_date if NA
    survey_date = if_else(is.na(resite_date), date_surveyed_originally, resite_date),
    #fix lat/longs
    actual_latitude = coalesce(actual_latitude, target_latitude),
    actual_longitude = coalesce(actual_longitude, target_longitude)) %>%
  #drop columns and rename
  select(-target_latitude, -target_longitude) %>%
  select(survey_type, region, site_name_2024, site_name_2025, transect, heading_out,
         latitude = actual_latitude, longitude = actual_longitude, 
         survey_date,date_surveyed_originally)


################################################################################
#Step 2: process recovery data

reco_build1 <- recovery_orig %>%
  data.frame() %>%
  mutate(across(everything(), as.character)) %>%
  mutate(across(everything(), ~ na_if(., "NULL"))) %>%
  type_convert() %>%
  mutate(
    site_long = factor(site_long),
    survey_type = factor(survey_type),
    region = factor(region),
    site_name_2024 = factor(site_name_2024),
    site_type_2024 = factor(site_type_2024),
    site_short_2024 = factor(site_short_2024),
    site_long_2024 = factor(site_long_2024),
    site_name_2025 = factor(site_name_2025),
    site_type_2025 = factor(site_type_2025),
    transect = factor(transect),
    old_latitude = as.numeric(old_latitude),
    old_longitude = as.numeric(old_longitude),
    new_latitude = as.numeric(new_latitude),
    new_longitude = as.numeric(new_longitude),
    reprojected_coords = factor(reprojected_coords),
    target_depth_meters = as.numeric(target_depth_meters),
    uc_heading = as.numeric(uc_heading),
    dc_heading = as.numeric(dc_heading),
    original_date_surveyed_2024 = ymd(original_date_surveyed_2024),
    original_date_surveyed_2025 = mdy(original_date_surveyed_2025),
    resite_date_2024 = ymd(resite_date_2024),
    resite_date_2025 = mdy(resite_date_2025),
    in_stack = factor(in_stack),
    notes = as.character(notes)
  ) %>%
  #create an official survey date. If a site was resampled, the official survey
  #date is the most recent sampling date for a given year. 
  mutate(
    survey_date_2024_official = if_else(is.na(resite_date_2024), original_date_surveyed_2024, resite_date_2024),
    survey_date_2025_official = if_else(is.na(resite_date_2025), original_date_surveyed_2025, resite_date_2025)
  )%>%
  # Apply standard site naming
  mutate(
    # Use a function within str_replace to process each match
    site_name_2025 = str_replace(site_name_2025, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    }),
    site_name_2024 = str_replace(site_name_2024, "([A-Za-z]+)([0-9]+)", function(x) {
      # Extract letters and numbers
      parts <- str_match(x, "([A-Za-z]+)([0-9]+)")
      letters <- toupper(parts[, 2])   # Convert to uppercase if needed
      numbers <- parts[, 3]
      # Pad numbers with leading zero
      numbers_padded <- str_pad(numbers, width = 2, side = "left", pad = "0")
      # Combine parts with underscore
      paste0(letters, "_", numbers_padded)
    })
  ) %>%
  #drop columns and rename
  select(survey_type, region, 
         #official site name and type becomes the 2025 designation
         site_name_2025, site_type_2025,
         site_name_2024, site_type_2024, zone = transect,
         latitude = new_latitude, longitude = new_longitude, latitude_old = old_latitude,
         longitude_old = old_longitude, 
         #official 2024 survey date is the most recent date for that year
         survey_date_2024 = survey_date_2024_official,
         #official 2025 survey date is the most recent date for that year
         survey_date_2025 = survey_date_2025_official,
         notes
  ) %>%
  #set data types
  mutate(
    survey_type = factor(survey_type),
    region = factor(region),
    site_type_2025 = factor(site_type_2025),
    site_type_2024 = factor(site_type_2024),
    zone = factor(zone),
  )

##NOTE: joining site table with the survey data based on survey date will 
#only include the most recent sampling date for a given year. This is intentional,
#since sites were resampled within a year only if there was an issue with the first
#sampling attempt. 



################################################################################
#Step 3: export

write_csv(marge_build1, file.path(datout, "site_tables","margin_site_table.csv")) #last write 28 January 2026

write_csv(reco_build1, file.path(datout, "site_tables","recovery_site_table.csv")) #last write 28 January 2026


