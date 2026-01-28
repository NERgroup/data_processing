

################################################################################
# About
# data processing script written by JG.Smith jogsmith@ucsc.edu


#context: data were entered twice. This script joins the two separate entries and
#identifies mismatches. The mismatched entries are then uploaded as a spreadsheet
#to Google drive for reconciliation. 

#steps involved

#1: compare first and second quadrat extries
#1a: set data types and apply standard site naming convention. Official 
#site names and types are irrelevant at this stage. These are handled
#in a later step where the reconciled data are processed.
#1b: identify mismatched metadata keys (useful for finding lines of missing
#data or line that were entered twice, etc.)
#1c: identify mismatches quadrat data entries. 

#2: compare first and second sea urchin size data entry
#2a: set data types and apply standard site naming convention. Official 
#site names and types are irrelevant at this stage. These are handled
#in a later step where the reconciled data are processed.
#2b: identify mismatched entries

#3: compare first and second kelp swath entries
#3a: set data types and apply standard site naming convention. Official 
#site names and types are irrelevant at this stage. These are handled
#in a later step where the reconciled data are processed.
#3b: identify mismatched entries for species other than MACPYR
#3c: identify mismatched entries for MACPYR

#4: compare first and second dermasterias entries
#4a: set data types and apply standard site naming convention. Official 
#site names and types are irrelevant at this stage. These are handled
#in a later step where the reconciled data are processed.
#4b: identify mismatched entries 

################################################################################

rm(list=ls())

librarian::shelf(tidyverse,here, janitor, googlesheets4, lubridate, splitstackshape,
                 googledrive)
#gs4_auth()

#set dir
datin <- "/Volumes/enhydra/data/kelp_recovery/MBA_kelp_forest_database/raw"

#read original data
quad_raw <- readxl::read_excel(
  file.path(datin, "recovery","original_entry","recovery_survey_entry.xlsx"),
  sheet = 1
) %>% clean_names()

urchin_raw <- readxl::read_excel(
  file.path(datin, "recovery","original_entry","recovery_survey_entry.xlsx"),
  sheet = 2
) %>% clean_names()


kelp_raw <- readxl::read_excel(
  file.path(datin, "recovery","original_entry","recovery_survey_entry.xlsx"),
  sheet = 3
) %>% clean_names()

derm_raw <- readxl::read_excel(
  file.path(datin, "recovery","original_entry","recovery_survey_entry.xlsx"),
  sheet = 4
) %>% clean_names()

#read QAQC data
quad_qc <- readxl::read_excel(
  file.path(datin, "recovery","second_entry","QAQC_recovery_survey_entry.xlsx"),
  sheet = 1
) %>% clean_names()

urchin_qc <- readxl::read_excel(
  file.path(datin, "recovery","second_entry","QAQC_recovery_survey_entry.xlsx"),
  sheet = 3
) %>% clean_names()


kelp_qc <- readxl::read_excel(
  file.path(datin, "recovery","second_entry","QAQC_recovery_survey_entry.xlsx"),
  sheet = 4
) %>% clean_names()

derm_qc <- readxl::read_excel(
  file.path(datin, "recovery","second_entry","QAQC_recovery_survey_entry.xlsx"),
  sheet = 2
) %>% clean_names()

################################################################################
# process quadrat entry

#step 1a: set data types and apply standard site naming convention
quad_raw_build1 <- quad_raw %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  data.frame()%>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0)%>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate),
    drift_superlayer = as.character(drift_superlayer)
  ) %>%
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
         -observer_buddy,
         -write_in,
         -notes) 

quad_qc_build1 <-  quad_qc %>%
  # Remove example first row and classifiers
  slice(-1) %>%
  select(-windows_ctrl_alt_shift_0_mac_command_option_shift_0) %>%
  # Set quadrat to numeric by removing R/L
  mutate(quadrat = str_remove(quadrat, "[RL]$")) %>%
  # Set column types
  mutate(
    name_of_data_enterer = factor(name_of_data_enterer),
    site = factor(site),
    site_type = factor(site_type),
    zone = factor(zone),
    survey_date = ymd(survey_date),
    transect = as.numeric(transect),
    quadrat = as.numeric(quadrat),
    substrate = factor(substrate),
    drift_superlayer = as.character(drift_superlayer)
  ) %>%
  #fix site name
  mutate(site = str_replace(site, "REC(\\d+)", "REC_\\1"))%>%
  select(-name_of_data_enterer,
         -observer_buddy,
         -write_in,
         -notes) 


#Step 1b: idnetify mismatched metadata keys
keys_missing_in_quad_qc <- anti_join(quad_raw_build1, quad_qc_build1, 
                                     by = c("site", "site_type", "zone", "survey_date", "transect", "quadrat")) %>%
  select("site", "site_type", "zone", "survey_date", "transect", "quadrat")


#Step 1c: identify mismatched data entries
quad_discrep_values <- quad_raw_build1 %>%
  inner_join(quad_qc_build1, by = c("site", "site_type", "zone", "survey_date", "transect", "quadrat"), suffix = c("_raw", "_qc")) %>%
  mutate(across(ends_with("_raw"), ~ if_else(. != get(str_replace(cur_column(), "_raw$", "_qc")), paste(.," ≠ ", get(str_replace(cur_column(), "_raw$", "_qc"))), NA_character_), .names = "{.col}_diff")) %>%
  select(site, site_type, zone, survey_date, transect, quadrat, ends_with("_diff")) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  mutate(resolved = "") %>%
  #filter(year(survey_date) == 2024) 
  filter(year(survey_date) == 2025)

#Export
# Define file path for export
quad_file <- "quad_discrep_values.csv"
quad_keys <- "quad_keys.csv"

# Write the CSV locally
#write_csv(quad_discrep_values, quad_file)

# Upload to the specified Google Drive folder

#2024 upload
#drive_upload(quad_file, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE) 

#2025 upload
drive_upload(quad_file, path = as_id("1bJmhI7ooyp2tIJXZxYKKRWiEpxKJZyf0"), overwrite = TRUE)



################################################################################
# process urchin size entry

#step 2a: set data types and apply standard site naming convention
urch_raw_build1 <- urchin_raw %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x15) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) %>%
  # Ensure unique species-size per grouping. We can drop rows that are duplicates
  distinct()

urch_qc_build1 <- urchin_qc %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_9_mac_command_option_shift_9) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) %>%
  # Ensure unique species-size per grouping. We can drop rows that are duplicates
  distinct()


#Step 2b: idnetify mismatched entries
urch_discrep_values <- urch_qc_build1 %>%
  inner_join(urch_raw_build1, 
             by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species", "size"), 
             suffix = c("_raw", "_qc")) %>%
  # Compare count values
  mutate(count_diff = if_else(count_raw != count_qc, paste(count_raw, "≠", count_qc), NA_character_)) %>%
  # Keep only mismatches
  filter(!is.na(count_diff)) %>%
  # Select relevant columns for output
  select(site, site_type, zone, date, transect, depth, depth_units, species, size, count_diff) %>%
  #filter(year(date) == 2024)
  filter(year(date) == 2025)


#Export
# Define file path for export
swath_urchin <- "swath_urchin.csv"

# Write the CSV locally
write_csv(urch_discrep_values, swath_urchin)

# Upload to the specified Google Drive folder

#2024 upload
#drive_upload(quad_urchin, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE)

#2025 upload
drive_upload(swath_urchin, path = as_id("1bJmhI7ooyp2tIJXZxYKKRWiEpxKJZyf0"), overwrite = TRUE)


################################################################################
# process kelp entry

# Step 3a: apply standard site naming 
kelp_raw_build1 <- kelp_raw %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  # Ensure unique species-size per grouping
  distinct()

# Process kelp entry
kelp_qc_build1 <- kelp_qc %>%
  slice(-1) %>%
  data.frame() %>%
  select(-windows_ctrl_alt_shift_8_mac_command_option_shift_8) %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    stipe_counts_macrocystis_only = as.numeric(stipe_counts_macrocystis_only),
    count = as.numeric(count),
    subsample_meter = as.numeric(subsample_meter)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -observer, -buddy, -x16) %>%
  # Ensure unique species-size per grouping
  distinct()


# Step 3b: Identify mismatched entries for species other than macro
kelp_discrep_values <- kelp_qc_build1 %>%
  full_join(kelp_raw_build1, 
            by = c("site", "site_type", "zone", "date", "transect", "depth", "depth_units", "species"), 
            suffix = c("_qc", "_raw")) %>%
  #
  filter(species != "MACPYR") %>%
  # Identify differences across key columns
  mutate(
    stipe_diff = if_else(stipe_counts_macrocystis_only_qc != stipe_counts_macrocystis_only_raw,
                         paste0(stipe_counts_macrocystis_only_raw, " ≠ ", stipe_counts_macrocystis_only_qc), NA_character_),
    count_diff = if_else(count_qc != count_raw,
                         paste0(count_raw, " ≠ ", count_qc), NA_character_),
    subsample_diff = if_else(subsample_meter_qc != subsample_meter_raw,
                             paste0(subsample_meter_raw, " ≠ ", subsample_meter_qc), NA_character_)
  ) %>%
  # Filter to rows where at least one mismatch occurred
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  select(site, site_type, zone, date, transect, depth, depth_units, species,
         stipe_diff, count_diff, subsample_diff) %>%
  arrange(site, zone, date, transect, depth, species)


# Step 3b: Identify mismatched entries for MACPYR
group_cols <- c("site", "site_type", "zone", "date", "transect")

# Filter MACPYR only
macpyr_raw <- kelp_raw_build1 %>%
  filter(species == "MACPYR") %>%
  select(all_of(group_cols), stipe_counts_macrocystis_only) %>%  # keep group cols + stipe counts
  group_by(site, site_type, zone, date, transect) %>%
  summarize(
    n_plants = dplyr::n(),                                  # number of rows/plants
    total_stipes = sum(stipe_counts_macrocystis_only, na.rm = TRUE),
    .groups = "drop"
  )


macpyr_qc <- kelp_qc_build1 %>%
  filter(species == "MACPYR") %>%
  select(all_of(group_cols), stipe_counts_macrocystis_only) %>%  # keep group cols + stipe counts
  group_by(site, site_type, zone, date, transect) %>%
  summarize(
    n_plants = dplyr::n(),                                  # number of rows/plants
    total_stipes = sum(stipe_counts_macrocystis_only, na.rm = TRUE),
    .groups = "drop"
  )


keys <- c("site", "site_type", "zone", "date", "transect")

# 1) Value mismatches on shared keys
macpyr_discrep <- macpyr_raw %>%
  inner_join(macpyr_qc, by = keys, suffix = c("_raw", "_qc")) %>%
  mutate(
    n_plants_diff = if_else(
      (n_plants_raw != n_plants_qc) | xor(is.na(n_plants_raw), is.na(n_plants_qc)),
      paste0(n_plants_raw, " ≠ ", n_plants_qc),
      NA_character_
    ),
    total_stipes_diff = if_else(
      (total_stipes_raw != total_stipes_qc) | xor(is.na(total_stipes_raw), is.na(total_stipes_qc)),
      paste0(total_stipes_raw, " ≠ ", total_stipes_qc),
      NA_character_
    )
  ) %>%
  select(all_of(keys), n_plants_diff, total_stipes_diff) %>%
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  mutate(resolved = "") %>%
  arrange(site, date, transect) %>%
  filter(year(date) == 2025)


#Export
# Define file path for export
swath_kelp <- "swath_kelp.csv"
swath_macpyr <- "swath_macpyr.csv"

# Write the CSV locally
write_csv(kelp_discrep_values, swath_kelp)
write_csv(macpyr_discrep, swath_macpyr)


# Upload to the specified Google Drive folder

#2024 upload
#drive_upload(swath_kelp, path = as_id("1IaTpgTw6Q8-EDvSo3oONBCMDVIfLyzRB"), overwrite = TRUE)

#2025 upload
drive_upload(swath_kelp, path = as_id("1bJmhI7ooyp2tIJXZxYKKRWiEpxKJZyf0"), overwrite = TRUE)
drive_upload(swath_macpyr, path = as_id("1bJmhI7ooyp2tIJXZxYKKRWiEpxKJZyf0"), overwrite = TRUE)


################################################################################
# Steo 4 process dermasterias entry

#step 4a: set data types and apply standard site naming convention
derm_raw_build1 <- derm_raw %>%
  data.frame() %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count),
    diet = as.factor(diet)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -windows_ctrl_alt_shift_1_mac_command_option_shift_1,
         -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) 

derm_qc_build1 <- derm_qc %>%
  data.frame() %>%
  # Set column types
  mutate(
    site = str_replace(site, "REC(\\d+)", "REC_\\1"),
    site = as.character(site),
    site_type = as.character(site_type),
    zone = as.character(zone),
    date = ymd(date),
    transect = as.numeric(transect),
    depth = as.numeric(depth),
    depth_units = as.character(depth_units),
    species = as.character(species),
    size = as.numeric(size),
    count = as.numeric(count),
    diet = as.factor(diet)
  ) %>%
  # Remove unnecessary columns
  select(-name_of_data_enterer, -windows_ctrl_alt_shift_1_mac_command_option_shift_1,
         -observer, -buddy) %>%
  # Arrange by all relevant columns
  arrange(site, site_type, zone, date, transect, depth, depth_units, species, size) 


# tep 4b: Identify mismatched entries
derm_discrep_values <- derm_qc_build1 %>%
  inner_join(derm_raw_build1, 
             by = c("site", "site_type", "zone", "date", "transect", 
                    "depth", "depth_units", "species", "size"), 
             suffix = c("_qc", "_raw")) %>%
  # Compare count and diet values
  mutate(
    count_diff = if_else(count_raw != count_qc, 
                         paste(count_raw, "≠", count_qc), 
                         NA_character_),
    
    diet_diff = if_else(as.character(diet_raw) != as.character(diet_qc), 
                        paste(as.character(diet_raw), "≠", as.character(diet_qc)), 
                        NA_character_)
  ) %>%
  # Keep only mismatches
  filter(if_any(ends_with("_diff"), ~ !is.na(.))) %>%
  # Select relevant columns
  select(site, site_type, zone, date, transect, depth, depth_units, 
         species, size, count_diff, diet_diff) %>%
  arrange(site, zone, date, transect, depth, species, size) %>%
  mutate(resolved = "")


#Export
# Define file path for export
swath_derm <- "swath_derm.csv"

# Write the CSV locally
write_csv(derm_discrep_values, swath_derm)

# Upload to the specified Google Drive folder
drive_upload(swath_derm, path = as_id("1bJmhI7ooyp2tIJXZxYKKRWiEpxKJZyf0"), overwrite = TRUE)


