#jogmsith@ucsc.edu

rm(list=ls())


################################################################################
#Load packages and data, set dir

librarian::shelf(tidyverse, janitor, googlesheets4)

#load data

#NOTE: input is currently set as first entry. Need to update to final reconciled
#data when complete.

spreadsheet_url <- "https://docs.google.com/spreadsheets/d/129OyWLuE64lXgXDu7VDU1EHLWpjZ0C5bverqpEhO9S8/edit"

(tabs <- sheet_names(spreadsheet_url))

urch_raw <- read_sheet(
  spreadsheet_url,
  sheet = "swath_urchin_size",
  skip = 4,
  col_types = "c"
) %>%
  janitor::clean_names()

kelp_raw <- read_sheet(
  spreadsheet_url,
  sheet = "swath_kelp",
  skip = 4,
  col_types = "c"
) %>%
  janitor::clean_names()

fronds_raw <- googlesheets4::read_sheet(
  spreadsheet_url,
  sheet = "fronds_pull_down_long",
  skip = 4,
  col_types = "c"
) %>%
  janitor::clean_names()


################################################################################
#Convert depth to meters

urch_raw_m <- urch_raw %>%
  mutate(
    depth_units_clean = str_to_lower(str_trim(depth_units)),
    depth = as.numeric(depth),
    depth = case_when(
      depth_units_clean %in% c("feet", "foot", "ft") ~ depth * 0.3048,
      depth_units_clean %in% c("meter", "meters", "m") ~ depth,
      TRUE ~ depth
    ),
    depth_units = case_when(
      depth_units_clean %in% c(
        "feet", "foot", "ft", "meter", "meters", "m"
      ) ~ "Meters",
      TRUE ~ str_trim(depth_units)
    )
  ) %>%
  dplyr::select(-depth_units_clean)


################################################################################
#Step 1: clean up data for live urchin density and behavior

#Keep blank-behavior count rows and concealed/exposed behavior rows only.
#Exclude Unknown species and other behavior categories, which are used for
#cracked-test data.

urch_build1 <- urch_raw_m %>%
  mutate(
    behavior_blank = is.na(behavior) | str_trim(behavior) == "",
    behavior_clean = str_to_lower(str_trim(behavior)),
    behavior_clean = case_when(
      behavior_clean %in% c("conceiled", "concealed") ~ "concealed",
      behavior_clean == "exposed" ~ "exposed",
      TRUE ~ NA_character_
    ),
    count = as.numeric(count),
    species = str_trim(species)
  ) %>%
  filter(
    (is.na(species) | str_to_lower(species) != "unknown") &
      (behavior_blank | behavior_clean %in% c("concealed", "exposed"))
  )


################################################################################
#Step 2: process live urchin density and behavior data

#Total counts are recorded for each species on a transect. Extrapolation is
#not needed.

str(urch_build1)


#Calculate behavior proportions from the behavior sample for each transect
#and species.

urch_behav_build1 <- urch_build1 %>%
  filter(behavior_clean %in% c("concealed", "exposed")) %>%
  group_by(
    site, site_type, zone, date, transect,
    depth, depth_units, species
  ) %>%
  summarise(
    concealed_sample_n = sum(
      count[behavior_clean == "concealed"],
      na.rm = TRUE
    ),
    exposed_sample_n = sum(
      count[behavior_clean == "exposed"],
      na.rm = TRUE
    ),
    behavior_sample_n = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    proportion_concealed = concealed_sample_n / behavior_sample_n,
    proportion_exposed = exposed_sample_n / behavior_sample_n
  )


#Calculate live urchin density and add behavior proportions and estimated
#behavior-specific densities.

urch_den_live_build1 <- urch_build1 %>%
  group_by(
    site, site_type, zone, date, transect,
    depth, depth_units, species
  ) %>%
  summarise(
    total_urchins = if (any(behavior_blank)) {
      sum(count[behavior_blank], na.rm = TRUE)
    } else {
      sum(count, na.rm = TRUE)
    },
    density_source = if (any(behavior_blank)) {
      "blank behavior counts"
    } else {
      "sum across size classes"
    },
    .groups = "drop"
  ) %>%
  mutate(
    density_urchins_m2 = total_urchins / 20
  ) %>%
  left_join(
    urch_behav_build1,
    by = c(
      "site", "site_type", "zone", "date", "transect",
      "depth", "depth_units", "species"
    )
  ) %>%
  mutate(
    estimated_concealed_density_m2 =
      density_urchins_m2 * proportion_concealed,
    estimated_exposed_density_m2 =
      density_urchins_m2 * proportion_exposed
  ) %>%
  arrange(site, zone, date, transect, species)


################################################################################
#Step 3: process cracked-test and whole-test density data

#Use the original data so Unknown species records are retained.
#Identify test condition from behavior labels containing "crack" or "whole".

urch_test_build1 <- urch_raw_m %>%
  mutate(
    behavior_clean = str_to_lower(str_trim(behavior)),
    test_type = case_when(
      str_detect(behavior_clean, "crack") ~ "cracked",
      str_detect(behavior_clean, "whole") ~ "whole",
      TRUE ~ NA_character_
    ),
    count = as.numeric(count),
    species = str_trim(species),
    species = if_else(
      str_to_lower(species) == "unknown",
      "Unknown",
      species
    )
  ) %>%
  filter(!is.na(test_type)) %>%
  group_by(
    site, site_type, zone, date, transect,
    depth, depth_units, species
  ) %>%
  summarise(
    total_cracked_tests = if (any(test_type == "cracked")) {
      sum(count[test_type == "cracked"], na.rm = TRUE)
    } else {
      NA_real_
    },
    total_whole_tests = if (any(test_type == "whole")) {
      sum(count[test_type == "whole"], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  ) %>%
  mutate(
    density_cracked_tests_m2 = total_cracked_tests / 20,
    density_whole_tests_m2 = total_whole_tests / 20
  ) %>%
  dplyr::select(
    site, site_type, zone, date, transect,
    depth, depth_units, species,
    density_cracked_tests_m2, density_whole_tests_m2
  )


################################################################################
#Step 4: combine live urchin and test density data

#The full join retains Unknown species rows used for test data.

urch_den_build1 <- full_join(
  urch_den_live_build1,
  urch_test_build1,
  by = c(
    "site", "site_type", "zone", "date", "transect",
    "depth", "depth_units", "species"
  )
) %>%
  arrange(site, zone, date, transect, species)


################################################################################
#Step 5: convert the combined table to wide format

urch_den_build1_wide <- urch_den_build1 %>%
  pivot_wider(
    id_cols = c(
      site, site_type, zone, date, transect,
      depth, depth_units
    ),
    names_from = species,
    values_from = c(
      total_urchins,
      density_source,
      density_urchins_m2,
      concealed_sample_n,
      exposed_sample_n,
      behavior_sample_n,
      proportion_concealed,
      proportion_exposed,
      estimated_concealed_density_m2,
      estimated_exposed_density_m2,
      density_cracked_tests_m2,
      density_whole_tests_m2
    ),
    names_glue = "{.value}_{species}"
  ) %>%
  arrange(site, zone, date, transect) %>%
  #Replace numeric NAs with true zeros
  mutate(
    across(
      where(is.numeric),
      ~ tidyr::replace_na(.x, 0)
    )
  ) %>%
  clean_names()


#Remove Unknown columns that are not test-density measures, as well as
#density_source columns for Purple and Red.

urch_den_build1_wide <- urch_den_build1_wide %>%
  dplyr::select(
    -matches(
      "^(total_urchins|density_source|density_urchins_m2|concealed_sample_n|exposed_sample_n|behavior_sample_n|proportion_concealed|proportion_exposed|estimated_concealed_density_m2|estimated_exposed_density_m2)_unknown$",
      ignore.case = TRUE
    ),
    -matches(
      "^density_source_(purple|red)$",
      ignore.case = TRUE
    )
  ) %>%
  clean_names()


################################################################################
#Check that the filtered live urchin source and density table have the same
#transects

urch_build1_keys <- urch_build1 %>%
  distinct(date, site, transect)

urch_den_build1_keys <- urch_den_live_build1 %>%
  distinct(date, site, transect)


#Compare number of unique transect combinations and sites

tibble(
  dataset = c("urch_build1", "urch_den_live_build1"),
  unique_date_site_transects = c(
    nrow(urch_build1_keys),
    nrow(urch_den_build1_keys)
  ),
  unique_sites = c(
    n_distinct(urch_build1$site),
    n_distinct(urch_den_live_build1$site)
  )
)


#Transects present in filtered source but missing from live density table

anti_join(
  urch_build1_keys,
  urch_den_build1_keys,
  by = c("date", "site", "transect")
)


#Transects present in live density table but not in filtered source

anti_join(
  urch_den_build1_keys,
  urch_build1_keys,
  by = c("date", "site", "transect")
)


################################################################################
#Step 6: csize frequency 


urch_sizefreq_build1 <- urch_build1 %>%
  filter(
    str_to_lower(species) %in% c("purple", "red"),
    behavior_clean %in% c("concealed", "exposed"),
    !is.na(size),
    str_trim(size) != ""
  ) %>%
  mutate(
    size = as.numeric(size)
  ) %>%
  group_by(
    site, site_type, zone, date, transect,
    depth, depth_units, species, size
  ) %>%
  summarise(
    size_count = sum(count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(
    urch_den_live_build1 %>%
      filter(str_to_lower(species) %in% c("purple", "red")) %>%
      dplyr::select(
        site, site_type, zone, date, transect,
        depth, depth_units, species,
        total_urchins,
        density_urchins_m2
      ),
    by = c(
      "site", "site_type", "zone", "date", "transect",
      "depth", "depth_units", "species"
    )
  ) %>%
  rename(
    total_density_urchins_m2 = density_urchins_m2
  ) %>%
  arrange(site, zone, date, transect, species, size)



###############################################################################
#Export
processed_dir <- "/Volumes/enhydra/data/kelp_productivity/benthic_surveys/processed"

if (!dir.exists(processed_dir)) {
  stop("Processed directory does not exist or the volume is not mounted.")
}

processed_tables <- list(
  urch_den_build1_wide = urch_den_build1_wide,
  urch_sizefreq_build1 = urch_sizefreq_build1
)

saveRDS(
  processed_tables,
  file = file.path(
    processed_dir,
    "urchin_density_and_size_frequency_build1.rds"
  )
)



