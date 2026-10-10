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
#Step 1: clean up data

#Keep blank-behavior count rows and concealed/exposed behavior rows only.
#Exclude Unknown species and other behavior categories, which are used for
#cracked-test data.

urch_build1 <- urch_raw %>%
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
#Step 2: process urchin density data

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


#Calculate urchin density and add behavior proportions and estimated
#behavior-specific densities.

urch_den_build1 <- urch_build1 %>%
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
#Check that the filtered source and density table have the same transects

urch_build1_keys <- urch_build1 %>%
  distinct(date, site, transect)

urch_den_build1_keys <- urch_den_build1 %>%
  distinct(date, site, transect)


#Compare number of unique transect combinations and sites

tibble(
  dataset = c("urch_build1", "urch_den_build1"),
  unique_date_site_transects = c(
    nrow(urch_build1_keys),
    nrow(urch_den_build1_keys)
  ),
  unique_sites = c(
    n_distinct(urch_build1$site),
    n_distinct(urch_den_build1$site)
  )
)


#Transects present in filtered source but missing from density table

anti_join(
  urch_build1_keys,
  urch_den_build1_keys,
  by = c("date", "site", "transect")
)


#Transects present in density table but not in filtered source

anti_join(
  urch_den_build1_keys,
  urch_build1_keys,
  by = c("date", "site", "transect")
)
