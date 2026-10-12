# jogmsith@ucsc.edu

rm(list = ls())


################################################################################
# Load packages and data

librarian::shelf(tidyverse, janitor, googlesheets4)

spreadsheet_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "129OyWLuE64lXgXDu7VDU1EHLWpjZ0C5bverqpEhO9S8/edit"
)

kelp_raw <- read_sheet(
  spreadsheet_url,
  sheet = "swath_kelp",
  skip = 4,
  col_types = "c"
) %>%
  janitor::clean_names()


################################################################################
# Load transect keys from the processed urchin density table

processed_dir <- paste0(
  "/Volumes/enhydra/data/kelp_productivity/",
  "benthic_surveys/processed"
)

urchin_tables <- readRDS(
  file.path(
    processed_dir,
    "urchin_density_and_size_frequency_build1.rds"
  )
)

if (!"urch_den_build1_wide" %in% names(urchin_tables)) {
  stop("urch_den_build1_wide was not found in the saved RDS file.")
}

# These keys define the transects to include in the kelp table.
kelp_transect_keys <- urchin_tables$urch_den_build1_wide %>%
  dplyr::select(site, site_type, zone, date, transect) %>%
  mutate(
    across(
      c(site, site_type, zone, date, transect),
      ~ str_trim(as.character(.x))
    )
  ) %>%
  distinct()


################################################################################
# Clean kelp source data

kelp_build1 <- kelp_raw %>%
  mutate(
    across(
      c(site, site_type, zone, date, transect),
      ~ str_trim(as.character(.x))
    ),
    species_clean = str_to_upper(str_trim(species)),
    count_num = readr::parse_number(count),
    stipe_count_num =
      readr::parse_number(stipe_counts_macrocystis_only)
  )


################################################################################
# Identify transects with no kelp records

kelp_source_keys <- kelp_build1 %>%
  distinct(site, site_type, zone, date, transect)

kelp_transects_without_records <- kelp_transect_keys %>%
  anti_join(
    kelp_source_keys,
    by = c("site", "site_type", "zone", "date", "transect")
  )

# Check for kelp source transects not present in the urchin density table.
kelp_transects_not_in_urchin <- kelp_source_keys %>%
  anti_join(
    kelp_transect_keys,
    by = c("site", "site_type", "zone", "date", "transect")
  )


################################################################################
# Macrocystis: plant count, plant density, stipe density, and mean plant size

kelp_macrocystis <- kelp_build1 %>%
  filter(species_clean == "MACPYR") %>%
  group_by(site, site_type, zone, date, transect) %>%
  summarise(
    macrocystis_plant_count = sum(count_num, na.rm = TRUE),
    macrocystis_plant_density_m2 =
      macrocystis_plant_count / 20,
    macrocystis_stipe_density_m2 =
      sum(stipe_count_num, na.rm = TRUE) / 20,
    macrocystis_mean_stipes_per_plant = if (
      all(is.na(stipe_count_num))
    ) {
      NA_real_
    } else {
      mean(stipe_count_num, na.rm = TRUE)
    },
    .groups = "drop"
  )


################################################################################
# Other kelp species: density

kelp_other_density <- kelp_build1 %>%
  filter(
    !is.na(species_clean),
    species_clean != "MACPYR",
    !species_clean %in% c("UPC_BROWN", "UPC_RED")
  ) %>%
  group_by(site, site_type, zone, date, transect, species_clean) %>%
  summarise(
    density_m2 = sum(count_num, na.rm = TRUE) / 20,
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = species_clean,
    values_from = density_m2,
    names_glue = "density_{species_clean}"
  )


################################################################################
# UPC categories: percentage cover from 20 points per transect

kelp_upc_counts <- kelp_build1 %>%
  filter(species_clean %in% c("UPC_BROWN", "UPC_RED")) %>%
  group_by(site, site_type, zone, date, transect, species_clean) %>%
  summarise(
    upc_points = sum(count_num, na.rm = TRUE),
    .groups = "drop"
  )

# Cross each expected transect with both UPC categories so both columns
# are present, including when a category has zero recorded points.
kelp_upc_cover <- kelp_transect_keys %>%
  tidyr::crossing(
    species_clean = c("UPC_BROWN", "UPC_RED")
  ) %>%
  left_join(
    kelp_upc_counts,
    by = c(
      "site", "site_type", "zone", "date", "transect",
      "species_clean"
    )
  ) %>%
  mutate(
    upc_points = tidyr::replace_na(upc_points, 0),
    percent_cover = upc_points / 20 * 100
  ) %>%
  pivot_wider(
    names_from = species_clean,
    values_from = c(upc_points, percent_cover),
    names_glue = "{.value}_{species_clean}"
  )


################################################################################
# Combine measures into one row per expected transect

kelp_transect_build1 <- kelp_transect_keys %>%
  left_join(
    kelp_macrocystis,
    by = c("site", "site_type", "zone", "date", "transect")
  ) %>%
  left_join(
    kelp_other_density,
    by = c("site", "site_type", "zone", "date", "transect")
  ) %>%
  left_join(
    kelp_upc_cover,
    by = c("site", "site_type", "zone", "date", "transect")
  ) %>%
  janitor::clean_names() %>%
  mutate(
    across(
      where(is.numeric),
      ~ tidyr::replace_na(.x, 0)
    ),
    # A mean plant size is undefined when no Macrocystis plants were recorded.
    macrocystis_mean_stipes_per_plant = if_else(
      macrocystis_plant_count == 0,
      NA_real_,
      macrocystis_mean_stipes_per_plant
    )
  ) %>%
  arrange(site, zone, date, transect)


################################################################################
# Checks

# Confirm the kelp table has one row for every transect in the urchin table.
tibble(
  dataset = c("urchin density keys", "kelp transect table"),
  transect_rows = c(
    nrow(kelp_transect_keys),
    nrow(kelp_transect_build1)
  )
)

# These are the expected transects that had no rows in the kelp source.
kelp_transects_without_records

# These should be empty if every kelp source transect is in the urchin table.
kelp_transects_not_in_urchin

# UPC point totals above 20 need review.
upc_points_over_20 <- kelp_transect_build1 %>%
  filter(
    upc_points_upc_brown > 20 |
      upc_points_upc_red > 20
  )

upc_points_over_20

################################################################################
#Export

saveRDS(
  kelp_transect_build1,
  file = file.path(processed_dir, "kelp_transect_build1.rds")
)




