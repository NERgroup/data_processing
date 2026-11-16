# jogmsith@ucsc.edu

rm(list = ls())


################################################################################
# Load packages and data

librarian::shelf(tidyverse, janitor, googlesheets4)

spreadsheet_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "129OyWLuE64lXgXDu7VDU1EHLWpjZ0C5bverqpEhO9S8/edit"
)

tag_spreadsheet_url <- paste0(
  "https://docs.google.com/spreadsheets/d/",
  "1ikbOQ3F9M2cC1F147trq3WBwehQ33cOBsCZ4tVKkBmQ/",
  "edit#gid=1596972084"
)

tag_raw <- googlesheets4::read_sheet(
  tag_spreadsheet_url,
  sheet = "cohort_tracking",
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
# Prepare tag dates by site and color

tag_cohort_events <- tag_raw %>%
  dplyr::select(
    site_name, site_type_2025, zone, tag_date, tag_color
  ) %>%
  mutate(
    site = recode(str_trim(site_name), "KF01DC" = "KF03S"),
    site_type = case_when(
      str_to_lower(str_trim(site_type_2025)) %in% c("forest", "forrest") ~
        "FOR",
      str_to_lower(str_trim(site_type_2025)) == "barren" ~ "BAR",
      TRUE ~ NA_character_
    ),
    zone = str_to_title(str_trim(zone)),
    tag_date = as.Date(tag_date, format = "%Y%m%d"),
    tag_color = str_to_lower(str_trim(tag_color))
  ) %>%
  dplyr::select(site, site_type, zone, tag_date, tag_color)

if (anyNA(tag_cohort_events$site_type) || anyNA(tag_cohort_events$tag_date)) {
  stop("Tag records contain an unrecognized site type or missing tag date.")
}

if (anyDuplicated(
  tag_cohort_events[c("site", "site_type", "zone", "tag_color")]
)) {
  stop("More than one tag date exists for a site, site type, zone, and color.")
}

tag_cohort_dates <- tag_cohort_events %>%
  tidyr::pivot_wider(
    id_cols = c(site, site_type, zone),
    names_from = tag_color,
    values_from = tag_date,
    names_glue = "{tag_color}_date"
  )


################################################################################
# Select and clean frond data

fronds_build1 <- fronds_raw %>%
  dplyr::select(
    site, site_type, zone, date, depth_units, depth, plant_id,
    total_frond, number_grn, number_wht, number_ylw, number_blk,
    frond, frond_length, damaged_stipe
  ) %>%
  mutate(
    site = recode(site, "KF01DC" = "KF03S"),
    across(
      c(depth, total_frond, number_grn, number_wht, number_ylw, number_blk),
      readr::parse_number
    ),
    depth = if_else(
      str_to_lower(str_trim(depth_units)) %in% c("feet", "foot", "ft"),
      depth * 0.3048,
      depth
    ),
    total_frond = if_else(
      is.na(total_frond),
      rowSums(
        pick(number_grn, number_wht, number_ylw, number_blk),
        na.rm = TRUE
      ),
      total_frond
    )
  ) %>%
  dplyr::select(-depth_units) %>%
  left_join(
    tag_cohort_dates,
    by = c("site", "site_type", "zone"),
    relationship = "many-to-one"
  )

################################################################################
# Calculate net productivity by plant and visit

plant_visit_profiles <- fronds_build1 %>%
  # Exclude the duplicate NABR01 survey entry selected for review.
  filter(!(site == "NABR01" & date == "20260917")) %>%
  dplyr::select(
    site, site_type, zone, date, plant_id, total_frond,
    number_grn, number_wht, number_ylw, number_blk
  ) %>%
  distinct()

ambiguous_plant_visits <- plant_visit_profiles %>%
  count(site, site_type, zone, date, plant_id, name = "count_profiles") %>%
  filter(count_profiles > 1)

plant_productivity <- plant_visit_profiles %>%
  anti_join(
    ambiguous_plant_visits,
    by = c("site", "site_type", "zone", "date", "plant_id")
  ) %>%
  dplyr::select(
    site, site_type, zone, date, plant_id, total_frond
  ) %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  arrange(site, site_type, zone, plant_id, date) %>%
  group_by(site, site_type, zone, plant_id) %>%
  mutate(
    previous_date = lag(date),
    previous_total_frond = lag(total_frond),
    interval_days = as.integer(date - previous_date),
    net_frond_change = total_frond - previous_total_frond,
    net_productivity_per_30_days =
      net_frond_change / interval_days * 30
  ) %>%
  ungroup() %>%
  filter(!is.na(net_productivity_per_30_days)) %>%
  mutate(month = factor(format(date, "%b"), levels = month.abb))

ggplot(
  plant_productivity,
  aes(x = month, y = net_productivity_per_30_days)
) +
  geom_hline(yintercept = 0) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(color = site),
    width = 0.12,
    height = 0,
    alpha = 0.7
  ) +
  labs(
    x = "Survey month",
    y = "Net frond change per plant per 30 days",
    color = "Site"
  )


################################################################################
# Estimate apparent frond exports by plant and visit

plant_exports <- plant_visit_profiles %>%
  anti_join(
    ambiguous_plant_visits,
    by = c("site", "site_type", "zone", "date", "plant_id")
  ) %>%
  mutate(date = as.Date(date, format = "%Y%m%d")) %>%
  arrange(site, site_type, zone, plant_id, date) %>%
  group_by(site, site_type, zone, plant_id) %>%
  mutate(
    previous_date = lag(date),
    interval_days = as.integer(date - previous_date),
    across(
      c(number_grn, number_wht, number_ylw, number_blk),
      ~ pmax(lag(.x) - .x, 0),
      .names = "export_{.col}"
    ),
    exported_fronds = rowSums(
      pick(starts_with("export_number_")),
      na.rm = TRUE
    ),
    exports_per_30_days = exported_fronds / interval_days * 30
  ) %>%
  ungroup() %>%
  filter(!is.na(exports_per_30_days)) %>%
  mutate(month = factor(format(date, "%b"), levels = month.abb))

ggplot(
  plant_exports,
  aes(x = month, y = exports_per_30_days)
) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(
    aes(color = site),
    width = 0.12,
    height = 0,
    alpha = 0.7
  ) +
  labs(
    x = "Survey month",
    y = "Apparent frond export per plant per 30 days",
    color = "Site"
  )
