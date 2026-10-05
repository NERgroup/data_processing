################################################################################
# About
# data processing script written by JG.Smith jogsmith@ucsc.edu
#
#context: data were entered twice. Compare the two entries and identify:
#1: different values for the same observation
#2: rows without a matching observation in the other entry
#3: duplicate or incomplete observation identifiers
#
#output: one reconciliation workbook uploaded to Google Drive

################################################################################
# required packages and source locations

rm(list=ls())

librarian::shelf(tidyverse, janitor, googlesheets4,
                 googledrive, openxlsx)

entry1_url <- "https://docs.google.com/spreadsheets/d/1QyCRsJg1Ms67hdSb8kzT-qAqKs5ccG7BT6sj76FHa94/edit"

entry2_url <- "https://docs.google.com/spreadsheets/d/1z_OXoZSBLXZTalicFg8h1wVEEMNxngOkJbWDzT3RsvE/edit"

output_folder <- "1x3fpMPRhnRoM6u8dY1jGUpaX8kKZhDUC"

datdir <- file.path(getwd(), "data_reconciliation")
dir.create(datdir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# read first and second swath_urchin entries
# raw = first entry; qc = second entry

urchin_raw <- read_sheet(
  entry1_url,
  sheet = "swath_urchin",
  col_types = "c"
) %>%
  clean_names()

urchin_qc <- read_sheet(
  entry2_url,
  sheet = "swath_urchin",
  col_types = "c"
) %>%
  clean_names()

################################################################################
# step 1: prepare data
#
#retain original sheet row numbers; headers occupy row 1
#select survey columns, excluding enterer names and extra annotation columns
#keep values as text so invalid entries are not converted silently to NA
#trim outer whitespace and treat empty cells as NA

urch_raw_build1 <- urchin_raw %>%
  mutate(sheet_row = row_number() + 1L) %>%
  dplyr::select(
    sheet_row, site, site_type, zone, date, observer, buddy,
    transect, depth, depth_units, species, size, count
  ) %>%
  mutate(
    across(
      -sheet_row,
      ~ na_if(str_trim(as.character(.x)), "")
    )
  ) %>%
  filter(if_any(-sheet_row, ~ !is.na(.x))) %>%
  arrange(site, zone, date, transect, species, size)

urch_qc_build1 <- urchin_qc %>%
  mutate(sheet_row = row_number() + 1L) %>%
  dplyr::select(
    sheet_row, site, site_type, zone, date, observer, buddy,
    transect, depth, depth_units, species, size, count
  ) %>%
  mutate(
    across(
      -sheet_row,
      ~ na_if(str_trim(as.character(.x)), "")
    )
  ) %>%
  filter(if_any(-sheet_row, ~ !is.na(.x))) %>%
  arrange(site, zone, date, transect, species, size)

################################################################################
# step 2: define observation identifiers
#
#each row represents one species-size class within a site/zone/date/transect
#depth, site_type, observer, buddy, and count will be compared as values

urch_keys <- c(
  "site", "zone", "date", "transect", "species", "size"
)

################################################################################
# step 3: identify duplicate or incomplete observation keys
#do not drop duplicate records or arbitrarily pair them

urch_duplicate_keys <- bind_rows(
  urch_raw_build1 %>%
    count(across(all_of(urch_keys)), name = "n_rows") %>%
    filter(n_rows > 1) %>%
    dplyr::select(all_of(urch_keys)),
  
  urch_qc_build1 %>%
    count(across(all_of(urch_keys)), name = "n_rows") %>%
    filter(n_rows > 1) %>%
    dplyr::select(all_of(urch_keys))
) %>%
  distinct()

#Include all records for a problematic key from BOTH entries.
urch_key_review <- bind_rows(
  urch_raw_build1 %>%
    mutate(entry = "first"),
  
  urch_qc_build1 %>%
    mutate(entry = "second")
) %>%
  left_join(
    urch_duplicate_keys %>% mutate(duplicate_key = TRUE),
    by = urch_keys
  ) %>%
  mutate(
    issue = case_when(
      if_any(all_of(urch_keys), ~ is.na(.x)) ~
        "Incomplete observation key",
      !is.na(duplicate_key) ~
        "Key duplicated in at least one entry",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(issue)) %>%
  dplyr::select(issue, entry, sheet_row, everything(), -duplicate_key) %>%
  arrange(site, zone, date, transect, species, size, entry) %>%
  mutate(resolved = "")

################################################################################
# step 4: set aside problematic keys before joining
#these records remain available in urch_key_review

urch_raw_build2 <- urch_raw_build1 %>%
  filter(if_all(all_of(urch_keys), ~ !is.na(.x))) %>%
  anti_join(urch_duplicate_keys, by = urch_keys)

urch_qc_build2 <- urch_qc_build1 %>%
  filter(if_all(all_of(urch_keys), ~ !is.na(.x))) %>%
  anti_join(urch_duplicate_keys, by = urch_keys)

################################################################################
# step 5: join entries and classify matched versus unmatched rows

urch_joined <- full_join(
  urch_raw_build2,
  urch_qc_build2,
  by = urch_keys,
  suffix = c("_raw", "_qc"),
  relationship = "one-to-one"
) %>%
  mutate(
    status = case_when(
      is.na(sheet_row_raw) ~ "Only in second entry",
      is.na(sheet_row_qc) ~ "Only in first entry",
      TRUE ~ "Matched observation"
    )
  )

################################################################################
# step 6: identify WHAT differs
#
#format: first entry ≠ second entry
#[blank] means the row exists but the cell is empty
#[no row] means the observation has no exact counterpart

urch_discrep_values <- urch_joined %>%
  mutate(
    across(
      all_of(c(
        "site_type_raw", "observer_raw", "buddy_raw",
        "depth_raw", "depth_units_raw", "count_raw"
      )),
      ~ case_when(
        is.na(sheet_row_raw) ~
          paste(
            "[no row]", "≠",
            coalesce(
              get(str_replace(cur_column(), "_raw$", "_qc")),
              "[blank]"
            )
          ),
        
        is.na(sheet_row_qc) ~
          paste(coalesce(.x, "[blank]"), "≠", "[no row]"),
        
        #Catch unequal nonmissing values AND blank-versus-value differences.
        coalesce(
          .x != get(str_replace(cur_column(), "_raw$", "_qc")),
          FALSE
        ) |
          xor(
            is.na(.x),
            is.na(get(str_replace(cur_column(), "_raw$", "_qc")))
          ) ~
          paste(
            coalesce(.x, "[blank]"), "≠",
            coalesce(
              get(str_replace(cur_column(), "_raw$", "_qc")),
              "[blank]"
            )
          ),
        
        TRUE ~ NA_character_
      ),
      .names = "{.col}_diff"
    )
  ) %>%
  rename_with(
    ~ str_replace(.x, "_raw_diff$", "_diff"),
    ends_with("_raw_diff")
  ) %>%
  filter(
    status != "Matched observation" |
      if_any(ends_with("_diff"), ~ !is.na(.x))
  ) %>%
  mutate(
    status = if_else(
      status == "Matched observation",
      "Value discrepancy",
      status
    )
  ) %>%
  dplyr::select(
    status, sheet_row_raw, sheet_row_qc,
    all_of(urch_keys), ends_with("_diff")
  ) %>%
  arrange(site, zone, date, transect, species, size) %>%
  mutate(resolved = "")

################################################################################
# step 7: inspect results

urch_discrep_values %>%
  count(status) %>%
  print()

View(urch_discrep_values)
View(urch_key_review)

################################################################################
# step 8: save one workbook with two review tabs

wb <- createWorkbook()

addWorksheet(wb, "swath_urchin")
writeData(
  wb, "swath_urchin", urch_discrep_values,
  withFilter = nrow(urch_discrep_values) > 0
)
freezePane(wb, "swath_urchin", firstRow = TRUE)
setColWidths(
  wb, "swath_urchin",
  cols = seq_len(ncol(urch_discrep_values)),
  widths = 22
)

addWorksheet(wb, "urchin_key_review")
writeData(
  wb, "urchin_key_review", urch_key_review,
  withFilter = nrow(urch_key_review) > 0
)
freezePane(wb, "urchin_key_review", firstRow = TRUE)
setColWidths(
  wb, "urchin_key_review",
  cols = seq_len(ncol(urch_key_review)),
  widths = 22
)

#Use a new filename each run to preserve previous review work.
output_file <- file.path(
  datdir,
  paste0(
    "benthic_reconciliation_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".xlsx"
  )
)

saveWorkbook(wb, output_file, overwrite = FALSE)

################################################################################
# step 9: upload to data_reconciliation folder

uploaded_report <- drive_upload(
  media = output_file,
  path = as_id(output_folder),
  overwrite = TRUE
)

uploaded_report
