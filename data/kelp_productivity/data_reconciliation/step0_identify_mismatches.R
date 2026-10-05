#


rm(list=ls())

################################################################################
# required packages and source spreadsheets

librarian::shelf(tidyverse, janitor, googlesheets4,
                 googledrive, openxlsx)

entry1_url <- "https://docs.google.com/spreadsheets/d/129OyWLuE64lXgXDu7VDU1EHLWpjZ0C5bverqpEhO9S8/edit"

entry2_url <- "https://docs.google.com/spreadsheets/d/1KVh9ao3k6Ac8ItIlsXPL3UYnrqQ1Cwtfn0Viqt785W8/edit"

################################################################################
# read first entries
#headers are on row 5

urchin_raw <- read_sheet(
  entry1_url,
  sheet = "swath_urchin_size",
  skip = 4,
  col_types = "c"
) %>%
  clean_names()

kelp_raw <- read_sheet(
  entry1_url,
  sheet = "swath_kelp",
  skip = 4,
  col_types = "c"
) %>%
  clean_names()

frond_raw <- read_sheet(
  entry1_url,
  sheet = "fronds_pull_down_long",
  skip = 4,
  col_types = "c"
) %>%
  clean_names()

################################################################################
# read second entries
#urchin and kelp headers are on row 1; frond headers are on row 5

urchin_qc <- read_sheet(
  entry2_url,
  sheet = "swath_urchin_size",
  col_types = "c"
) %>%
  clean_names()

kelp_qc <- read_sheet(
  entry2_url,
  sheet = "swath_kelp",
  col_types = "c"
) %>%
  clean_names()

frond_qc <- read_sheet(
  entry2_url,
  sheet = "fronds_pull_down_long",
  skip = 4,
  col_types = "c"
) %>%
  clean_names()

################################################################################
# verify that actual column headers were imported

stopifnot(
  all(c("site", "behavior", "size", "count") %in% names(urchin_raw)),
  all(c("site", "behavior", "size", "count") %in% names(urchin_qc)),
  all(c("site", "species", "count") %in% names(kelp_raw)),
  all(c("site", "species", "count") %in% names(kelp_qc)),
  all(c("site", "plant_id", "frond", "frond_length") %in% names(frond_raw)),
  all(c("site", "plant_id", "frond", "frond_length") %in% names(frond_qc))
)

################################################################################
# display corrected headers and second-entry examples

list(
  urchin_raw = names(urchin_raw),
  urchin_qc = names(urchin_qc),
  kelp_raw = names(kelp_raw),
  kelp_qc = names(kelp_qc),
  frond_raw = names(frond_raw),
  frond_qc = names(frond_qc)
)

urchin_qc %>% slice_head(n = 10) %>% print(width = Inf)
kelp_qc %>% slice_head(n = 10) %>% print(width = Inf)
frond_qc %>% slice_head(n = 10) %>% print(width = Inf)





################################################################################
# output settings

output_folder <- "1hXRqPf8Ii9SjU-HetVE7M-MDFK0F7Ctw"

datdir <- file.path(getwd(), "data_reconciliation")
dir.create(datdir, recursive = TRUE, showWarnings = FALSE)

################################################################################
# prepare entries
#retain sheet row numbers and remove only completely empty survey rows
#values remain text; trim whitespace without discarding invalid entries

prepare_entry <- function(dat, columns, header_row) {
  
  dat %>%
    mutate(sheet_row = row_number() + header_row) %>%
    dplyr::select(sheet_row, all_of(columns)) %>%
    mutate(
      across(
        all_of(columns),
        ~ na_if(str_trim(as.character(.x)), "")
      )
    ) %>%
    filter(if_any(all_of(columns), ~ !is.na(.x)))
}

################################################################################
# step 1: prepare urchin entries

urch_columns <- c(
  "site", "site_type", "zone", "date", "observer", "buddy",
  "transect", "depth", "depth_units", "species", "behavior",
  "size", "meter_sizing_stopped", "count"
)

urch_keys <- c(
  "site", "zone", "date", "transect", "species",
  "record_type", "behavior", "size"
)

urch_fields <- c(
  "site_type", "observer", "buddy", "depth", "depth_units",
  "meter_sizing_stopped", "count"
)

urch_raw_build1 <- prepare_entry(urchin_raw, urch_columns, 5) %>%
  mutate(
    #Standardize the two observed spellings of concealed behavior.
    behavior = case_when(
      str_to_lower(behavior) %in% c("conceiled", "concealed") ~
        "Concealed",
      TRUE ~ behavior
    ),
    record_type = if_else(
      is.na(size) & !is.na(meter_sizing_stopped),
      "Remaining count",
      "Size class"
    ),
    key_problem =
      if_any(
        all_of(c("site", "zone", "date", "transect", "species")),
        ~ is.na(.x)
      ) |
      (record_type == "Size class" &
         (is.na(size) | is.na(behavior)))
  )

urch_qc_build1 <- prepare_entry(urchin_qc, urch_columns, 1) %>%
  mutate(
    behavior = case_when(
      str_to_lower(behavior) %in% c("conceiled", "concealed") ~
        "Concealed",
      TRUE ~ behavior
    ),
    record_type = if_else(
      is.na(size) & !is.na(meter_sizing_stopped),
      "Remaining count",
      "Size class"
    ),
    key_problem =
      if_any(
        all_of(c("site", "zone", "date", "transect", "species")),
        ~ is.na(.x)
      ) |
      (record_type == "Size class" &
         (is.na(size) | is.na(behavior)))
  )

################################################################################
# step 2: prepare kelp entries
#multiple plant records within a transect/species are legitimate

kelp_columns <- c(
  "site", "site_type", "zone", "date", "observer", "buddy",
  "transect", "depth", "depth_units", "species",
  "stipe_counts_macrocystis_only", "count", "subsample_meter"
)

kelp_keys <- c("site", "zone", "date", "transect", "species")

kelp_fields <- c(
  "site_type", "observer", "buddy", "depth", "depth_units",
  "stipe_counts_macrocystis_only", "count", "subsample_meter"
)

kelp_raw_build1 <- prepare_entry(kelp_raw, kelp_columns, 5) %>%
  mutate(
    key_problem = if_any(all_of(kelp_keys), ~ is.na(.x))
  )

kelp_qc_build1 <- prepare_entry(kelp_qc, kelp_columns, 1) %>%
  mutate(
    key_problem = if_any(all_of(kelp_keys), ~ is.na(.x))
  )

################################################################################
# step 3: prepare frond entries

frond_columns <- c(
  "site", "site_type", "zone", "date", "observer", "buddy",
  "depth_units", "plant_id", "depth", "time",
  "number_grn", "number_white", "number_ylw",
  "frond", "frond_length", "damaged_stipe", "notes"
)

frond_keys <- c("site", "zone", "date", "plant_id", "frond")

frond_fields <- c(
  "site_type", "observer", "buddy", "depth_units", "depth",
  "time", "number_grn", "number_white", "number_ylw",
  "frond_length", "damaged_stipe", "notes"
)

frond_raw_build1 <- prepare_entry(frond_raw, frond_columns, 5) %>%
  mutate(
    key_problem = if_any(all_of(frond_keys), ~ is.na(.x))
  )

frond_qc_build1 <- prepare_entry(frond_qc, frond_columns, 5) %>%
  mutate(
    key_problem = if_any(all_of(frond_keys), ~ is.na(.x))
  )

################################################################################
# step 4: compare entries
#
#urchin/fronds: duplicate observation keys require review
#kelp: match identical records first, including repeated identical records
#never pair ambiguous remaining groups by row order

compare_entries <- function(raw, qc, keys, fields,
                            repeated_records = FALSE) {
  
  #Set aside incomplete keys.
  review <- bind_rows(
    raw %>%
      filter(key_problem) %>%
      mutate(entry = "first", issue = "Incomplete observation key"),
    
    qc %>%
      filter(key_problem) %>%
      mutate(entry = "second", issue = "Incomplete observation key")
  )
  
  a <- raw %>%
    filter(!key_problem) %>%
    dplyr::select(-key_problem)
  
  b <- qc %>%
    filter(!key_problem) %>%
    dplyr::select(-key_problem)
  
  exact_pairs <- 0L
  
  if (!repeated_records) {
    
    #Flag duplicate keys and retain counterparts from both entries.
    duplicate_keys <- bind_rows(
      a %>%
        count(across(all_of(keys))) %>%
        filter(n > 1) %>%
        dplyr::select(all_of(keys)),
      
      b %>%
        count(across(all_of(keys))) %>%
        filter(n > 1) %>%
        dplyr::select(all_of(keys))
    ) %>%
      distinct()
    
    review <- bind_rows(
      review,
      a %>%
        semi_join(duplicate_keys, by = keys) %>%
        mutate(entry = "first", issue = "Duplicate observation key"),
      b %>%
        semi_join(duplicate_keys, by = keys) %>%
        mutate(entry = "second", issue = "Duplicate observation key")
    )
    
    a <- anti_join(a, duplicate_keys, by = keys)
    b <- anti_join(b, duplicate_keys, by = keys)
    
  } else {
    
    #The occurrence number distinguishes identical repeated records.
    #It is NOT used to pair different plant records.
    exact_columns <- c(keys, fields)
    
    a_exact <- a %>%
      group_by(across(all_of(exact_columns))) %>%
      mutate(occurrence = row_number()) %>%
      ungroup()
    
    b_exact <- b %>%
      group_by(across(all_of(exact_columns))) %>%
      mutate(occurrence = row_number()) %>%
      ungroup()
    
    exact_matches <- inner_join(
      a_exact,
      b_exact,
      by = c(exact_columns, "occurrence"),
      suffix = c("_raw", "_qc"),
      relationship = "one-to-one"
    )
    
    exact_pairs <- nrow(exact_matches)
    
    a <- a %>%
      filter(!sheet_row %in% exact_matches$sheet_row_raw)
    
    b <- b %>%
      filter(!sheet_row %in% exact_matches$sheet_row_qc)
    
    #If multiple unmatched records remain on both sides, pairing is ambiguous.
    ambiguous_keys <- full_join(
      a %>% count(across(all_of(keys)), name = "n_raw"),
      b %>% count(across(all_of(keys)), name = "n_qc"),
      by = keys
    ) %>%
      mutate(
        n_raw = replace_na(n_raw, 0L),
        n_qc = replace_na(n_qc, 0L)
      ) %>%
      filter(n_raw > 0, n_qc > 0, n_raw > 1 | n_qc > 1) %>%
      dplyr::select(all_of(keys))
    
    review <- bind_rows(
      review,
      a %>%
        semi_join(ambiguous_keys, by = keys) %>%
        mutate(entry = "first", issue = "Ambiguous remaining plant records"),
      b %>%
        semi_join(ambiguous_keys, by = keys) %>%
        mutate(entry = "second", issue = "Ambiguous remaining plant records")
    )
    
    a <- anti_join(a, ambiguous_keys, by = keys)
    b <- anti_join(b, ambiguous_keys, by = keys)
  }
  
  #Remaining shared keys have at most one row on each side.
  #One-sided kelp groups can still contain several unmatched rows.
  joined <- full_join(
    a, b,
    by = keys,
    suffix = c("_raw", "_qc"),
    relationship = if (repeated_records) "many-to-many" else "one-to-one"
  ) %>%
    mutate(
      status = case_when(
        is.na(sheet_row_raw) ~ "Only in second entry",
        is.na(sheet_row_qc) ~ "Only in first entry",
        TRUE ~ "Matched observation"
      )
    )
  
  #Create a difference column for each compared field.
  for (field in fields) {
    
    value_raw <- joined[[paste0(field, "_raw")]]
    value_qc <- joined[[paste0(field, "_qc")]]
    
    different <- coalesce(value_raw != value_qc, FALSE) |
      xor(is.na(value_raw), is.na(value_qc))
    
    joined[[paste0(field, "_diff")]] <- case_when(
      is.na(joined$sheet_row_raw) ~
        paste("[no row]", "≠", coalesce(value_qc, "[blank]")),
      
      is.na(joined$sheet_row_qc) ~
        paste(coalesce(value_raw, "[blank]"), "≠", "[no row]"),
      
      different ~
        paste(
          coalesce(value_raw, "[blank]"), "≠",
          coalesce(value_qc, "[blank]")
        ),
      
      TRUE ~ NA_character_
    )
  }
  
  exact_pairs <- exact_pairs + joined %>%
    filter(
      status == "Matched observation",
      if_all(ends_with("_diff"), ~ is.na(.x))
    ) %>%
    nrow()
  
  report <- joined %>%
    filter(
      status != "Matched observation" |
        if_any(ends_with("_diff"), ~ !is.na(.x))
    ) %>%
    mutate(
      status = if_else(
        status == "Matched observation",
        if (repeated_records) {
          "Value discrepancy: sole remaining pair"
        } else {
          "Value discrepancy"
        },
        status
      ),
      resolved = ""
    ) %>%
    dplyr::select(
      status, sheet_row_raw, sheet_row_qc,
      all_of(keys), ends_with("_diff"), resolved
    ) %>%
    arrange(across(all_of(keys)))
  
  review <- review %>%
    dplyr::select(
      issue, entry, sheet_row, all_of(keys), all_of(fields)
    ) %>%
    arrange(across(all_of(keys)), entry, sheet_row) %>%
    mutate(resolved = "")
  
  #Ensure every prepared source row is accounted for.
  stopifnot(
    nrow(raw) ==
      exact_pairs +
      sum(!is.na(report$sheet_row_raw)) +
      sum(review$entry == "first"),
    
    nrow(qc) ==
      exact_pairs +
      sum(!is.na(report$sheet_row_qc)) +
      sum(review$entry == "second")
  )
  
  summary <- tibble(
    first_entry_rows = nrow(raw),
    second_entry_rows = nrow(qc),
    exact_matching_pairs = exact_pairs,
    discrepancy_pairs = sum(str_starts(report$status, "Value discrepancy")),
    first_entry_unmatched = sum(report$status == "Only in first entry"),
    second_entry_unmatched = sum(report$status == "Only in second entry"),
    first_entry_review = sum(review$entry == "first"),
    second_entry_review = sum(review$entry == "second")
  )
  
  list(report = report, review = review, summary = summary)
}

################################################################################
# step 5: reconcile all three tabs

urch_result <- compare_entries(
  urch_raw_build1, urch_qc_build1,
  keys = urch_keys,
  fields = urch_fields
)

kelp_result <- compare_entries(
  kelp_raw_build1, kelp_qc_build1,
  keys = kelp_keys,
  fields = kelp_fields,
  repeated_records = TRUE
)

frond_result <- compare_entries(
  frond_raw_build1, frond_qc_build1,
  keys = frond_keys,
  fields = frond_fields
)

urch_discrep_values <- urch_result$report
kelp_discrep_values <- kelp_result$report
frond_discrep_values <- frond_result$report

################################################################################
# step 6: combine summary and records requiring manual pairing

reconciliation_summary <- bind_rows(
  swath_urchin_size = urch_result$summary,
  swath_kelp = kelp_result$summary,
  fronds_pull_down_long = frond_result$summary,
  .id = "tab"
)

key_review <- bind_rows(
  swath_urchin_size = urch_result$review,
  swath_kelp = kelp_result$review,
  fronds_pull_down_long = frond_result$review,
  .id = "tab"
)

print(reconciliation_summary, width = Inf)

View(urch_discrep_values)
View(kelp_discrep_values)
View(frond_discrep_values)
View(key_review)

################################################################################
# step 7: create one formatted workbook

output_tables <- list(
  Summary = reconciliation_summary,
  swath_urchin_size = urch_discrep_values,
  swath_kelp = kelp_discrep_values,
  fronds_pull_down_long = frond_discrep_values,
  key_review = key_review
)

wb <- openxlsx::createWorkbook()

header_style <- openxlsx::createStyle(
  fgFill = "#24495E",
  fontColour = "#FFFFFF",
  textDecoration = "bold",
  wrapText = TRUE
)

for (tab in names(output_tables)) {
  
  dat <- output_tables[[tab]]
  
  openxlsx::addWorksheet(wb, tab)
  
  openxlsx::writeData(
    wb, tab, dat,
    headerStyle = header_style,
    withFilter = nrow(dat) > 0
  )
  
  openxlsx::freezePane(wb, tab, firstRow = TRUE)
  
  openxlsx::setColWidths(
    wb, tab,
    cols = seq_len(ncol(dat)),
    widths = 22
  )
  
  wide_columns <- which(names(dat) %in% c("status", "issue"))
  
  if (length(wide_columns) > 0) {
    openxlsx::setColWidths(
      wb, tab,
      cols = wide_columns,
      widths = 45
    )
  }
  
  openxlsx::setRowHeights(wb, tab, rows = 1, heights = 42)
}

################################################################################
# step 8: save and upload to data_reconciliation

output_file <- file.path(
  datdir,
  paste0(
    "productivity_reconciliation_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    ".xlsx"
  )
)

openxlsx::saveWorkbook(wb, output_file, overwrite = FALSE)

uploaded_report <- googledrive::drive_upload(
  media = output_file,
  path = googledrive::as_id(output_folder),
  overwrite = FALSE
)

uploaded_report




