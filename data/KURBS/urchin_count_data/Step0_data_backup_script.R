

rm(list=ls())

################################################################################
#Prep workspace and load packages

librarian::shelf("googlesheets4", "writexl", "fs")

#gs4_auth()

server_root <- "/Volumes/enhydra/data"
dir.exists(server_root)

backup_root <- "/Volumes/enhydra/data/KURBS/GDrive_backups/urchin_count_data"
dir_create(backup_root, recurse = TRUE)
dir.exists(backup_root)


################################################################################
#Define sheet to backup and the corresponding tab

sheet_id <- "1IQsGCYl4hXibI9KnnpSjxPLYulQjXGDP_MgT4lIOtsg"
gid <- 1324794989
ss <- as_sheets_id(sheet_id)

#convert gid to tab name
meta <- gs4_get(ss)

tab_title <- meta$sheets$name[ meta$sheets$id == gid ]
tab_title


################################################################################
#Create automatic function and perform backup (.xlsx only)
#NOTE: writes directly into backup_root (no nested folders)

backup_gsheet_gid <- function(sheet_id, gid, backup_root) {
  
  ss <- googlesheets4::as_sheets_id(sheet_id)
  
  #convert gid to tab name
  meta <- googlesheets4::gs4_get(ss)
  
  tab_title <- meta$sheets$name[ meta$sheets$id == gid ]
  if (length(tab_title) != 1) stop("Could not uniquely resolve gid to a single tab title.")
  
  #create backup directory (directly in backup_root)
  backup_dir <- fs::path(backup_root)
  fs::dir_create(backup_dir, recurse = TRUE)
  
  #read sheet
  dat <- googlesheets4::read_sheet(ss, sheet = tab_title)
  
  #timestamp + safe filename
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  safe_tab <- gsub("[^A-Za-z0-9_\\-]+", "_", tab_title)
  base_name <- paste0("backup_", safe_tab, "_", ts)
  
  #write versioned + latest backups
  xlsx_file <- fs::path(backup_dir, paste0(base_name, ".xlsx"))
  writexl::write_xlsx(dat, xlsx_file)
  
  latest_file <- fs::path(backup_dir, paste0("LATEST_", safe_tab, ".xlsx"))
  writexl::write_xlsx(dat, latest_file)
  
  invisible(list(
    tab_title = tab_title,
    backup_dir = backup_dir,
    versioned_file = xlsx_file,
    latest_file = latest_file
  ))
}


################################################################################
#Run backup

backup_gsheet_gid(
  sheet_id = sheet_id,
  gid = gid,
  backup_root = backup_root
)

################################################################################
#Delete server backups older than 90 days

prune_cmd <- paste(
  "find",
  shQuote(backup_root),
  "-type f",
  "-name 'backup_*.xlsx'",
  "-mtime +90",
  "-delete"
)

system(prune_cmd)
