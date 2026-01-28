################################################################################
# Download and tidy daily MUR SST anomalies for CA coastal / EEZ region
# Dataset: jplMURSST41anom1day (MUR SST anomaly, 0.01°, daily, 2002-present)
# Output: tidy RDS of daily anomalies saved on enhydra server
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rerddap)
library(lubridate)
library(tidyverse)

################################################################################
# Directories
################################################################################

# Adjust this if your mount point is different on your system/RStudio Server
basedir   <- "/Volumes/enhydra/data/environmental_data/MURSST"

inputdir  <- file.path(basedir, "raw")        # smb://samba.pbsci.ucsc.edu/enhydra/data/environmental_data/MURSST/raw
outputdir <- file.path(basedir, "processed")
plotdir   <- file.path(basedir, "figures")

dir.create(inputdir,  showWarnings = FALSE, recursive = TRUE)
dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)
dir.create(plotdir,   showWarnings = FALSE, recursive = TRUE)

################################################################################
# ERDDAP dataset info
################################################################################

mur_url    <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "jplMURSST41anom1day"   # daily anomaly product

################################################################################
# Define spatial and temporal domain
################################################################################

# Approximate CA coastal / EEZ bounding box
lat_range <- c(32, 42.1)    # degrees_north
lon_range <- c(-130, -114)  # degrees_east

# Time range (dataset starts 2002-06-01, not 2000)
time_start <- "2002-06-01"
time_end   <- format(Sys.Date() - 5, "%Y-%m-%d")   # avoid last few revisable days

# Stride: [time, latitude, longitude]
# daily in time, every 10th cell in space (~0.1°)
stride_vec <- c(1, 10, 10)

################################################################################
# Download data via griddap
################################################################################

mur_anom <- griddap(
  datasetx  = dataset_id,           # <-- THIS is the key change
  url       = mur_url,
  time      = c(time_start, time_end),
  longitude = lon_range,
  latitude  = lat_range,
  fields    = c("sstAnom", "mask"),
  stride    = stride_vec,
  fmt       = "csv",
  store     = disk(path = inputdir)   # writes a CSV into `raw`
)

################################################################################
# Tidy data
################################################################################

anom_df <- mur_anom$data %>%
  rename(
    lat_dd     = latitude,
    long_dd    = longitude,
    sst_anom_c = sstAnom
  ) %>%
  mutate(
    date = as.Date(time)
  ) %>%
  select(date, lat_dd, long_dd, sst_anom_c, mask)

# Quick checks
glimpse(anom_df)
print(range(anom_df$date))

################################################################################
# Save processed data
################################################################################

outfile <- file.path(outputdir, "2002_present_mursst_daily_anom_CAEEZ_stride10.Rds")
saveRDS(anom_df, file = outfile)

message("Saved processed daily MUR SST anomaly data to: ", outfile)
