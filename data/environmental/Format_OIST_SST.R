################################################################################
# Daily regional-mean SST anomalies (OISST) for CA coastal / EEZ region
# Years: 2014, 2017, 2020, 2023, 2025
#
# Dataset: ncdcOisst21Agg_LonPM180 (NOAA OISST v2.1 daily, 0.25°, global)
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rerddap)
library(tidyverse)
library(lubridate)

################################################################################
# Directories
################################################################################

basedir   <- "/Volumes/enhydra/data/environmental_data/MURSST"  # adjust if needed
outputdir <- file.path(basedir, "processed")
plotdir   <- file.path(basedir, "figures")

dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)
dir.create(plotdir,   showWarnings = FALSE, recursive = TRUE)

################################################################################
# ERDDAP settings
################################################################################

oisst_url  <- "https://coastwatch.pfeg.noaa.gov/erddap/"
dataset_id <- "ncdcOisst21Agg_LonPM180"  # OISST v2.1, lon -180..180

# CA coastal / EEZ-ish box
lat_range <- c(32, 42.1)
lon_range <- c(-130, -114)

#Monterey Area
#lat_range <- c(36.0, 37.2)
#lon_range <- c(-123.3, -121.6)

#Single Monterey grid point
#lat_range <- c(36.606670, 36.606670)
#lon_range <- c(-121.882710, -121.882710)



################################################################################
# Helper: download ONE YEAR of OISST as daily regional means (chunked by month)
################################################################################

download_oisst_year_mean_chunked <- function(year,
                                             lat_range,
                                             lon_range,
                                             url,
                                             dataset_id) {
  
  # Start / end for this year
  t_start <- as.Date(paste0(year, "-01-01"))
  if (year == lubridate::year(Sys.Date())) {
    # Dataset currently ends at 2025-11-25 (per ERDDAP error message)
    t_final <- as.Date("2025-12-31")
  } else {
    t_final <- as.Date(paste0(year, "-12-31"))
  }
  
  message("Downloading OISST ", year, ": ", t_start, " to ", t_final)
  
  # Monthly boundaries
  month_starts <- seq(t_start, t_final, by = "1 month")
  month_ends   <- c(month_starts[-1] - 1, t_final)
  month_ends   <- pmin(month_ends, t_final)
  
  out_list <- vector("list", length(month_starts))
  
  for (i in seq_along(month_starts)) {
    t1 <- month_starts[i]
    t2 <- month_ends[i]
    
    message("  ", year, " chunk ", i, "/", length(month_starts),
            ": ", t1, " to ", t2)
    
    oisst_raw <- griddap(
      datasetx  = dataset_id,
      url       = url,
      time      = c(as.character(t1), as.character(t2)),
      zlev      = c(0, 0),
      latitude  = lat_range,
      longitude = lon_range,
      fields    = "anom"
    )
    
    # Skip empty chunks (in case server hiccups)
    if (is.null(oisst_raw$data) || nrow(oisst_raw$data) == 0) next
    
    out_list[[i]] <- oisst_raw$data %>%
      mutate(
        date = as.Date(time)  # "YYYY-MM-DDT12:00:00Z" -> Date
      ) %>%
      group_by(date) %>%
      summarise(
        year      = year(first(date)),
        doy       = yday(first(date)),
        anom_mean = mean(anom, na.rm = TRUE),
        .groups   = "drop"
      )
  }
  
  year_df <- bind_rows(out_list)
  
  if (nrow(year_df) == 0) {
    stop("No OISST data returned for year ", year,
         " after monthly chunking. Check server or parameters.")
  }
  
  return(year_df)
}

################################################################################
# Years to download
################################################################################

#years_to_get <- c(2014, 2017, 2020, 2023, 2025)

years_to_get <- c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 
                  2020, 2021, 2022, 2023, 2024, 2025)

################################################################################
# Download all years and combine
################################################################################

daily_list <- lapply(
  years_to_get,
  \(yr) download_oisst_year_mean_chunked(
    year       = yr,
    lat_range  = lat_range,
    lon_range  = lon_range,
    url        = oisst_url,
    dataset_id = dataset_id
  )
)

daily_anom <- bind_rows(daily_list)

# Quick sanity checks
glimpse(daily_anom)
table(daily_anom$year)
range(daily_anom$date)

################################################################################
# Save daily regional mean anomalies
################################################################################

outfile <- file.path(
  outputdir,
  "OISST_daily_regionalMean_CAEEZ_2014_2017_2020_2023_2025.Rds"
)
saveRDS(daily_anom, file = outfile)
message("Saved daily regional mean anomalies to: ", outfile)

################################################################################
# Plot: DOY on x, anomaly on y, separate line for each year
################################################################################


# Choose your truncation window for the figure
doy_max <- 360

# Years
years_faint    <- c(2024)   # drop 2023 (and 2022)
year_highlight <- 2014

# 2025 (bold)
anom_2025 <- daily_anom %>%
  filter(year == 2025, doy <= doy_max) %>%
  transmute(doy, anom_mean, line = "2025")

# 2015 (highlight comparison)
anom_2015 <- daily_anom %>%
  filter(year == year_highlight, doy <= doy_max) %>%
  transmute(doy, anom_mean, line = as.character(year_highlight))

# 2024 (faint context line)
anom_faint <- daily_anom %>%
  filter(year %in% years_faint, doy <= doy_max) %>%
  mutate(line = as.character(year)) %>%
  transmute(doy, anom_mean, line)

# Mean across all pre-2025 years (baseline)
anom_mean <- daily_anom %>%
  filter(year != 2025, doy <= doy_max) %>%
  group_by(doy) %>%
  summarise(anom_mean = mean(anom_mean, na.rm = TRUE), .groups = "drop") %>%
  transmute(doy, anom_mean, line = "Mean (2010–2024)")

# Combine
plot_dat <- bind_rows(anom_faint, anom_2015, anom_mean, anom_2025)

# Legend order
plot_dat$line <- factor(
  plot_dat$line,
  levels = c("2024", as.character(year_highlight), "Mean (2010–2024)", "2025")
)

# Plot
p_sub <- ggplot() +
  
  # Faint context (2024)
  geom_line(
    data = plot_dat %>% filter(line %in% as.character(years_faint)),
    aes(x = doy, y = anom_mean, color = line),
    linewidth = 0.45,
    alpha = 0.8
  ) +
  
  # 2015 highlight
  geom_line(
    data = plot_dat %>% filter(line == as.character(year_highlight)),
    aes(x = doy, y = anom_mean, color = line),
    linewidth = 0.5,
    alpha = 0.8
  ) +
  
  # Mean baseline (solid black) — mapped so it appears in legend
  geom_line(
    data = plot_dat %>% filter(line == "Mean (2010–2024)"),
    aes(x = doy, y = anom_mean, color = line),
    linewidth = 0.7
  ) +
  
  # 2025 (boldest) — mapped so it appears in legend
  geom_line(
    data = plot_dat %>% filter(line == "2025"),
    aes(x = doy, y = anom_mean, color = line),
    linewidth = 1.1
  ) +
  
  # Now include ONLY the lines you actually want in the legend
  scale_color_manual(values = c(
    "2024"             = "grey40",
    "2014"             = "navyblue",
    "Mean (2010–2024)" = "black",
    "2025"             = "indianred"
  )) +
  
  labs(
    x = "Day of year",
    y = "Regional mean SST anomaly (°C)",
    color    = "",
    title    = "Daily regional mean SST anomalies (OISST v2.1), CA coastal",
    #subtitle = paste0("2025 (bold) vs. 2015 (last major heatwave), 2024, and baseline mean; through DOY ", doy_max)
  ) +
  theme_bw()

p_sub



ggsave(
  filename = "~/Downloads/OISST_daily_anom_small.png",
  plot     = p_small,
  width    = 3,       # inches
  height   = 1.5,
  dpi      = 600,
  units    = "in"
)



