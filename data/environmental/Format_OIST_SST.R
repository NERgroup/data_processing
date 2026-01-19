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

years_to_get <- c(2010, 2012, 2014, 2016, 2018, 2020, 2021, 2023, 2025)

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


# 2014 line
anom_2014 <- daily_anom %>%
  filter(year == 2014, doy <= 360)   # also truncate to 280 for consistency

# 2025 line, truncated at DOY 280
#anom_2025 <- daily_anom %>%
#  filter(year == 2025, doy <= 280)

# 2025 line, truncated at DOY 280
anom_2025 <- daily_anom %>%
  filter(year == 2025, doy <= 360)

# Mean across *all* years for each DOY, up to 280
anom_mean <- daily_anom %>%
  filter(doy <= 360) %>%
  group_by(doy) %>%
  summarise(
    anom_mean = mean(anom_mean, na.rm = TRUE),
    .groups   = "drop"
  )

# Combine: keep only doy + anom_mean + line label (no `year` column)
plot_dat <- bind_rows(
  anom_2014 %>% transmute(doy, anom_mean, line = "2014"),
  anom_2025 %>% transmute(doy, anom_mean, line = "2025"),
  anom_mean %>% transmute(doy, anom_mean, line = "Mean (all years)")
)

plot_dat$line <- factor(plot_dat$line,
                        levels = c("2014", "2025", "Mean (all years)"))

# Plot
p_sub <- ggplot() +
  
  # 2014 and Mean lines (normal thickness)
  geom_line(
    data = plot_dat %>% filter(line != "2025"),
    aes(x = doy, y = anom_mean, color = line, linetype = line),
    linewidth = 0.5
  ) +
  
  # 2025 line (twice as thick)
  geom_line(
    data = plot_dat %>% filter(line == "2025"),
    aes(x = doy, y = anom_mean, color = line, linetype = line),
    linewidth = 0.7
  ) +
  
  scale_color_manual(values = c(
    "2014"             = "grey50",
    "2025"             = "indianred",
    "Mean (all years)" = "black"
  )) +
  scale_linetype_manual(values = c(
    "2014"             = "solid",
    "2025"             = "solid",
    "Mean (all years)" = "solid"
  )) +
  labs(
    x = "Day of year",
    y = "Regional mean SST anomaly (°C)",
    color    = "",
    linetype = "",
    title    = "Daily regional mean SST anomalies (OISST v2.1)\nCA coastal / EEZ region"
  ) +
  theme_bw()

p_sub


ggsave(
  filename = "~/Downloads/OISST_daily_anom_small.png",
  plot     = p_sub,
  width    = 3,        # inches
  height   = 2.2,      # inches
  dpi      = 600,      # very crisp for tiny figures
  units    = "in"
)


p_small <- p_sub +
  theme(
    text = element_text(size = 6),       # global text size
    axis.title = element_text(size = 6),
    axis.text  = element_text(size = 5),
    legend.title = element_text(size = 6),
    legend.text  = element_text(size = 5),
    plot.title   = element_text(size = 7, lineheight = 0.9),
    plot.subtitle= element_text(size = 6)
  )


ggsave(
  filename = "~/Downloads/OISST_daily_anom_small.png",
  plot     = p_small,
  width    = 3,       # inches
  height   = 1.5,
  dpi      = 600,
  units    = "in"
)



