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
years_faint    <- c(2020, 2022, 2024)   # drop 2023 (and 2022)
year_highlight <- 2014


# --- Build series -------------------------------------------------------------

anom_2025 <- daily_anom %>%
  filter(year == 2025, doy <= doy_max) %>%
  transmute(doy, anom_mean, line = "2025")

anom_2014 <- daily_anom %>%
  filter(year == year_highlight, doy <= doy_max) %>%
  transmute(doy, anom_mean, line = as.character(year_highlight))

anom_faint <- daily_anom %>%
  filter(year %in% years_faint, doy <= doy_max) %>%
  transmute(doy, anom_mean, line = as.character(year))

anom_mean <- daily_anom %>%
  filter(year != 2025, doy <= doy_max) %>%
  group_by(doy) %>%
  summarise(anom_mean = mean(anom_mean, na.rm = TRUE), .groups = "drop") %>%
  transmute(doy, anom_mean, line = "Mean (2010–2024)")

plot_dat <- bind_rows(anom_faint, anom_2014, anom_mean, anom_2025) %>%
  filter(doy > 150)

# Legend order (THIS drives everything)
legend_order <- c(
  "Mean (2010–2024)",
  as.character(year_highlight),
  as.character(years_faint),
  "2025"
)

# Force factor levels so ggplot can't "helpfully" reorder/drop them
plot_dat <- plot_dat %>%
  mutate(line = factor(line, levels = legend_order))

# Colors (must match legend_order labels exactly)
line_cols <- c(
  "Mean (2010–2024)" = "black",
  "2014"             = "navy",
  "2020"             = "grey50",
  "2022"             = "grey50",
  "2024"             = "grey50",
  "2025"             = "indianred"
)

# --- Plot --------------------------------------------------------------------

p_sub <- ggplot(plot_dat, aes(x = doy, y = anom_mean)) +
  
  # Faint context years
  geom_line(
    data = plot_dat %>% filter(line %in% as.character(years_faint)),
    aes(color = line),
    linewidth = 0.45,
    alpha = 0.5,
    show.legend = TRUE
  ) +
  
  # Highlight year (2014)
  geom_line(
    data = plot_dat %>% filter(line == as.character(year_highlight)),
    aes(color = line),
    linewidth = 0.55,
    alpha = 0.85,
    show.legend = TRUE
  ) +
  
  # Mean baseline
  geom_line(
    data = plot_dat %>% filter(line == "Mean (2010–2024)"),
    aes(color = line),
    linewidth = 0.75,
    show.legend = TRUE
  ) +
  
  # 2025 (bold)
  geom_line(
    data = plot_dat %>% filter(line == "2025"),
    aes(color = line),
    linewidth = 1.1,
    show.legend = TRUE
  ) +
  
  scale_color_manual(
    values = line_cols,
    limits = legend_order,  # force legend entries & order
    drop   = FALSE          # don't drop levels even if sparse
  ) +
  
  guides(
    color = guide_legend(override.aes = list(alpha = 1))  # make legend lines fully visible
  ) +
  
  labs(
    x = "Day of year",
    y = "SST anomaly (°C)",
    color = "",
    title = "Daily SST anomalies, CA coastal"
  ) +
  
  theme_bw(base_size = 7) +
  theme(
    plot.title = element_text(size = 8, face = "bold", hjust = 0),
    axis.title = element_text(size = 7),
    axis.text  = element_text(size = 6),
    
    legend.position  = "bottom",
    legend.direction = "horizontal",
    legend.title     = element_blank(),
    legend.text      = element_text(size = 6),
    legend.key.width  = unit(0.8, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(t = 0, b = 0),
    
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")
  )

p_sub






p_small <- p_sub +
  theme_bw(base_size = 7) +   # <-- globally shrink text
  theme(
    plot.title = element_text(
      size = 8,
      face = "bold",
      hjust = 0
    ),
    axis.title = element_text(size = 7),
    axis.text  = element_text(size = 6),
    
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.title = element_blank(),
    legend.text  = element_text(size = 6),
    legend.key.width = unit(0.8, "lines"),
    legend.key.height = unit(0.6, "lines"),
    legend.margin = margin(t = 0, b = 0),
    
    panel.grid.minor = element_blank(),
    
    plot.margin = margin(
      t = 3, r = 3, b = 3, l = 3, unit = "pt"
    )
  )


ggsave(
  filename = "~/Downloads/OISST_daily_anom_smallv4.png",
  plot     = p_small,
  width    = 2.5,       # inches
  height   = 2,
  dpi      = 600,
  units    = "in"
)



