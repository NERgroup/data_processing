#jogsmith@ucsc.edu


###############################################################################
# MPEN occupiable canopy area: union of all 30 m Landsat cells recorded in
# the complete MPEN series, including area == 0 historical-footprint cells.
# Output: one multipart polygon feature in a shapefile (EPSG:3310).


rm(list=ls())

library(sf)
library(dplyr)
library(terra)

# load data and set directories
datin <- "/Volumes/enhydra/data/kelp_remote_sensing/landsat/processed"
input_shp <- file.path(datin, "monterey_peninsula",
                       "landsat_mpen_1984_2026_points_withNAs.shp")
output_shp <- file.path(datin, "monterey_peninsula",
                        "mpen_occupiable_canopy_area.shp")

################################################################################

# These are 30 x 30 m pixel centers, with repeated observations through time.
# In the prior workflow, zero-cover records represented the historical
# kelp footprint. Retain those AND positive-cover records, across all years
# and quarters; exclude NA records because they do not define coverage.
landsat_mpen <- st_read(input_shp, quiet = TRUE)

required <- c("area", "year", "quarter", "latitude")

missing_fields <- setdiff(required, names(landsat_mpen))
if (length(missing_fields)) {
  stop("Missing expected field(s): ", paste(missing_fields, collapse = ", "))
}
if (is.na(st_crs(landsat_mpen))) stop("Input shapefile has no CRS.")

observed <- landsat_mpen %>%
  filter(latitude >= 36.510140, latitude <= 36.670574,
         !is.na(area), area >= 0, !st_is_empty(geometry)) %>%
  st_transform(3310)
if (nrow(observed) == 0L) stop("No zero- or positive-area records in MPEN.")
if (any(st_geometry_type(observed) != "POINT")) {
  stop("Expected pixel-center POINT geometries.")
}

# Remove repeated observations of the same pixel across years and quarters.
cat("Zero-area records:", sum(observed$area == 0), "\n")
cat("Positive-area records:", sum(observed$area > 0), "\n")
xy <- st_coordinates(observed)[, 1:2, drop = FALSE]
xy <- unique(as.data.frame(xy))
names(xy) <- c("x", "y")
cat("Unique cells in occupiable footprint:", nrow(xy), "\n")

# Put projected pixel centers onto a fixed 30 m grid. One occupied cell = 1.
# Using a fixed origin makes the result reproducible across runs.
cell_size <- 30
grid_extent <- terra::ext(
  floor((min(xy$x) - cell_size / 2) / cell_size) * cell_size,
  ceiling((max(xy$x) + cell_size / 2) / cell_size) * cell_size,
  floor((min(xy$y) - cell_size / 2) / cell_size) * cell_size,
  ceiling((max(xy$y) + cell_size / 2) / cell_size) * cell_size
)
grid <- terra::rast(grid_extent, resolution = cell_size, crs = "EPSG:3310")
xy$occupied <- 1L
occupied <- terra::rasterize(
  terra::vect(xy, geom = c("x", "y"), crs = "EPSG:3310"),
  grid, field = "occupied", background = NA
)
names(occupied) <- "occupied"

# Polygonize and dissolve touching cells. Disconnected patches remain parts
# of this one multipart feature, rather than becoming separate records.
poly <- terra::as.polygons(occupied, dissolve = TRUE, na.rm = TRUE)
footprint <- st_as_sf(poly) %>%
  summarise(occupied = 1L, .groups = "drop") %>%
  st_make_valid()

if (nrow(footprint) != 1L) stop("Expected exactly one output feature.")


###############################################################################
#Inspect

# Preview the 30 m cells and the dissolved export boundary
plot(
  occupied,
  col = "#76A9CF",
  legend = FALSE,
  main = "MPEN occupiable canopy area"
)

plot(
  sf::st_geometry(footprint),
  add = TRUE,
  col = NA,
  border = "#163F65",
  lwd = 1
)

###############################################################################
#Export

# delete_dsn removes an older shapefile and its companion files before writing.
st_write(footprint, output_shp, delete_dsn = TRUE, quiet = TRUE)
cat("Wrote:", output_shp, "\n")
cat("Occupiable footprint (km2):",
    as.numeric(st_area(footprint)) / 1e6, "\n")
