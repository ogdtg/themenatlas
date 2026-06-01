# prepare_data.R
#
# One-time data preparation script. Run this script LOCALLY before deploying
# the app. It transforms raw RDS files into formats optimised for runtime use.
#
# INPUTS  (must exist in data/ relative to the project root):
#   data/gemeindegrenzen.rds   – sf POLYGON, Thurgau Gemeinden (LV95 / EPSG 2056)
#   data/bezirksgrenzen.rds    – sf POLYGON, Thurgau Bezirke
#   data/psg.rds               – sf POLYGON, Primarschulgemeinden
#   data/vsg.rds               – sf POLYGON, Volksschulgemeinden
#   data/ssg.rds               – sf POLYGON, Sekundarschulgemeinden
#
# OUTPUTS (written to data/geojson/ relative to the project root):
#   data/geojson/gemeindegrenzen.json
#   data/geojson/bezirksgrenzen.json
#   data/geojson/psg.json
#   data/geojson/vsg.json
#   data/geojson/ssg.json
#
# These GeoJSON files are registered at runtime with echarts4r so that the
# choropleth map can be rendered without converting sf objects on every request.
#
# HOW TO RUN:
#   Rscript data-raw/prepare_data.R
#   (or source() it from an R session at the project root)

library(sf)

out_dir <- file.path("data", "geojson")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

geo_files <- c(
  gemeindegrenzen = "data/gemeindegrenzen.rds",
  bezirksgrenzen  = "data/bezirksgrenzen.rds",
  psg             = "data/psg.rds",
  vsg             = "data/vsg.rds",
  ssg             = "data/ssg.rds"
)

for (name in names(geo_files)) {
  path <- geo_files[[name]]
  if (!file.exists(path)) {
    message("SKIP (not found): ", path)
    next
  }

  message("Processing: ", name)
  sf_obj <- readRDS(path)

  # Re-project to WGS84 (required by echarts4r / ECharts GeoJSON)
  sf_wgs84 <- sf::st_transform(sf_obj, crs = 4326)

  # Write as GeoJSON to a temp file, then read back as a string
  tmp <- tempfile(fileext = ".geojson")
  sf::st_write(sf_wgs84, tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE)
  geojson_str <- paste(readLines(tmp, warn = FALSE), collapse = "\n")
  unlink(tmp)

  # Save the GeoJSON string as a plain text file
  out_path <- file.path(out_dir, paste0(name, ".json"))
  writeLines(geojson_str, out_path)
  message("  -> written: ", out_path)
}

message("Done. GeoJSON files are in: ", out_dir)
