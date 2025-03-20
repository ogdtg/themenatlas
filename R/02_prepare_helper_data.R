# Load Data

# Data with gemeinde and Bezirk
bezirk_data <- readRDS("data/bezirk_data.rds")

# bezirk_data_mod <- bezirk_data %>%
#   filter(name_gemeinde != "Frauenfeld")
#' Load and Prepare Data for the Shiny App
#'
#' This script loads all necessary datasets for the application, processes geographic
#' and statistical information, and prepares named character vectors for mapping.
#' It also loads color palettes for visualization and retrieves updated data
#' from an external repository.
#'
#' - Loads district-level and municipal-level data.
#' - Processes geographic information for mapping.
#' - Filters data for specific comparisons.
#' - Loads color palettes for map visualization.
#' - Fetches additional data updates from GitHub Actions.
#'
#' @note Some datasets are retrieved externally and require an internet connection.
#' @author [Felix Lorenz]
#' @date [2025-03-17]

# Data containing the bezirk and the Kanton additional to the gemeinde
bezirk_data2 <- bezirk_data %>%
  distinct(bfs_nr_bezirk,name_bezirk) %>%
  mutate(name_bezirk = paste0("Bezirk ",name_bezirk)) %>%
  bind_rows(data.frame(name_bezirk="Kanton Thurgau",bfs_nr_bezirk="20")) %>%
  setNames(c("bfs_nr_gemeinde","name_gemeinde")) %>%
  bind_rows(bezirk_data %>%
              select(bfs_nr_gemeinde,name_gemeinde))


# All data without frauenfeld for the comparision in the reports
bezirk_data_mod <- bezirk_data2 %>%
  filter(name_gemeinde != "Frauenfeld")

# Named character (bfs_nr named with the gemeinde name)
bezirk_data_names2 <- setNames(bezirk_data2$bfs_nr_gemeinde,bezirk_data2$name_gemeinde)
bezirk_data_names <- setNames(bezirk_data$bfs_nr_gemeinde,bezirk_data$name_gemeinde)

# Colour palette for the maps
palette_ds <- readRDS("data/farbpalette_karte.rds")
palette_ds_alternative <- readRDS("data/farbpalette_karte_mod.rds")

# Geo Data for the Map
gemeindegrenzen <- readRDS("data/gemeindegrenzen.rds")
geo_data <- gemeindegrenzen

# or th
# content <- read_html("data/atlas.Rhtml")


# All necessary data that is updated via GitHub Actions in the prepare_indicators repo
nested_list <- readRDS(gzcon(url("https://github.com/ogdtg/prepare_indicators/raw/refs/heads/main/data/nested_list.rds")))
additional_data <- readRDS(gzcon(url("https://github.com/ogdtg/prepare_indicators/raw/refs/heads/main/data/additional_data.rds")))

