# ==============================
# DSS Indicator: Huila Municipal Map
# ==============================
# Source: GADM (Global Administrative Areas)
# Description: Downloads municipality boundaries for the Huila department
#   (Colombia), attaches a mock indicator value per municipality, computes
#   a 5-class YlOrRd colour palette, and exports the result as GeoJSON
#   (for the Leaflet choropleth map) and CSV (tabular download).
#
# The GeoJSON output includes:
#   - NAME_2   : municipality name
#   - mock_value: integer 1-100 (replace with the real indicator once available)
#   - color     : hex colour code from the YlOrRd RColorBrewer palette
# ==============================

library(here)
library(geodata)
library(terra)
library(RColorBrewer)
library(sf)
library(dplyr)
library(readr)
library(glue)

process_huila_map <- function(output_dir = here("outputs")) {
  message("⬇️  Downloading Colombia municipality boundaries from GADM (level 2)...")
  colombia_muni <- geodata::gadm(country = "COL", level = 2, path = tempdir())

  message("📋 Filtering to Huila department...")
  huila_muni <- colombia_muni[colombia_muni$NAME_1 == "Huila", ]

  n_muni <- nrow(huila_muni)
  message(glue("   Found {n_muni} municipalities in Huila"))

  # ── Mock data ───────────────────────────────────────────────────────────────
  # Replace `mock_value` with the real indicator column once the R pipeline
  # produces it (e.g. suicide rate, disease prevalence, …).
  set.seed(123)
  huila_muni$mock_value <- sample(
    1:100,
    size = n_muni,
    replace = TRUE
  )

  # ── Colour palette ───────────────────────────────────────────────────────────
  n_classes <- 5
  breaks <- quantile(
    huila_muni$mock_value,
    probs = seq(0, 1, length.out = n_classes + 1),
    na.rm = TRUE
  )
  palette <- RColorBrewer::brewer.pal(n_classes, "YlOrRd")

  huila_muni$color <- palette[
    cut(
      huila_muni$mock_value,
      breaks = breaks,
      include.lowest = TRUE,
      labels = FALSE
    )
  ]

  # ── Export ───────────────────────────────────────────────────────────────────
  dir.create(file.path(output_dir, "geojson"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "csv"), recursive = TRUE, showWarnings = FALSE)

  geojson_file <- file.path(output_dir, "geojson", "huila_municipalities.geojson")
  csv_file     <- file.path(output_dir, "csv",     "huila_map.csv")

  # Convert terra SpatVector → sf, keep only the columns the frontend needs
  huila_sf <- sf::st_as_sf(huila_muni) |>
    select(NAME_2, mock_value, color)

  sf::st_write(huila_sf, geojson_file, delete_dsn = TRUE)

  # Tabular CSV (no geometry)
  huila_df <- as.data.frame(huila_muni) |>
    select(NAME_2, mock_value, color)
  write_csv(huila_df, csv_file)

  message(glue("✅ Processed {nrow(huila_df)} municipalities"))
  message(glue("💾 GeoJSON: {geojson_file}"))
  message(glue("💾 CSV:     {csv_file}"))

  return(list(
    data         = huila_df,
    output_files = c(geojson_file, csv_file)
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_huila_map()
  cat("✅ Huila map processing completed\n")
}
