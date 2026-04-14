# ==============================
# DSS Mock: SMV Municipal Map
# ==============================
# Generates a choropleth-ready GeoJSON and CSV for the San Martín del Valle
# (SMV) mock territory, using Huila GADM boundaries with fake barrio names
# and a synthetic indicator value.
#
# Outputs:
#   outputs/geojson/SMV_municipalities.geojson
#   outputs/csv/SMV_map.csv
# ==============================

library(here)
library(geodata)
library(terra)
library(RColorBrewer)
library(sf)
library(dplyr)
library(readr)
library(glue)

process_SMV_map <- function(output_dir = here("outputs")) {
  message("⬇️  Downloading Colombia municipality boundaries from GADM (level 2)...")
  colombia_muni <- geodata::gadm(country = "COL", level = 2, path = tempdir())

  message("📋 Filtering to Huila department...")
  SMV_muni <- colombia_muni[colombia_muni$NAME_1 == "Huila", ]

  # Remove municipalities that are not part of the SMV mock territory
  SMV_muni <- SMV_muni[!SMV_muni$NAME_2 %in% c("Colombia", "Baraya", "San Agustín", "Acevedo", "Villavieja"), ]

  n_muni <- nrow(SMV_muni)
  message(glue("   Found {n_muni} municipalities in SMV"))

  # ── Zone classification based on distance to central municipalities ──────────

  centrales <- c("Tarqui", "Altamira", "Agrado", "Pital")

  SMV_sf_tmp <- sf::st_as_sf(SMV_muni)
  centroides <- sf::st_centroid(SMV_sf_tmp)

  centros_ref <- centroides[centroides$NAME_2 %in% centrales, ]

  dist_mat <- sf::st_distance(centroides, centros_ref)
  dist_min <- apply(dist_mat, 1, min)

  SMV_muni$dist_centro <- as.numeric(dist_min)

  umbral_periurbano <- quantile(SMV_muni$dist_centro, 0.50, na.rm = TRUE)

  SMV_muni$tipo_zona <- dplyr::case_when(
    SMV_muni$NAME_2 %in% centrales ~ "Urbano central",
    SMV_muni$dist_centro <= umbral_periurbano ~ "Periurbano",
    TRUE ~ "Rural"
  )

  # ── Barrio name assignment ───────────────────────────────────────────────────

  barrios <- c(
    "Santa Ana", "San Joaquín", "San Rafael", "La Alameda",
    "Villa Esperanza", "Plaza del Valle", "Los Arrayanes", "Nueva Esperanza",
    "Las Palmas", "Santa Lucía", "San Isidro", "La Primavera",
    "Los Pinos", "Vista Hermosa", "La Rivera", "Los Álamos", "El Mirador",
    "Villa Norte", "Las Brisas", "El Porvenir", "El Carmen", "Santa Rosa",
    "Villa Sur", "El Edén", "La Esperanza", "Nueva Vida",
    "Colinas del Valle", "Bosques del Valle", "Altos del Sol", "La Cañada"
  )

  barrios_peri_extra <- c("La Arboleda", "Santa Elena")

  if ((length(barrios) + length(barrios_peri_extra)) != nrow(SMV_muni)) {
    stop("El número total de nombres de barrios no coincide con el número de unidades territoriales.")
  }

  set.seed(123)

  SMV_muni$barrio <- NA_character_

  idx_peri <- which(SMV_muni$tipo_zona == "Periurbano")

  if (length(idx_peri) < length(barrios_peri_extra)) {
    stop("No hay suficientes unidades periurbanas para asignar los barrios extra.")
  }

  # Reserve the first two periurbano units for the extra names
  idx_reservados <- idx_peri[1:2]
  idx_restantes  <- setdiff(seq_len(nrow(SMV_muni)), idx_reservados)

  SMV_muni$barrio[idx_restantes] <- sample(barrios, size = length(idx_restantes), replace = FALSE)
  SMV_muni$barrio[idx_reservados] <- barrios_peri_extra

  # ── Synthetic indicator value ────────────────────────────────────────────────

  set.seed(123)
  SMV_muni$mock_value <- sample(1:100, size = n_muni, replace = TRUE)

  # ── Colour palette (YlOrRd, 5 classes) ──────────────────────────────────────

  n_classes <- 5
  breaks <- quantile(
    SMV_muni$mock_value,
    probs = seq(0, 1, length.out = n_classes + 1),
    na.rm = TRUE
  )
  breaks <- unique(breaks)
  n_effective_classes <- length(breaks) - 1
  palette <- RColorBrewer::brewer.pal(max(3, n_effective_classes), "YlOrRd")[1:n_effective_classes]

  SMV_muni$class_id <- cut(
    SMV_muni$mock_value,
    breaks = breaks,
    include.lowest = TRUE,
    labels = FALSE
  )
  SMV_muni$color <- palette[SMV_muni$class_id]

  # ── Export ───────────────────────────────────────────────────────────────────

  dir.create(file.path(output_dir, "geojson"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "csv"),     recursive = TRUE, showWarnings = FALSE)

  geojson_file <- file.path(output_dir, "geojson", "SMV_municipalities.geojson")
  csv_file     <- file.path(output_dir, "csv",     "SMV_map.csv")

  # Convert to sf, keep only the columns the frontend needs
  SMV_sf <- sf::st_as_sf(SMV_muni)
  SMV_sf <- SMV_sf[, c("barrio", "tipo_zona", "mock_value", "color", "geometry")]
  names(SMV_sf)[names(SMV_sf) == "barrio"] <- "NAME_2"

  sf::st_write(SMV_sf, geojson_file, delete_dsn = TRUE)

  # Tabular CSV (no geometry)
  SMV_df <- as.data.frame(SMV_muni) |>
    dplyr::select(barrio, tipo_zona, mock_value, color)
  names(SMV_df)[names(SMV_df) == "barrio"] <- "NAME_2"

  write_csv(SMV_df, csv_file)

  message(glue("✅ Processed {nrow(SMV_df)} municipalities"))
  message(glue("💾 GeoJSON: {geojson_file}"))
  message(glue("💾 CSV:     {csv_file}"))

  return(list(
    data         = SMV_df,
    sf           = SMV_sf,
    output_files = c(geojson_file, csv_file)
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_SMV_map()
  cat("✅ SMV map processing completed\n")
}
