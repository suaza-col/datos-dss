# ==============================
# DSS Indicator: Huila Municipal Map - Bivariate Choropleth
# ==============================
# Source: GADM (boundaries) + datos.gov.co (education data)
#         + outputs/geojson/huila_maternal_mortality.geojson (maternal mortality)
#
# Description: Downloads municipality boundaries for the Huila department
#   (Colombia), joins education indicator data for the most recent year,
#   and computes a bivariate choropleth colour using a 3×3 palette that
#   combines maternal-mortality class (1–3) with education-indicator class (1–3).
#
#   One bivariate GeoJSON is exported per education indicator.
#   A combined CSV (tabular download) is also written.
#
# NOTE: This script reads huila_maternal_mortality.geojson from the outputs
#   directory. Run maternal_mortality_rate.R first to ensure that file exists.
#
# GeoJSON properties per feature:
#   - NAME_2          : municipality name (from GADM)
#   - value           : education indicator value for the most recent year
#                       (NA / null if no education data — kept for table view)
#   - maternal_value  : maternal mortality rate per 100k live births
#                       (NA / null if no maternal data)
#   - maternal_class  : tertile class for maternal mortality (1 low – 3 high,
#                       NA if no data)
#   - edu_class       : tertile class for the education indicator (1 low – 3
#                       high, NA if no data)
#   - color           : bivariate hex colour (#CCCCCC if either value is NA)
# ==============================

library(here)
library(geodata)
library(terra)
library(sf)
library(dplyr)
library(readr)
library(glue)
library(jsonlite)
library(stringi)

# ── Bivariate colour palette (Joshua Stevens scheme) ────────────────────────
# Rows = maternal-mortality class (1 = low, 3 = high)
# Cols = education-indicator class (1 = low, 3 = high)
# Access via BIVARIATE_PALETTE[mm_class, edu_class]
BIVARIATE_PALETTE <- matrix(
  c(
    "#e8e8e8", "#ace4e4", "#5ac8c8",  # row 1: low MM
    "#dfb0d6", "#a5b8c5", "#5a9ab5",  # row 2: med MM
    "#be64ac", "#8c62aa", "#3b4994"   # row 3: high MM
  ),
  nrow = 3, ncol = 3, byrow = TRUE
)

# ── Helper functions ─────────────────────────────────────────────────────────

#' Download full Socrata dataset with pagination
descargar_socrata_completa <- function(base_url, batch_size = 50000, extra_query = NULL) {
  offset <- 0
  lista_batches <- list()
  i <- 1

  repeat {
    url <- paste0(
      base_url,
      "?$limit=", batch_size,
      "&$offset=", offset,
      if (!is.null(extra_query)) paste0("&", extra_query) else ""
    )
    message(glue("  Descargando desde offset = {offset}"))
    batch <- tryCatch(
      fromJSON(url, flatten = TRUE),
      error = function(e) stop(
        glue("Error al descargar datos desde '{url}': {conditionMessage(e)}"),
        call. = FALSE
      )
    )
    if (nrow(batch) == 0) break
    lista_batches[[i]] <- batch
    if (nrow(batch) < batch_size) break
    offset <- offset + batch_size
    i <- i + 1
  }

  bind_rows(lista_batches)
}

#' Normalise a character vector for fuzzy municipality name matching.
#' Converts to lowercase, strips diacritics, and trims whitespace.
normalize_name <- function(x) {
  x |>
    tolower() |>
    stri_trans_general("Latin-ASCII") |>
    trimws()
}

#' Assign a 3-class tertile to a numeric vector using rank-based assignment.
#' Returns integer 1 (low), 2 (medium), or 3 (high), with NA for missing values.
#' Uses dplyr::ntile() which is robust to duplicate quantile breaks and tied
#' values — situations where quantile-based cut() with fixed labels would error.
compute_class3 <- function(values) {
  as.integer(dplyr::ntile(values, 3L))
}

#' Compute bivariate colour from mm_class and edu_class (each 1–3 or NA).
#' Returns "#CCCCCC" (grey) when either class is NA.
compute_bivariate_color <- function(mm_class, edu_class) {
  mapply(function(mm, edu) {
    if (is.na(mm) || is.na(edu)) "#CCCCCC" else BIVARIATE_PALETTE[mm, edu]
  }, mm_class, edu_class, USE.NAMES = FALSE)
}

# ── Main processing function ─────────────────────────────────────────────────

process_huila_map <- function(output_dir = here("outputs")) {

  # ── Municipality boundaries ──────────────────────────────────────────────
  message("⬇️  Downloading Colombia municipality boundaries from GADM (level 2)...")
  colombia_muni <- geodata::gadm(country = "COL", level = 2, path = tempdir())

  message("📋 Filtering to Huila department...")
  huila_muni <- colombia_muni[colombia_muni$NAME_1 == "Huila", ]
  n_muni     <- nrow(huila_muni)
  message(glue("   Found {n_muni} municipalities in Huila"))

  # ── Maternal mortality data ──────────────────────────────────────────────
  mm_geojson_path <- file.path(output_dir, "geojson", "map_maternal_mortality.geojson")

  mm_lookup <- setNames(
    rep(NA_real_, n_muni),
    normalize_name(as.data.frame(huila_muni)$NAME_2)
  )

  if (file.exists(mm_geojson_path)) {
    message(glue("📂 Loading maternal mortality data from {mm_geojson_path}"))
    mm_sf    <- sf::st_read(mm_geojson_path, quiet = TRUE)
    mm_df    <- as.data.frame(mm_sf) |>
      mutate(nombre_norm = normalize_name(NAME_2))

    for (i in seq_len(nrow(mm_df))) {
      nm  <- mm_df$nombre_norm[i]
      val <- mm_df$value[i]
      if (!is.null(val) && !is.na(val)) mm_lookup[nm] <- as.numeric(val)
    }
    n_mm <- sum(!is.na(mm_lookup))
    message(glue("   Loaded maternal mortality for {n_mm}/{n_muni} municipalities"))
  } else {
    warning(glue(
      "Maternal mortality GeoJSON not found at '{mm_geojson_path}'. ",
      "Run maternal_mortality_rate.R first. Bivariate colours will be grey."
    ))
  }

  # ── Education data for all Huila municipalities ──────────────────────────
  message("⬇️  Downloading education data for Huila from datos.gov.co...")
  base_url <- "https://www.datos.gov.co/resource/nudc-7mev.json"

  select_fields <- paste(
    c(
      "a_o", "municipio", "departamento",
      "cobertura_bruta", "cobertura_neta",
      "deserci_n", "aprobaci_n", "reprobaci_n", "repitencia"
    ),
    collapse = ","
  )
  where_clause <- URLencode("departamento = 'Huila'", reserved = TRUE)
  extra_query  <- glue("$select={select_fields}&$where={where_clause}")

  edu_raw <- descargar_socrata_completa(base_url, extra_query = extra_query)

  message("📋 Processing education data...")
  edu <- edu_raw |>
    mutate(
      anio            = as.integer(a_o),
      cobertura_bruta = as.numeric(cobertura_bruta),
      cobertura_neta  = as.numeric(cobertura_neta),
      deserci_n       = as.numeric(deserci_n),
      aprobaci_n      = as.numeric(aprobaci_n),
      reprobaci_n     = as.numeric(reprobaci_n),
      repitencia      = as.numeric(repitencia)
    ) |>
    filter(!is.na(anio))

  if (nrow(edu) == 0) {
    stop(
      "No education records were returned for Huila after processing. ",
      "This may indicate an API issue, a schema change, or a mismatch in the query filter."
    )
  }

  # Use the most recent year with available data
  latest_year  <- max(edu$anio, na.rm = TRUE)
  message(glue("   Using year: {latest_year}"))
  edu_latest <- edu |>
    filter(anio == latest_year) |>
    mutate(nombre_norm = normalize_name(municipio))

  # ── Join boundaries ↔ education ──────────────────────────────────────────
  huila_base_df <- as.data.frame(huila_muni) |>
    mutate(nombre_norm = normalize_name(NAME_2))

  joined <- huila_base_df |>
    left_join(
      edu_latest |>
        select(
          nombre_norm, municipio,
          cobertura_bruta, cobertura_neta,
          deserci_n, aprobaci_n, reprobaci_n, repitencia
        ),
      by = "nombre_norm"
    )

  # Attach maternal mortality values
  joined <- joined |>
    mutate(maternal_value = mm_lookup[nombre_norm])

  n_matched <- sum(!is.na(joined$cobertura_bruta))
  message(glue("   Matched {n_matched}/{n_muni} municipalities with education data"))

  # Compute 3-class tertile for maternal mortality (shared across all indicators)
  mm_class <- compute_class3(joined$maternal_value)

  # ── Export ───────────────────────────────────────────────────────────────
  dir.create(file.path(output_dir, "geojson"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "csv"),     recursive = TRUE, showWarnings = FALSE)

  huila_sf_base <- sf::st_as_sf(huila_muni)

  # One bivariate GeoJSON per education indicator
  indicator_map <- list(
    cobertura_bruta = "map_cobertura_bruta",
    cobertura_neta  = "map_cobertura_neta",
    deserci_n       = "map_desercion",
    aprobaci_n      = "map_aprobacion",
    reprobaci_n     = "map_reprobacion",
    repitencia      = "map_repitencia"
  )

  output_files <- character(0)

  for (ind_col in names(indicator_map)) {
    file_stem  <- indicator_map[[ind_col]]
    edu_values <- joined[[ind_col]]
    edu_class  <- compute_class3(edu_values)
    biv_colors <- compute_bivariate_color(mm_class, edu_class)

    huila_sf_ind <- huila_sf_base |>
      mutate(
        value          = edu_values,
        maternal_value = joined$maternal_value,
        maternal_class = mm_class,
        edu_class      = edu_class,
        color          = biv_colors
      ) |>
      select(NAME_2, value, maternal_value, maternal_class, edu_class, color)

    geojson_file <- file.path(output_dir, "geojson", paste0(file_stem, ".geojson"))
    sf::st_write(huila_sf_ind, geojson_file, delete_dsn = TRUE)
    output_files <- c(output_files, geojson_file)
    message(glue("💾 Bivariate GeoJSON ({ind_col}): {geojson_file}"))
  }

  # Combined CSV with all indicators
  csv_df <- joined |>
    select(
      NAME_2, municipio,
      cobertura_bruta, cobertura_neta,
      deserci_n, aprobaci_n, reprobaci_n, repitencia
    )
  csv_file <- file.path(output_dir, "csv", "map.csv")
  write_csv(csv_df, csv_file)
  output_files <- c(output_files, csv_file)
  message(glue("💾 CSV: {csv_file}"))

  message(glue(
    "✅ Processed {n_muni} municipalities, matched {n_matched} with education data (year {latest_year})"
  ))

  return(list(
    data         = csv_df,
    output_files = output_files
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_huila_map()
  cat("✅ Huila map processing completed\n")
}
