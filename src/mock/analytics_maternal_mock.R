# ==============================
# Mock Analytics: Maternal Mortality × DSS Indicators
# ==============================
# Reads mock barrio-level outputs for maternal mortality and the five DSS
# indicators, then generates:
#
#   outputs/parquet/mock_forest_plot.parquet
#   outputs/parquet/mock_analytics_maternal.parquet
#   outputs/parquet/mock_scatter_maternal.parquet
#   outputs/geojson/mock_bivariate_{indicator}_{year}.geojson  (per indicator × year)
#   outputs/geojson/mock_maternal_mortality_{year}.geojson     (per year)
#   outputs/geojson/mock_bivariate_dss_{ind_x}_{ind_y}_{year}.geojson (per pair × year)
#
# Run AFTER: SMV_map.R, maternal_mortality_rate.R, and all five
#            *_municipal.R scripts have been executed.
# ==============================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(arrow)
library(sf)
library(RColorBrewer)

output_dir <- here("outputs")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Load barrio-level rows from a parquet, keeping one row per NAME_2/anio.
# Handles three column layouts across different indicators:
#   - grupo_edad present  → keep rows where grupo_edad == "Todas las edades"
#   - etnia present       → keep rows where etnia == "Total"
#   - neither present     → keep all barrio-level rows
load_barrio <- function(filename, col_name) {
  df <- read_parquet(file.path(output_dir, "parquet", filename))

  if ("grupo_edad" %in% names(df)) {
    df <- dplyr::filter(df, grupo_edad == "Todas las edades")
  } else if ("etnia" %in% names(df)) {
    df <- dplyr::filter(df, etnia == "Total")
  } else if ("sexo" %in% names(df)) {
    df <- dplyr::filter(df, sexo == "Total")
  }

  df |>
    dplyr::filter(!NAME_2 %in% c("San Martín del Valle")) |>
    dplyr::group_by(NAME_2, anio) |>
    dplyr::summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(valor = ifelse(is.nan(valor), NA_real_, valor)) |>
    dplyr::rename(!!col_name := valor)
}

# Spearman rho with approximate 95 % CI via Fisher z-transformation.
spearman_ci <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x  <- x[ok]; y <- y[ok]
  n  <- length(x)
  if (n < 4) {
    return(list(rho = NA_real_, ci_lower = NA_real_,
                ci_upper = NA_real_, p_value = NA_real_, n = n))
  }
  test  <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho   <- as.numeric(test$estimate)
  p_val <- test$p.value

  # Fisher z CI (avoids |rho| == 1 edge-case)
  rho_clamp <- max(-0.9999, min(0.9999, rho))
  z   <- atanh(rho_clamp)
  se  <- 1 / sqrt(n - 3)
  ci_l <- tanh(z - 1.96 * se)
  ci_u <- tanh(z + 1.96 * se)

  list(rho = rho, ci_lower = ci_l, ci_upper = ci_u, p_value = p_val, n = n)
}

# Classify a numeric vector into tercile classes 0/1/2 (returns NA for missing).
classify_tercile <- function(x) {
  breaks <- stats::quantile(x, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE)
  breaks <- unique(breaks)
  if (length(breaks) < 4) return(rep(NA_integer_, length(x)))
  cls <- as.integer(cut(x, breaks = breaks, include.lowest = TRUE,
                        labels = FALSE))
  cls - 1L
}

# ── 1. Load maternal mortality barrio-level data ──────────────────────────────

mm_raw <- read_parquet(
  file.path(output_dir, "parquet", "maternal_mortality_rate.parquet")
)

mm_barrio <- mm_raw |>
  dplyr::filter(
    etnia == "Total",
    !NAME_2 %in% c("San Martín del Valle")
  ) |>
  dplyr::select(NAME_2, anio, zona, valor_mm = valor)

# ── 2. Load each DSS indicator's barrio-level data ────────────────────────────

traslado_df    <- load_barrio("journey_time.parquet",       "traslado")
empleo_df      <- load_barrio("informal_employment.parquet","empleo_informal")
sobrecarga_df  <- load_barrio("care_overload_municipal.parquet",      "sobrecarga")
cobertura_df   <- load_barrio("program_cover.parquet",                "cobertura_programa")
transporte_df  <- load_barrio("transport_frequency_municipal.parquet","transporte")

# ── 3. Join all indicators ────────────────────────────────────────────────────

all_data <- mm_barrio |>
  dplyr::left_join(traslado_df,   by = c("NAME_2", "anio")) |>
  dplyr::left_join(empleo_df,     by = c("NAME_2", "anio")) |>
  dplyr::left_join(sobrecarga_df, by = c("NAME_2", "anio")) |>
  dplyr::left_join(cobertura_df,  by = c("NAME_2", "anio")) |>
  dplyr::left_join(transporte_df, by = c("NAME_2", "anio"))

# ── 4. Forest plot: Spearman correlations (all available years) ───────────────
#
# Generates one row per (anio, indicador) where sufficient non-NA data exists.
# Years or indicators with fewer than 4 complete pairs are silently skipped.

available_years <- sort(unique(all_data$anio))
last_year       <- max(available_years)

indicator_meta <- list(
  traslado           = "Tiempo de traslado (>1h al CS)",
  empleo_informal    = "Empleo informal",
  sobrecarga         = "Sobrecarga de cuidados",
  cobertura_programa = "Cobertura programa social",
  transporte         = "Transporte subsidiado"
)

forest_rows_list <- lapply(available_years, function(yr) {
  yr_data <- dplyr::filter(all_data, anio == yr, !is.na(valor_mm))
  rows <- lapply(names(indicator_meta), function(ind_key) {
    vals <- dplyr::pull(yr_data, ind_key)
    mm   <- yr_data$valor_mm
    res  <- spearman_ci(vals, mm)
    if (is.na(res$rho)) return(NULL)
    tibble::tibble(
      anio        = yr,
      indicador   = ind_key,
      label       = indicator_meta[[ind_key]],
      correlacion = res$rho,
      ci_lower    = res$ci_lower,
      ci_upper    = res$ci_upper,
      p_value     = res$p_value,
      n           = as.integer(res$n)
    )
  })
  dplyr::bind_rows(rows)
})

mock_forest_plot <- dplyr::bind_rows(forest_rows_list)

# ── 5. Analytics maternal: annual weighted means ──────────────────────────────

mock_analytics_maternal <- all_data |>
  dplyr::group_by(anio) |>
  dplyr::summarise(
    valor              = mean(valor_mm,          na.rm = TRUE),
    traslado           = mean(traslado,           na.rm = TRUE),
    empleo_informal    = mean(empleo_informal,    na.rm = TRUE),
    sobrecarga         = mean(sobrecarga,         na.rm = TRUE),
    cobertura_programa = mean(cobertura_programa, na.rm = TRUE),
    transporte         = mean(transporte,         na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::arrange(anio)

# ── 6. Scatter maternal: per barrio, all years ────────────────────────────────

# Simulate nacimientos (live births) from barrio zone – fixed per barrio
set.seed(42)
barrio_nacimientos <- mm_barrio |>
  dplyr::distinct(NAME_2, zona) |>
  dplyr::mutate(
    nacimientos = dplyr::case_when(
      zona == "urbano"     ~ as.integer(round(stats::runif(dplyr::n(), 80, 130))),
      zona == "periurbano" ~ as.integer(round(stats::runif(dplyr::n(), 50, 80))),
      TRUE                 ~ as.integer(round(stats::runif(dplyr::n(), 25, 50)))
    )
  ) |>
  dplyr::select(NAME_2, nacimientos)

mock_scatter_maternal <- all_data |>
  dplyr::left_join(barrio_nacimientos, by = "NAME_2") |>
  dplyr::rename(territorio = NAME_2, valor = valor_mm) |>
  dplyr::select(
    anio, territorio, valor,
    traslado, empleo_informal, sobrecarga, cobertura_programa, transporte,
    nacimientos
  ) |>
  dplyr::arrange(anio, territorio)

# ── 7. Save parquet + CSV outputs ─────────────────────────────────────────────

parquet_dir <- file.path(output_dir, "parquet")
csv_dir     <- file.path(output_dir, "csv")
dir.create(parquet_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(csv_dir,     showWarnings = FALSE, recursive = TRUE)

write_parquet(mock_forest_plot,        file.path(parquet_dir, "mock_forest_plot.parquet"))
write_parquet(mock_analytics_maternal, file.path(parquet_dir, "mock_analytics_maternal.parquet"))
write_parquet(mock_scatter_maternal,   file.path(parquet_dir, "mock_scatter_maternal.parquet"))

write_csv(mock_forest_plot,        file.path(csv_dir, "mock_forest_plot.csv"))
write_csv(mock_analytics_maternal, file.path(csv_dir, "mock_analytics_maternal.csv"))
write_csv(mock_scatter_maternal,   file.path(csv_dir, "mock_scatter_maternal.csv"))

message("✅ mock_forest_plot, mock_analytics_maternal, mock_scatter_maternal saved")

# ── 8. Per-year bivariate and maternal GeoJSONs ───────────────────────────────
#
# Generates one GeoJSON per (indicator, year) and one per year for maternal-only.
# Years or indicators with no data produce a GeoJSON with all NA values (grey).

# Bivariate colour palette — rows = MM tercile (0=low,1=med,2=high),
#                             cols = indicator tercile (0=low,1=med,2=high)
bivariate_colors <- matrix(c(
  "#e8e8e8", "#ace4e4", "#5ac8c8",  # mm low
  "#dfb0d6", "#a5b8c5", "#5a9ab5",  # mm med
  "#be64ac", "#8c62aa", "#3b4994"   # mm high
), nrow = 3, byrow = TRUE)

# Load base geometry
geojson_path <- file.path(output_dir, "geojson", "SMV_municipalities.geojson")
smv_sf <- sf::st_read(geojson_path, quiet = TRUE)

ylord <- RColorBrewer::brewer.pal(5, "YlOrRd")

make_bivariate_geojson <- function(ind_col, out_name, map_data_yr, mm_last_map_yr) {
  ind_df <- map_data_yr |>
    dplyr::select(territorio, value = !!rlang::sym(ind_col)) |>
    dplyr::left_join(
      mm_last_map_yr |> dplyr::select(territorio, maternal_value = valor_mm,
                                      maternal_class = mm_class),
      by = "territorio"
    ) |>
    dplyr::mutate(
      ind_class = classify_tercile(value),
      color = mapply(
        function(mc, ic) {
          if (is.na(mc) || is.na(ic)) "#CCCCCC"
          else bivariate_colors[mc + 1L, ic + 1L]
        },
        maternal_class, ind_class,
        USE.NAMES = FALSE
      )
    )

  out_sf <- smv_sf |>
    dplyr::select(NAME_2, geometry) |>
    dplyr::left_join(ind_df, by = c("NAME_2" = "territorio")) |>
    dplyr::select(NAME_2, value, maternal_value, maternal_class, ind_class,
                  color, geometry)

  out_path <- file.path(output_dir, "geojson", out_name)
  sf::st_write(out_sf, out_path, delete_dsn = TRUE, quiet = TRUE)
  message(paste0("✅ ", out_name))
}

make_maternal_only_geojson <- function(out_name, mm_last_map_yr) {
  mm_values <- mm_last_map_yr$valor_mm
  mm_breaks <- stats::quantile(mm_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
  mm_breaks <- unique(mm_breaks)

  maternal_only_sf <- smv_sf |>
    dplyr::select(NAME_2, geometry) |>
    dplyr::left_join(
      mm_last_map_yr |> dplyr::select(territorio, value = valor_mm),
      by = c("NAME_2" = "territorio")
    ) |>
    dplyr::mutate(
      color = {
        if (length(mm_breaks) < 2) {
          dplyr::if_else(is.na(value), "#CCCCCC", ylord[3L])
        } else {
          cls <- as.integer(cut(value, breaks = mm_breaks,
                                include.lowest = TRUE, labels = FALSE))
          dplyr::if_else(is.na(cls), "#CCCCCC", ylord[cls])
        }
      }
    ) |>
    dplyr::select(NAME_2, value, color, geometry)

  out_path <- file.path(output_dir, "geojson", out_name)
  sf::st_write(maternal_only_sf, out_path, delete_dsn = TRUE, quiet = TRUE)
  message(paste0("✅ ", out_name))
}

for (yr in available_years) {
  map_data_yr <- dplyr::filter(all_data, anio == yr) |>
    dplyr::rename(territorio = NAME_2)

  mm_last_map_yr <- map_data_yr |>
    dplyr::select(territorio, valor_mm) |>
    dplyr::mutate(mm_class = classify_tercile(valor_mm))

  make_bivariate_geojson("traslado",           paste0("mock_bivariate_traslado_",           yr, ".geojson"), map_data_yr, mm_last_map_yr)
  make_bivariate_geojson("empleo_informal",    paste0("mock_bivariate_empleo_informal_",    yr, ".geojson"), map_data_yr, mm_last_map_yr)
  make_bivariate_geojson("sobrecarga",         paste0("mock_bivariate_sobrecarga_",         yr, ".geojson"), map_data_yr, mm_last_map_yr)
  make_bivariate_geojson("cobertura_programa", paste0("mock_bivariate_cobertura_programa_", yr, ".geojson"), map_data_yr, mm_last_map_yr)
  make_bivariate_geojson("transporte",         paste0("mock_bivariate_transporte_",         yr, ".geojson"), map_data_yr, mm_last_map_yr)

  make_maternal_only_geojson(paste0("mock_maternal_mortality_", yr, ".geojson"), mm_last_map_yr)
}

# ── 8.5. Per-year single-indicator GeoJSONs (one per DSS indicator) ──────────
#
# Generates one GeoJSON per (indicator, year) showing only that indicator's
# values with a sequential YlOrRd colour scale.
# File: mock_{indicator}_{year}.geojson

make_indicator_only_geojson <- function(ind_col, out_name, map_data_yr) {
  ind_df <- map_data_yr |>
    dplyr::select(territorio, value = !!rlang::sym(ind_col))

  ind_values <- ind_df$value
  ind_breaks <- stats::quantile(ind_values, probs = seq(0, 1, length.out = 6),
                                na.rm = TRUE)
  ind_breaks <- unique(ind_breaks)

  out_sf <- smv_sf |>
    dplyr::select(NAME_2, geometry) |>
    dplyr::left_join(ind_df, by = c("NAME_2" = "territorio")) |>
    dplyr::mutate(
      color = {
        if (length(ind_breaks) < 2) {
          dplyr::if_else(is.na(value), "#CCCCCC", ylord[3L])
        } else {
          cls <- as.integer(cut(value, breaks = ind_breaks,
                                include.lowest = TRUE, labels = FALSE))
          dplyr::if_else(is.na(cls), "#CCCCCC", ylord[cls])
        }
      }
    ) |>
    dplyr::select(NAME_2, value, color, geometry)

  out_path <- file.path(output_dir, "geojson", out_name)
  sf::st_write(out_sf, out_path, delete_dsn = TRUE, quiet = TRUE)
  message(paste0("✅ ", out_name))
}

for (yr in available_years) {
  map_data_yr <- dplyr::filter(all_data, anio == yr) |>
    dplyr::rename(territorio = NAME_2)

  make_indicator_only_geojson("traslado",           paste0("mock_traslado_",           yr, ".geojson"), map_data_yr)
  make_indicator_only_geojson("empleo_informal",    paste0("mock_empleo_informal_",    yr, ".geojson"), map_data_yr)
  make_indicator_only_geojson("sobrecarga",         paste0("mock_sobrecarga_",         yr, ".geojson"), map_data_yr)
  make_indicator_only_geojson("cobertura_programa", paste0("mock_cobertura_programa_", yr, ".geojson"), map_data_yr)
  make_indicator_only_geojson("transporte",         paste0("mock_transporte_",         yr, ".geojson"), map_data_yr)
}

message("✅ Single-indicator GeoJSONs saved")

# ── 9. Per-year DSS Bivariate GeoJSONs (all ordered indicator pairs) ──────────
#
# For each ordered pair (ind_x, ind_y) where ind_x ≠ ind_y, and each year:
#   File: mock_bivariate_dss_{ind_x}_{ind_y}_{year}.geojson

make_dss_bivariate_geojson <- function(ind_x_col, ind_y_col, out_name, map_data_yr) {
  ind_df <- map_data_yr |>
    dplyr::select(
      territorio,
      value          = !!rlang::sym(ind_x_col),
      maternal_value = !!rlang::sym(ind_y_col)
    ) |>
    dplyr::mutate(
      ind_class      = classify_tercile(value),
      maternal_class = classify_tercile(maternal_value),
      color = mapply(
        function(mc, ic) {
          if (is.na(mc) || is.na(ic)) "#CCCCCC"
          else bivariate_colors[mc + 1L, ic + 1L]
        },
        maternal_class, ind_class,
        USE.NAMES = FALSE
      )
    )

  out_sf <- smv_sf |>
    dplyr::select(NAME_2, geometry) |>
    dplyr::left_join(ind_df, by = c("NAME_2" = "territorio")) |>
    dplyr::select(NAME_2, value, maternal_value, maternal_class, ind_class,
                  color, geometry)

  out_path <- file.path(output_dir, "geojson", out_name)
  sf::st_write(out_sf, out_path, delete_dsn = TRUE, quiet = TRUE)
  message(paste0("✅ ", out_name))
}

dss_indicators <- c("traslado", "empleo_informal", "sobrecarga",
                    "cobertura_programa", "transporte")

for (yr in available_years) {
  map_data_yr <- dplyr::filter(all_data, anio == yr) |>
    dplyr::rename(territorio = NAME_2)

  for (ind_x in dss_indicators) {
    for (ind_y in dss_indicators) {
      if (ind_x != ind_y) {
        out_name <- paste0("mock_bivariate_dss_", ind_x, "_", ind_y, "_", yr, ".geojson")
        make_dss_bivariate_geojson(ind_x, ind_y, out_name, map_data_yr)
      }
    }
  }
}

message("\n✅ analytics_maternal_mock.R complete")
