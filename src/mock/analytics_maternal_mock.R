# ==============================
# Mock Analytics: Maternal Mortality × DSS Indicators
# ==============================
# Reads mock barrio-level outputs for maternal mortality and the five DSS
# indicators, then generates:
#
#   outputs/parquet/mock_forest_plot.parquet
#   outputs/parquet/mock_analytics_maternal.parquet
#   outputs/parquet/mock_scatter_maternal.parquet
#   outputs/geojson/mock_bivariate_traslado.geojson
#   outputs/geojson/mock_bivariate_empleo_informal.geojson
#   outputs/geojson/mock_bivariate_sobrecarga.geojson
#   outputs/geojson/mock_bivariate_cobertura_programa.geojson
#   outputs/geojson/mock_bivariate_transporte.geojson
#   outputs/geojson/mock_maternal_mortality.geojson
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
# "barrio_total" rows are identified by grupo_edad == "Todas las edades" and
# NAME_2 not being the municipality aggregate ("San Martín del Valle").
load_barrio <- function(filename, col_name) {
  df <- read_parquet(file.path(output_dir, "parquet", filename))
  df |>
    dplyr::filter(
      grupo_edad == "Todas las edades",
      !NAME_2 %in% c("San Martín del Valle")
    ) |>
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
    grupo_edad == "Todas las edades",
    !NAME_2 %in% c("San Martín del Valle")
  ) |>
  dplyr::select(NAME_2, anio, zona, valor_mm = valor)

# ── 2. Load each DSS indicator's barrio-level data ────────────────────────────

traslado_df    <- load_barrio("journey_time_municipal.parquet",       "traslado")
empleo_df      <- load_barrio("informal_employment_municipal.parquet","empleo_informal")
sobrecarga_df  <- load_barrio("care_overload_municipal.parquet",      "sobrecarga")
cobertura_df   <- load_barrio("program_cover_municipal.parquet",      "cobertura_programa")
transporte_df  <- load_barrio("transport_frequency_municipal.parquet","transporte")

# ── 3. Join all indicators ────────────────────────────────────────────────────

all_data <- mm_barrio |>
  dplyr::left_join(traslado_df,   by = c("NAME_2", "anio")) |>
  dplyr::left_join(empleo_df,     by = c("NAME_2", "anio")) |>
  dplyr::left_join(sobrecarga_df, by = c("NAME_2", "anio")) |>
  dplyr::left_join(cobertura_df,  by = c("NAME_2", "anio")) |>
  dplyr::left_join(transporte_df, by = c("NAME_2", "anio"))

# ── 4. Forest plot: Spearman correlations (last available year) ───────────────

last_year       <- max(all_data$anio)
last_year_data  <- dplyr::filter(all_data, anio == last_year, !is.na(valor_mm))

indicator_meta <- list(
  traslado           = "Tiempo de traslado (>1h al CS)",
  empleo_informal    = "Empleo informal",
  sobrecarga         = "Sobrecarga de cuidados",
  cobertura_programa = "Cobertura programa social",
  transporte         = "Transporte subsidiado"
)

forest_rows <- lapply(names(indicator_meta), function(ind_key) {
  vals <- dplyr::pull(last_year_data, ind_key)
  mm   <- last_year_data$valor_mm
  res  <- spearman_ci(vals, mm)
  tibble::tibble(
    indicador   = ind_key,
    label       = indicator_meta[[ind_key]],
    correlacion = res$rho,
    ci_lower    = res$ci_lower,
    ci_upper    = res$ci_upper,
    p_value     = res$p_value,
    n           = as.integer(res$n)
  )
})

mock_forest_plot <- dplyr::bind_rows(forest_rows)

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

# ── 8. Bivariate GeoJSONs ─────────────────────────────────────────────────────

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

# Last-year barrio values for map generation
map_data <- dplyr::filter(all_data, anio == last_year) |>
  dplyr::rename(territorio = NAME_2)

mm_last_map <- map_data |>
  dplyr::select(territorio, valor_mm) |>
  dplyr::mutate(mm_class = classify_tercile(valor_mm))

make_bivariate_geojson <- function(ind_col, out_name) {
  ind_df <- map_data |>
    dplyr::select(territorio, value = !!rlang::sym(ind_col)) |>
    dplyr::left_join(
      mm_last_map |> dplyr::select(territorio, maternal_value = valor_mm,
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

make_bivariate_geojson("traslado",           "mock_bivariate_traslado.geojson")
make_bivariate_geojson("empleo_informal",    "mock_bivariate_empleo_informal.geojson")
make_bivariate_geojson("sobrecarga",         "mock_bivariate_sobrecarga.geojson")
make_bivariate_geojson("cobertura_programa", "mock_bivariate_cobertura_programa.geojson")
make_bivariate_geojson("transporte",         "mock_bivariate_transporte.geojson")

# Maternal-only GeoJSON (YlOrRd single-variable palette)
ylord <- RColorBrewer::brewer.pal(5, "YlOrRd")

mm_values  <- mm_last_map$valor_mm
mm_breaks  <- stats::quantile(mm_values, probs = seq(0, 1, length.out = 6), na.rm = TRUE)
mm_breaks  <- unique(mm_breaks)

maternal_only_sf <- smv_sf |>
  dplyr::select(NAME_2, geometry) |>
  dplyr::left_join(
    mm_last_map |> dplyr::select(territorio, value = valor_mm),
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

sf::st_write(
  maternal_only_sf,
  file.path(output_dir, "geojson", "mock_maternal_mortality.geojson"),
  delete_dsn = TRUE, quiet = TRUE
)
message("✅ mock_maternal_mortality.geojson")

# ── 9. DSS Bivariate GeoJSONs (all ordered indicator pairs) ──────────────────
#
# For each ordered pair (ind_x, ind_y) where ind_x ≠ ind_y:
#   ind_x  = X-axis  (the forest-plot–selected indicator)
#   ind_y  = Y-axis  (the second DSS indicator, replacing maternal mortality)
#
# File: mock_bivariate_dss_{ind_x}_{ind_y}.geojson
# Properties: NAME_2, value (ind_x), maternal_value (ind_y),
#             ind_class, maternal_class, color

make_dss_bivariate_geojson <- function(ind_x_col, ind_y_col, out_name) {
  ind_df <- map_data |>
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

for (ind_x in dss_indicators) {
  for (ind_y in dss_indicators) {
    if (ind_x != ind_y) {
      out_name <- paste0("mock_bivariate_dss_", ind_x, "_", ind_y, ".geojson")
      make_dss_bivariate_geojson(ind_x, ind_y, out_name)
    }
  }
}

message("\n✅ analytics_maternal_mock.R complete")
