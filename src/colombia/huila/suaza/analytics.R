# ==============================
# Analytics: Suicide Mortality × DSS Indicators (Suaza)
# ==============================
# Reads real Suaza-level outputs for suicide mortality (main focus indicator),
# the six education indicators, and health insurance coverage, then generates:
#
#   outputs/parquet/suaza_forest_plot.parquet
#   outputs/parquet/suaza_analytics.parquet
#   outputs/parquet/suaza_scatter.parquet
#
# Run AFTER: suicide_mortality.R, education.R, and health_insurance.R have
#            been executed (this script reads their parquet outputs).
#
# No spatial/map outputs: unlike the mock (which correlates across barrios
# within the San Martín del Valle catchment), Suaza has a single municipal
# unit, so correlation here is computed across years (temporal) instead of
# across sub-units within a year.
# ==============================

library(here)
library(dplyr)
library(tidyr)
library(readr)
library(arrow)

output_dir <- here("outputs")

# ── Helpers ───────────────────────────────────────────────────────────────────

# Spearman rho with approximate 95 % CI via Fisher z-transformation.
spearman_ci <- function(x, y) {
  ok <- !is.na(x) & !is.na(y)
  x <- x[ok]
  y <- y[ok]
  n <- length(x)
  if (n < 4) {
    return(list(
      rho = NA_real_, ci_lower = NA_real_,
      ci_upper = NA_real_, p_value = NA_real_, n = n
    ))
  }
  test <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho <- as.numeric(test$estimate)
  p_val <- test$p.value

  # Fisher z CI (avoids |rho| == 1 edge-case)
  rho_clamp <- max(-0.9999, min(0.9999, rho))
  z <- atanh(rho_clamp)
  se <- 1 / sqrt(n - 3)
  ci_l <- tanh(z - 1.96 * se)
  ci_u <- tanh(z + 1.96 * se)

  list(rho = rho, ci_lower = ci_l, ci_upper = ci_u, p_value = p_val, n = n)
}

# Load a single-indicator parquet (anio, territorio, valor) and rename valor.
load_indicator <- function(filename, col_name) {
  read_parquet(file.path(output_dir, "parquet", filename)) |>
    dplyr::filter(territorio == "Suaza") |>
    dplyr::select(anio, valor) |>
    dplyr::rename(!!col_name := valor)
}

# ── 1. Load suicide mortality data (main focus indicator) ─────────────────────

suicidio_raw <- read_parquet(file.path(output_dir, "parquet", "mortalidad-suicidio.parquet"))

suicidio <- suicidio_raw |>
  dplyr::filter(territorio == "Suaza", sexo == "Total") |>
  dplyr::select(anio, valor_suicidio = valor)

# ── 2. Load education indicators ───────────────────────────────────────────────

cobertura_bruta_df <- load_indicator("cobertura-bruta.parquet", "cobertura_bruta")
cobertura_neta_df <- load_indicator("cobertura-neta.parquet", "cobertura_neta")
desercion_df <- load_indicator("desercion.parquet", "desercion")
aprobacion_df <- load_indicator("aprobacion.parquet", "aprobacion")
reprobacion_df <- load_indicator("reprobacion.parquet", "reprobacion")
repitencia_df <- load_indicator("repitencia.parquet", "repitencia")
formalidad_df <- load_indicator("formalidad.parquet", "formalidad")

# ── 3. Load health insurance coverage (regimen == "Total") ────────────────────

aseguramiento_df <- read_parquet(file.path(output_dir, "parquet", "aseguramiento.parquet")) |>
  dplyr::filter(regimen == "Total") |>
  dplyr::select(anio, aseguramiento = valor)

# ── 4. Join all indicators on anio ─────────────────────────────────────────────

all_data <- suicidio |>
  dplyr::left_join(cobertura_bruta_df, by = "anio") |>
  dplyr::left_join(cobertura_neta_df, by = "anio") |>
  dplyr::left_join(desercion_df, by = "anio") |>
  dplyr::left_join(aprobacion_df, by = "anio") |>
  dplyr::left_join(reprobacion_df, by = "anio") |>
  dplyr::left_join(repitencia_df, by = "anio") |>
  dplyr::left_join(aseguramiento_df, by = "anio") |>
  dplyr::left_join(formalidad_df, by = "anio") |>
  dplyr::arrange(anio)

indicator_meta <- list(
  cobertura_bruta = "Cobertura bruta educativa",
  cobertura_neta = "Cobertura neta educativa",
  desercion = "Deserción escolar",
  aprobacion = "Aprobación escolar",
  reprobacion = "Reprobación escolar",
  repitencia = "Repitencia escolar",
  aseguramiento = "Cobertura de aseguramiento en salud",
  formalidad = "Cobertura de formalidad"
)

# ── 5. Forest plot: rolling-window Spearman correlations, one row per (anio, indicador) ─
#
# Suaza is a single territorial unit, so a within-year cross-sectional
# correlation is not possible. Instead, for each year Y the indicator series is
# correlated against the suicide series over a trailing window of
# `window_years` calendar years (Y - window_years + 1 … Y). A (year, indicator)
# pair with fewer than 4 complete observations in its window is silently
# skipped (spearman_ci returns NA below n = 4).

window_years <- 5L

available_years <- sort(unique(all_data$anio))

forest_rows <- lapply(available_years, function(yr) {
  win <- dplyr::filter(all_data, anio > yr - window_years, anio <= yr)
  rows <- lapply(names(indicator_meta), function(ind_key) {
    res <- spearman_ci(dplyr::pull(win, ind_key), win$valor_suicidio)
    if (is.na(res$rho)) {
      return(NULL)
    }
    tibble::tibble(
      anio        = as.integer(yr),
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

suaza_forest_plot <- dplyr::bind_rows(forest_rows)

# ── 6. Analytics: wide table, one row per year ─────────────────────────────────

suaza_analytics <- all_data |>
  dplyr::rename(valor = valor_suicidio) |>
  dplyr::select(
    anio, valor,
    cobertura_bruta, cobertura_neta, desercion, aprobacion, reprobacion,
    repitencia, aseguramiento, formalidad
  )

# ── 7. Save parquet + CSV outputs ──────────────────────────────────────────────

parquet_dir <- file.path(output_dir, "parquet")
csv_dir <- file.path(output_dir, "csv")
dir.create(parquet_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(csv_dir, showWarnings = FALSE, recursive = TRUE)

write_parquet(suaza_forest_plot, file.path(parquet_dir, "forest-plot.parquet"))
write_parquet(suaza_analytics, file.path(parquet_dir, "analytics.parquet"))

write_csv(suaza_forest_plot, file.path(csv_dir, "forest-plot.csv"))
write_csv(suaza_analytics, file.path(csv_dir, "analytics.csv"))

message("✅ suaza_forest_plot, suaza_analytics saved")
