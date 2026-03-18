# ==============================
# DSS Indicator: Forest Plot - Suaza
# ==============================
# Cross-indicator analysis: Spearman correlations between each education
# indicator and the suicide mortality gender gap (brecha_absoluta and razon)
# for Suaza municipality.
# ==============================

library(here)
library(dplyr)
library(tidyr)
library(arrow)
library(readr)
library(fs)
library(glue)

source(here("packages/data-r/src/indicators/health/suicide/suicide_huila.R"))
source(here("packages/data-r/src/indicators/health/education/education_suaza.R"))

#' Compute Spearman correlation with 95% CI (Fisher's z-transform on rho) and p-value
#'
#' cor.test(method="spearman") does not return a CI, so the CI is computed via
#' the Fisher z-transform applied to rho (standard approximation for n >= 4).
#'
#' @param x Numeric vector (predictor)
#' @param y Numeric vector (outcome)
#' @return Named list: correlacion, ci_lower, ci_upper, p_value, n
spearman_with_ci <- function(x, y) {
  complete <- complete.cases(x, y)
  x <- x[complete]
  y <- y[complete]
  n <- length(x)

  if (n < 4) {
    return(list(
      correlacion = NA_real_,
      ci_lower    = NA_real_,
      ci_upper    = NA_real_,
      p_value     = NA_real_,
      n           = n
    ))
  }

  test <- cor.test(x, y, method = "spearman", exact = FALSE)
  rho <- as.numeric(test$estimate)
  pval <- as.numeric(test$p.value)

  # Fisher z-transform 95% CI (approximate, valid for moderate n)
  z <- atanh(rho)
  se <- 1 / sqrt(n - 3)
  z_lo <- z - 1.96 * se
  z_hi <- z + 1.96 * se
  back <- function(zv) tanh(zv) # inverse of atanh

  list(
    correlacion = rho,
    ci_lower    = back(z_lo),
    ci_upper    = back(z_hi),
    p_value     = pval,
    n           = n
  )
}

process_forest_plot_suaza <- function(output_dir = here("outputs")) {
  # ── 1. Download / process component indicators ───────────────────────────
  message("⬇️  Fetching suicide mortality data...")
  suicide_result <- process_suicide_huila(output_dir = output_dir)
  suicide_raw <- suicide_result$data

  message("⬇️  Fetching education data...")
  education_result <- process_education_suaza(output_dir = output_dir)
  education_raw <- education_result$data

  # ── 2. Prepare suicide gaps: Suaza only ──────────────────────────────────
  # Compute brecha_absoluta and razon for Suaza from the raw suicide data
  suicidio_suaza_m <- suicide_raw |>
    filter(territorio == "Suaza", sexo == "Masculino") |>
    select(anio, masculino = valor)

  suicidio_suaza_f <- suicide_raw |>
    filter(territorio == "Suaza", sexo == "Femenino") |>
    select(anio, femenino = valor)

  gaps_suaza <- suicidio_suaza_m |>
    inner_join(suicidio_suaza_f, by = "anio") |>
    mutate(
      brecha_absoluta = masculino - femenino,
      razon = if_else(
        femenino > 0 & masculino > 0,
        masculino / femenino,
        NA_real_
      )
    ) |>
    select(anio, brecha_absoluta, razon)

  # ── 3. Prepare education data ─────────────────────────────────────────────
  educacion <- education_raw |>
    rename(
      desercion   = deserci_n,
      aprobacion  = aprobaci_n,
      reprobacion = reprobaci_n
    ) |>
    select(
      anio, cobertura_bruta, cobertura_neta, desercion,
      aprobacion, reprobacion, repitencia
    ) |>
    mutate(anio = as.integer(anio))

  # ── 4. Join on anio ───────────────────────────────────────────────────────
  message("📋 Joining datasets for Suaza...")
  data_joined <- gaps_suaza |>
    mutate(anio = as.integer(anio)) |>
    inner_join(educacion, by = "anio") |>
    arrange(anio)

  message(glue("  Rows in joined dataset: {nrow(data_joined)}"))
  if (nrow(data_joined) > 0) {
    message(glue("  Years: {min(data_joined$anio)} – {max(data_joined$anio)}"))
  }

  # ── 5. Education indicator metadata ──────────────────────────────────────
  edu_indicators <- tibble::tibble(
    indicador = c(
      "cobertura_bruta",
      "cobertura_neta",
      "desercion",
      "aprobacion",
      "reprobacion",
      "repitencia"
    ),
    label = c(
      "Cobertura Bruta",
      "Cobertura Neta",
      "Deserción Escolar",
      "Tasa de Aprobación",
      "Tasa de Reprobación",
      "Tasa de Repitencia"
    )
  )

  # ── 6. Compute correlations for both gap metrics ──────────────────────────
  message("📐 Computing Spearman correlations...")
  metrics <- c("brecha_absoluta", "razon")
  metric_labels <- c(
    brecha_absoluta = "Brecha Absoluta",
    razon           = "Razón Hombre/Mujer"
  )

  results <- purrr::map_dfr(metrics, function(metrica) {
    gap_vec <- data_joined[[metrica]]

    purrr::map_dfr(edu_indicators$indicador, function(ind) {
      edu_vec <- data_joined[[ind]]
      stats <- spearman_with_ci(edu_vec, gap_vec)

      tibble::tibble(
        indicador   = ind,
        metrica     = metrica,
        correlacion = stats$correlacion,
        ci_lower    = stats$ci_lower,
        ci_upper    = stats$ci_upper,
        p_value     = stats$p_value,
        n           = stats$n
      )
    })
  }) |>
    left_join(edu_indicators, by = "indicador") |>
    mutate(metrica_label = metric_labels[metrica]) |>
    select(
      indicador, label, metrica, metrica_label,
      correlacion, ci_lower, ci_upper, p_value, n
    )

  message(glue("  Computed {nrow(results)} correlation rows"))

  # ── 7. Save outputs ───────────────────────────────────────────────────────
  dir_create(file.path(output_dir, "csv"))
  dir_create(file.path(output_dir, "parquet"))

  out_csv <- file.path(output_dir, "csv", "forest_plot_suaza.csv")
  out_parquet <- file.path(output_dir, "parquet", "forest_plot_suaza.parquet")

  write_csv(results, out_csv)
  write_parquet(results, out_parquet)

  message(glue("✅ Saved forest plot data → {out_csv}"))
  message(glue("✅ Saved forest plot data → {out_parquet}"))

  return(list(
    data         = results,
    output_files = c(out_csv, out_parquet)
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_forest_plot_suaza()
  cat("✅ Forest plot (Suaza) processing completed\n")
}
