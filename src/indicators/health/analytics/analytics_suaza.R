# ==============================
# DSS Indicator: Analytics - Suaza (Deserción Escolar × Mortalidad por Suicidio)
# ==============================
# Cross-indicator analysis joining suicide mortality with education indicators
# for Suaza municipality.  Produces a joined dataset and lag regression summaries.
# ==============================

library(here)
library(readxl)
library(jsonlite)
library(dplyr)
library(tidyr)
library(arrow)
library(readr)
library(fs)
library(glue)

source(here("packages/data-r/src/indicators/health/suicide/suicide_huila.R"))
source(here("packages/data-r/src/indicators/health/education/education_suaza.R"))

#' Build cross-indicator dataset for Suaza and run lag regression models
#'
#' @param output_dir Directory for outputs
#' @return List with data, lag_models_summary, and output_files
process_analytics_suaza <- function(output_dir = here("outputs")) {

  # ── 1. Download / process component indicators ───────────────────────────
  message("⬇️  Fetching suicide mortality data...")
  suicide_result    <- process_suicide_huila(output_dir = output_dir)
  suicide_raw       <- suicide_result$data

  message("⬇️  Fetching education data...")
  education_result  <- process_education_suaza(output_dir = output_dir)
  education_raw     <- education_result$data

  # ── 2. Prepare suicide data: Suaza, sexo == "Total" ───────────────────────
  suicidio <- suicide_raw |>
    filter(territorio == "Suaza", sexo == "Total") |>
    select(anio, valor) |>
    mutate(anio = as.integer(anio))

  # ── 3. Prepare education data ─────────────────────────────────────────────
  educacion <- education_raw |>
    select(
      anio,
      cobertura_bruta,
      cobertura_neta,
      desercion    = deserci_n,
      aprobacion   = aprobaci_n,
      reprobacion  = reprobaci_n,
      repitencia
    ) |>
    filter(!is.na(repitencia)) |>
    mutate(anio = as.integer(anio))

  # ── 4. Inner join on anio ─────────────────────────────────────────────────
  message("📋 Joining datasets for Suaza...")
  data_cruce <- suicidio |>
    inner_join(educacion, by = "anio") |>
    arrange(anio)

  message(glue("  Rows in joined dataset: {nrow(data_cruce)}"))
  if (nrow(data_cruce) > 0) {
    message(glue("  Years: {min(data_cruce$anio)} – {max(data_cruce$anio)}"))
  }

  # ── 5. Lag regression models ──────────────────────────────────────────────
  message("📐 Fitting lag regression models...")
  data_lag <- data_cruce |>
    mutate(
      desercion_lag1 = lag(desercion, 1),
      desercion_lag2 = lag(desercion, 2)
    )

  modelo_lag1   <- lm(valor ~ desercion_lag1,         data = data_lag)
  modelo_lag1_t <- lm(valor ~ desercion_lag1 + anio,  data = data_lag)
  modelo_lag2   <- lm(valor ~ desercion_lag2 + anio,  data = data_lag)

  # Compact summary helper
  tidy_model <- function(model, label) {
    s   <- summary(model)
    cf  <- as.data.frame(coef(s))
    cf$term   <- rownames(cf)
    cf$model  <- label
    cf$r2     <- s$r.squared
    cf$r2_adj <- s$adj.r.squared
    f_val     <- s$fstatistic
    cf$f_stat <- if (!is.null(f_val)) f_val[1] else NA_real_
    cf$f_df1  <- if (!is.null(f_val)) f_val[2] else NA_real_
    cf$f_df2  <- if (!is.null(f_val)) f_val[3] else NA_real_
    cf$f_pval <- if (!is.null(f_val)) pf(f_val[1], f_val[2], f_val[3], lower.tail = FALSE) else NA_real_
    cf$n      <- nrow(model$model)
    names(cf)[1:4] <- c("estimate", "std_error", "t_value", "p_value")
    cf[, c("model", "term", "estimate", "std_error", "t_value", "p_value",
           "r2", "r2_adj", "f_stat", "f_df1", "f_df2", "f_pval", "n")]
  }

  lag_summaries <- bind_rows(
    tidy_model(modelo_lag1,   "lag1"),
    tidy_model(modelo_lag1_t, "lag1_trend"),
    tidy_model(modelo_lag2,   "lag2_trend")
  )

  # ── 6. Save outputs ───────────────────────────────────────────────────────
  dir_create(file.path(output_dir, "csv"))
  dir_create(file.path(output_dir, "parquet"))

  cruce_csv     <- file.path(output_dir, "csv",     "analytics_suaza.csv")
  cruce_parquet <- file.path(output_dir, "parquet", "analytics_suaza.parquet")
  lags_csv      <- file.path(output_dir, "csv",     "analytics_suaza_lags.csv")
  lags_parquet  <- file.path(output_dir, "parquet", "analytics_suaza_lags.parquet")

  write_csv(data_cruce,    cruce_csv)
  write_parquet(data_cruce, cruce_parquet)
  write_csv(lag_summaries, lags_csv)
  write_parquet(lag_summaries, lags_parquet)

  message(glue("✅ Saved joined data  → {cruce_csv}"))
  message(glue("✅ Saved lag models   → {lags_csv}"))

  return(list(
    data              = data_cruce,
    lag_models        = list(lag1 = modelo_lag1, lag1_t = modelo_lag1_t, lag2 = modelo_lag2),
    lag_summaries     = lag_summaries,
    output_files      = c(cruce_csv, cruce_parquet, lags_csv, lags_parquet)
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_analytics_suaza()
  cat("✅ Analytics (Suaza) processing completed\n")
}
