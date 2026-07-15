# ==============================
# DSS Indicator: Health Insurance
# ==============================
# Source: Observatorio de Salud de Suaza
# Indicator: Aseguramiento
# Stratifier: regimen
# ==============================

library(here)
library(readxl)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)

process_health_insurance <- function(output_dir = here("outputs")) {
  url <- "https://www.huila.gov.co/observatoriosalud/loader.php?lServicio=Tools2&lTipo=descargas&lFuncion=descargar&idFile=84085"

  temp_file <- tempfile()

  message("⬇️ Downloading health insurance data from Suaza observatory...")
  tryCatch(
    download.file(
      url = url, destfile = temp_file,
      mode = "wb", quiet = TRUE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      stop(glue("❌ Failed to download: {msg}"))
    }
  )

  message("📋 Processing data...")
  health_insurance_raw <- read_excel(temp_file)

  insurance <- health_insurance_raw |>
    mutate(
      valor = `Cobertura de aseguramiento`
    ) |>
    rename(
      anio            = Año,
      regimen         = Régimen,
      territorio      = Territorio
    ) |>
    filter(!is.na(valor), !is.na(anio), territorio == "Suaza") |>
    select(
      anio,
      regimen,
      valor
    )

  csv_dir <- file.path(output_dir, "csv")
  parquet_dir <- file.path(output_dir, "parquet")

  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  if (!dir.exists(parquet_dir)) {
    dir.create(parquet_dir, recursive = TRUE)
  }

  insurance_csv_file <- file.path(output_dir, "csv", "health_insurance.csv")
  insurance_parquet_file <- file.path(output_dir, "parquet", "health_insurance.parquet")

  write_csv(insurance, insurance_csv_file)
  write_parquet(insurance, insurance_parquet_file)

  file.remove(temp_file)

  message(glue("✅ Processed {nrow(insurance)} rows"))
  message(glue("📅 Years: {min(insurance$anio)} - {max(insurance$anio)}"))
  message(glue("👤 Regimen categories: {paste(unique(insurance$regimen), collapse = ', ')}"))
  message(glue("💾 CSV:     {insurance_csv_file}"))
  message(glue("💾 Parquet: {insurance_parquet_file}"))

  return(list(
    data         = insurance,
    output_files = c(insurance_csv_file, insurance_parquet_file)
  ))
}

if (!interactive()) {
  result <- process_health_insurance()
  cat("✅ Health insurance (Suaza) processing completed\n")
}
