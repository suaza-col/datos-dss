# ==============================
# DSS Indicator: Suicide Mortality - Suaza
# ==============================
# Source: Observatorio de Salud de Suaza
# Indicator: Mortalidad por suicidio (por 100.000 hab.)
# Stratifier: sexo (gender)
# ==============================

library(here)
library(readxl)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)
source(here("R/util_gaps.R"))

process_suicide <- function(output_dir = here("outputs")) {
  url <- "https://www.huila.gov.co/observatoriosalud/loader.php?lServicio=Tools2&lTipo=descargas&lFuncion=descargar&idFile=84079"

  temp_file <- tempfile()

  message("⬇️ Downloading suicide mortality data from Suaza observatory...")
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
  suicidio_raw <- read_excel(temp_file)

  suicidio <- suicidio_raw |>
    mutate(
      iso3      = "COL",
      valor     = Suicidios / Población * 100000,
      indicador = "Mortalidad por suicidio (100.000 hab.)"
    ) |>
    rename(
      cod_subnacional = Regional,
      cod_local       = `Cod - Terr`,
      anio            = Año,
      sexo            = Sexo,
      territorio      = Territorio
    ) |>
    select(
      territorio,
      cod_local,
      anio,
      sexo,
      valor
    ) |>
    filter(!is.na(valor), !is.na(anio), cod_local == "41770 - Suaza")

  # Create output directories
  csv_dir <- file.path(output_dir, "csv")
  parquet_dir <- file.path(output_dir, "parquet")

  if (!dir.exists(csv_dir)) {
    dir.create(csv_dir, recursive = TRUE)
  }

  if (!dir.exists(parquet_dir)) {
    dir.create(parquet_dir, recursive = TRUE)
  }

  # Save outputs
  suicide_csv_file <- file.path(output_dir, "csv", "mortalidad-suicidio.csv")
  suicide_parquet_file <- file.path(output_dir, "parquet", "mortalidad-suicidio.parquet")

  write_csv(suicidio, suicide_csv_file)
  write_parquet(suicidio, suicide_parquet_file)

  file.remove(temp_file)

  message(glue("✅ Processed {nrow(suicidio)} rows"))
  message(glue("📅 Years: {min(suicidio$anio)} - {max(suicidio$anio)}"))
  message(glue("👤 Sex categories: {paste(unique(suicidio$sexo), collapse = ', ')}"))
  message(glue("💾 CSV:     {suicide_csv_file}"))
  message(glue("💾 Parquet: {suicide_parquet_file}"))

  return(list(
    data         = suicidio,
    output_files = c(suicide_csv_file, suicide_parquet_file)
  ))
}

if (!interactive()) {
  result <- process_suicide()
  cat("✅ Suicide mortality (Suaza) processing completed\n")
}
