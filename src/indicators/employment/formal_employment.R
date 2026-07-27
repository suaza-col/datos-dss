# ==============================
# DSS Indicator: Formal Employment - Suaza
# ==============================
# Source: TerriData (DNP - Departamento Nacional de Planeación)
# Indicator: Porcentaje de personas ocupadas formalmente con respecto a la
#            población total
# URL: https://terridata.dnp.gov.co/assets/docs/excel/entidades/TerriData41770.xlsx.zip
# Filter: Entidad == "Suaza"
# Note: the source file is a .zip containing a single .xlsx with all
#   TerriData indicators for the entity (Suaza). It is downloaded and
#   unzipped here before being read.
# ==============================

library(here)
library(readxl)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)

process_formal_employment <- function(output_dir = here("outputs")) {
  url <- "https://terridata.dnp.gov.co/assets/docs/excel/entidades/TerriData41770.xlsx.zip"

  temp_zip <- tempfile(fileext = ".zip")
  temp_dir <- tempfile("terridata_")
  dir_create(temp_dir)

  message("⬇️ Downloading TerriData (Suaza) data from DNP...")
  tryCatch(
    download.file(
      url = url, destfile = temp_zip,
      mode = "wb", quiet = TRUE
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      stop(glue("❌ Failed to download: {msg}"))
    }
  )

  message("📦 Unzipping TerriData file...")
  unzip(temp_zip, exdir = temp_dir)

  excel_files <- dir_ls(temp_dir, glob = "*.xlsx", recurse = TRUE)
  if (length(excel_files) == 0) {
    stop("❌ No .xlsx file found inside the downloaded TerriData zip")
  }

  message("📋 Processing data...")
  terridata_raw <- read_excel(excel_files[[1]])

  formal_employment <- terridata_raw |>
    filter(
      Entidad == "Suaza",
      Indicador == "Porcentaje de personas ocupadas formalmente con respecto a la población total"
    ) |>
    transmute(
      anio = as.numeric(Año),
      territorio = Entidad,
      valor = as.numeric(
        gsub(",", ".", gsub("\\.", "", as.character(`Dato Numérico`)))
      )
    ) |>
    filter(!is.na(anio), !is.na(valor)) |>
    arrange(territorio, anio)

  csv_dir <- file.path(output_dir, "csv")
  parquet_dir <- file.path(output_dir, "parquet")
  dir_create(csv_dir)
  dir_create(parquet_dir)

  csv_file <- file.path(csv_dir, "formal_employment.csv")
  parquet_file <- file.path(parquet_dir, "formal_employment.parquet")

  write_csv(formal_employment, csv_file)
  write_parquet(formal_employment, parquet_file)

  file_delete(temp_zip)
  dir_delete(temp_dir)

  message(glue("✅ Processed {nrow(formal_employment)} rows"))
  if (nrow(formal_employment) > 0) {
    anio_min <- min(formal_employment$anio)
    anio_max <- max(formal_employment$anio)
    message(glue("📅 Years: {anio_min} - {anio_max}"))
  }
  message(glue("💾 CSV:     {csv_file}"))
  message(glue("💾 Parquet: {parquet_file}"))

  return(list(
    data         = formal_employment,
    output_files = c(csv_file, parquet_file)
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_formal_employment()
  cat("✅ Formal employment (Suaza) processing completed\n")
}
