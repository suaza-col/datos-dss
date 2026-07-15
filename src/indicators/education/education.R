# ==============================
# DSS Indicator: Education - Suaza
# ==============================
# Source: Ministerio de Educación Nacional (datos.gov.co)
# Dataset: MEN - Estadísticas en Educación Preescolar, Básica y Media por Municipio
# URL: https://www.datos.gov.co/resource/nudc-7mev.json
# Indicators: cobertura_bruta, cobertura_neta, deserci_n, aprobaci_n, reprobaci_n, repitencia
# Filter: municipio == "Suaza"
# ==============================

library(here)
library(jsonlite)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)

#' Download full Socrata dataset with pagination
#'
#' @param base_url Base API URL
#' @param batch_size Number of records per request
#' @param extra_query Optional additional query string (without leading '?'),
#'   e.g. "$select=...&$where=...". Applied to every paginated request.
#' @return Combined data frame of all records
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
      error = function(e) {
        stop(
          glue(
            "Error al descargar datos desde la URL '{url}' (offset = {offset}): {conditionMessage(e)}"
          ),
          call. = FALSE
        )
      }
    )

    if (nrow(batch) == 0) break

    lista_batches[[i]] <- batch

    if (nrow(batch) < batch_size) break

    offset <- offset + batch_size
    i <- i + 1
  }

  bind_rows(lista_batches)
}

process_education_suaza <- function(output_dir = here("outputs")) {
  base_url <- "https://www.datos.gov.co/resource/nudc-7mev.json"

  message("⬇️  Downloading education data from datos.gov.co...")
  # Limit Socrata download to Suaza and required columns
  select_fields <- paste(
    c(
      "a_o",
      "c_digo_municipio",
      "municipio",
      "c_digo_departamento",
      "departamento",
      "cobertura_bruta",
      "cobertura_neta",
      "deserci_n",
      "aprobaci_n",
      "reprobaci_n",
      "repitencia"
    ),
    collapse = ","
  )
  where_clause <- URLencode("municipio = 'Suaza'", reserved = TRUE)
  extra_query <- glue("$select={select_fields}&$where={where_clause}")
  men_educacion_raw <- descargar_socrata_completa(base_url, extra_query = extra_query)

  message("📋 Processing data...")

  men_educacion <- men_educacion_raw |>
    select(
      a_o,
      c_digo_municipio,
      municipio,
      c_digo_departamento,
      departamento,
      cobertura_bruta,
      cobertura_neta,
      deserci_n,
      aprobaci_n,
      reprobaci_n,
      repitencia
    ) |>
    mutate(
      anio             = as.integer(a_o),
      cobertura_bruta  = as.numeric(cobertura_bruta),
      cobertura_neta   = as.numeric(cobertura_neta),
      deserci_n        = as.numeric(deserci_n),
      aprobaci_n       = as.numeric(aprobaci_n),
      reprobaci_n      = as.numeric(reprobaci_n),
      repitencia       = as.numeric(repitencia)
    ) |>
    filter(!is.na(anio))

  # Filter to Suaza only
  education_suaza <- men_educacion |>
    filter(municipio == "Suaza") |>
    select(
      anio,
      municipio,
      departamento,
      cobertura_bruta,
      cobertura_neta,
      deserci_n,
      aprobaci_n,
      reprobaci_n,
      repitencia
    ) |>
    arrange(anio)

  message(glue("  Rows for Suaza: {nrow(education_suaza)}"))

  # Create output directories
  dir_create(file.path(output_dir, "csv"))
  dir_create(file.path(output_dir, "parquet"))

  # One file per indicator, each with just anio + valor
  indicadores <- c(
    cobertura_bruta = "education_cobertura_bruta",
    cobertura_neta  = "education_cobertura_neta",
    deserci_n       = "education_desercion",
    aprobaci_n      = "education_aprobacion",
    reprobaci_n     = "education_reprobacion",
    repitencia      = "education_repitencia"
  )

  output_files <- character(0)

  for (col in names(indicadores)) {
    file_stub <- indicadores[[col]]

    data_indicador <- education_suaza |>
      select(anio, territorio = municipio, valor = all_of(col)) |>
      filter(!is.na(valor))

    csv_file     <- file.path(output_dir, "csv", glue("{file_stub}.csv"))
    parquet_file <- file.path(
      output_dir, "parquet", glue("{file_stub}.parquet")
    )

    write_csv(data_indicador, csv_file)
    write_parquet(data_indicador, parquet_file)

    message(glue("💾 {file_stub}: {csv_file}"))
    output_files <- c(output_files, csv_file, parquet_file)
  }

  message(glue("✅ Processed {nrow(education_suaza)} rows for Suaza"))
  if (nrow(education_suaza) > 0) {
    anio_min <- min(education_suaza$anio)
    anio_max <- max(education_suaza$anio)
    message(glue("📅 Years: {anio_min} - {anio_max}"))
  }

  return(list(
    data         = education_suaza,
    output_files = output_files
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_education_suaza()
  cat("✅ Education (Suaza) processing completed\n")
}
