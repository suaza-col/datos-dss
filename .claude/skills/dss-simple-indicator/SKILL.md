---
name: dss-simple-indicator
description: Use when adding a new DSS/Suaza indicator to this repository (datos-dss) that has NO stratifier (single value per year), produces NO map/geojson output, and whose source data must be downloaded and (optionally) unzipped, e.g. a .zip containing an .xlsx. Covers writing the R script, wiring the npm/docker script, and the output/naming conventions this repo expects. Triggers on requests like "add an indicator for X", "process this new dataset", "there's a new TerriData/observatory file for Suaza".
---

# DSS simple indicator workflow (download + optional unzip, no stratifier, no map)

This repo (`@ops-dss/data-r`) processes indicators for Suaza (Huila, Colombia).
Each indicator is a standalone R script executed inside the `r-dev` Docker
container via an `npm run r:process-<name>` script. This skill covers the
simplest shape: one source file, one value per year, no stratifying column
(e.g. `sexo`, `regimen`), no geospatial/map output.

Look at `src/indicators/education/education.R` (API/JSON source) and
`src/indicators/welfare/insurance/health_insurance.R` (direct Excel
download) as the canonical references for this pattern — read one of them
before writing a new script so the style matches exactly.

## When NOT to use this skill

- The indicator needs a stratifier column (e.g. by sex, regime, age group) —
  follow `src/indicators/health/suicide/suicide_mortality.R` instead (keeps
  the stratifier column in the output, one file, no per-category split).
- The indicator produces a choropleth/geojson map — follow
  `src/huila_map/huila_map.R` instead.
- The source is a paginated Socrata/JSON API — follow
  `src/indicators/education/education.R`'s `descargar_socrata_completa()`
  helper instead of `download.file()`.

## Steps

1. **Locate/create the script file.**
   Path convention: `src/indicators/<category>/<indicator_name>.R`
   (e.g. `src/indicators/employment/formal_employment.R`). Category matches
   the domain (`education`, `employment`, `welfare/insurance`,
   `health/suicide`, etc.). If the file already exists as an empty
   placeholder, fill it in — don't rename it.

2. **Write the script** using this structure (adapt names/URLs/columns):

   ```r
   # ==============================
   # DSS Indicator: <Human Name> - Suaza
   # ==============================
   # Source: <organization>
   # Indicator: <exact indicator label from source, if applicable>
   # URL: <source URL>
   # Filter: <entity filter, e.g. Entidad == "Suaza">
   # ==============================

   library(here)
   library(readxl)   # only if reading .xlsx
   library(dplyr)
   library(arrow)
   library(readr)
   library(fs)
   library(glue)

   process_<indicator_name> <- function(output_dir = here("outputs")) {
     url <- "<source URL, .zip or .xlsx>"

     temp_zip <- tempfile(fileext = ".zip")   # skip if source isn't zipped
     temp_dir <- tempfile("<prefix>_")
     dir_create(temp_dir)

     message("⬇️ Downloading <source> data...")
     tryCatch(
       download.file(url = url, destfile = temp_zip, mode = "wb", quiet = TRUE),
       error = function(e) stop(glue("❌ Failed to download: {conditionMessage(e)}"))
     )

     message("📦 Unzipping file...")
     unzip(temp_zip, exdir = temp_dir)

     excel_files <- dir_ls(temp_dir, glob = "*.xlsx", recurse = TRUE)
     if (length(excel_files) == 0) {
       stop("❌ No .xlsx file found inside the downloaded zip")
     }

     message("📋 Processing data...")
     raw <- read_excel(excel_files[[1]])

     data <- raw |>
       filter(<entity/indicator filters>) |>
       transmute(
         territorio = <entity column>,
         anio       = as.numeric(<year column>),
         valor      = as.numeric(<value column, cleaned>)
       ) |>
       filter(!is.na(anio), !is.na(valor)) |>
       arrange(territorio, anio)

     csv_dir <- file.path(output_dir, "csv")
     parquet_dir <- file.path(output_dir, "parquet")
     dir_create(csv_dir)
     dir_create(parquet_dir)

     csv_file <- file.path(csv_dir, "<indicator_name>.csv")
     parquet_file <- file.path(parquet_dir, "<indicator_name>.parquet")

     write_csv(data, csv_file)
     write_parquet(data, parquet_file)

     file_delete(temp_zip)
     dir_delete(temp_dir)

     message(glue("✅ Processed {nrow(data)} rows"))
     message(glue("💾 CSV:     {csv_file}"))
     message(glue("💾 Parquet: {parquet_file}"))

     return(list(data = data, output_files = c(csv_file, parquet_file)))
   }

   if (!interactive()) {
     result <- process_<indicator_name>()
     cat("✅ <Human Name> (Suaza) processing completed\n")
   }
   ```

   Conventions to preserve:
   - Snake_case output columns: `anio`, `territorio`, `valor` (add more only
     if genuinely needed — no stratifier means no extra grouping column).
   - Numeric cleanup for Spanish-formatted numbers (`.` as thousands sep,
     `,` as decimal sep) uses
     `as.numeric(gsub(",", ".", gsub("\\.", "", as.character(x))))`.
   - One output pair per indicator: `outputs/csv/<name>.csv` +
     `outputs/parquet/<name>.parquet`. Only split into multiple file pairs
     (like `education.R` does) if the source genuinely bundles several
     distinct indicators together.
   - Always delete temp files/dirs (`file_delete`, `dir_delete`) after
     writing outputs.
   - Emoji-prefixed `message()` calls at each stage (⬇️ download, 📦 unzip,
     📋 process, 💾 save, ✅ done) — matches every existing script.
   - Guard execution with `if (!interactive())` so the script is safe to
     `source()` interactively during development.

3. **Wire the npm/docker script** in `package.json` under `"scripts"`:

   ```json
   "r:process-<short-name>": "docker-compose run --rm r-dev bash -c 'Rscript src/indicators/<category>/<indicator_name>.R'"
   ```

   Check the script name doesn't already exist first — some indicators
   (e.g. `formal_employment.R` → `r:process-formality`) were pre-registered
   before the script was written.

4. **Run it** with `npm run r:process-<short-name>` (requires Docker
   running; the container mounts the repo and writes into `outputs/csv`
   and `outputs/parquet` on the host).

5. **Sanity-check the output**: row count > 0, year range printed in the
   console message looks plausible, `outputs/csv/<name>.csv` and
   `outputs/parquet/<name>.parquet` both exist and open cleanly.

## Notes specific to TerriData (DNP) sources

TerriData entity exports (`https://terridata.dnp.gov.co/assets/docs/excel/entidades/TerriData<code>.xlsx.zip`)
are zipped Excel files containing **every** indicator for one entity. Read
the single sheet with `read_excel()` (default sheet), then `filter()` down
to the specific `Indicador` string and `Entidad == "Suaza"` — don't try to
find a per-indicator URL, the whole-entity file is the only export DNP
offers. The `<code>` in the URL is the entity's DIVIPOLA-derived TerriData
id — reuse `41770` for Suaza; do not guess a new one for other entities.
