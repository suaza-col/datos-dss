# ==============================
# Shared helper: Mock Stratified DSS Indicator
# ==============================
# Generates a stratified parquet + CSV for any DSS indicator that shares the
# same structure:
#   Stratifiers: sexo, grupo_edad, zona
#   Value column: any numeric column (auto-detected)
#   Year column:  anio/año/year (auto-detected)
#
# Output column order (must match TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Aggregate labels used (must match TypeScript TOTAL_* constants):
#   sexo      → "Todos/as"
#   grupo_edad → "Todas las edades"
#   zona       → "Todas las zonas"
# ==============================

library(readxl)
library(dplyr)
library(tidyr)
library(arrow)
library(readr)
library(glue)
library(stringi)

# ── Constants ─────────────────────────────────────────────────────────────────
TOTAL_SEXO <- "Todos/as"
TOTAL_EDAD <- "Todas las edades"
TOTAL_ZONA <- "Todas las zonas"

# ── Helpers ───────────────────────────────────────────────────────────────────
normalize_name <- function(x) {
  x |>
    tolower() |>
    stri_trans_general("Latin-ASCII") |>
    trimws()
}

normalise_colnames <- function(df) {
  names(df) <- names(df) |>
    stri_trans_general("Latin-ASCII") |>
    tolower() |>
    trimws() |>
    gsub("[^a-z0-9]+", "_", x = _) |>
    gsub("_+$", "", x = _) |>
    gsub("^_+", "", x = _)
  df
}

find_col <- function(df, candidates) {
  nms <- names(df)
  for (cand in candidates) {
    m <- nms[normalize_name(nms) == normalize_name(cand)]
    if (length(m) > 0) {
      return(m[1])
    }
  }
  NULL
}

# ── Main shared function ──────────────────────────────────────────────────────
#
# @param mock_file   Path to the Excel file
# @param output_name Base name for output files (e.g. "traslado")
# @param output_dir  Directory that contains csv/ and parquet/ subdirectories
#
process_mock_stratified_indicator <- function(
    mock_file,
    output_name,
    output_dir) {
  message(glue("📋 Reading mock {output_name} data from: {mock_file}"))
  raw <- read_excel(mock_file)
  raw <- normalise_colnames(raw)

  message(glue("   Columns: {paste(names(raw), collapse = ', ')}"))
  message(glue("   Rows: {nrow(raw)}"))

  # ── Identify columns ──────────────────────────────────────────────────────
  col_anio <- find_col(raw, c("anio", "ano", "year", "a_o"))
  col_valor <- find_col(raw, c(
    "valor", "tiempo_minutos", "minutos", "tiempo", "promedio",
    "frecuencia", "porcentaje", "pct", "percent", "value", "minutes"
  ))
  col_sexo <- find_col(raw, c("sexo", "sex", "genero", "gender"))
  col_grupo_edad <- find_col(raw, c(
    "grupo_edad", "grupo_etario", "edad", "age_group",
    "rango_edad", "grupo_de_edad"
  ))
  col_zona <- find_col(raw, c("zona", "zone", "area", "region", "territorio"))

  col_label <- function(x) if (is.null(x)) "(not found)" else x
  message(glue("   anio       → {col_label(col_anio)}"))
  message(glue("   valor      → {col_label(col_valor)}"))
  message(glue("   sexo       → {col_label(col_sexo)}"))
  message(glue("   grupo_edad → {col_label(col_grupo_edad)}"))
  message(glue("   zona       → {col_label(col_zona)}"))

  if (is.null(col_anio) || is.null(col_valor)) {
    stop(
      "Cannot find required columns 'anio' and 'valor' in the Excel file. ",
      "Found columns: ", paste(names(raw), collapse = ", ")
    )
  }

  # ── Build working data frame ──────────────────────────────────────────────
  data <- raw

  data <- rename(data, anio = !!sym(col_anio), valor = !!sym(col_valor))
  data$anio <- as.integer(data$anio)
  data$valor <- as.numeric(data$valor)

  if (!is.null(col_sexo)) {
    data <- rename(data, sexo = !!sym(col_sexo))
  } else {
    data$sexo <- TOTAL_SEXO
    message("   sexo column not found — treating all rows as 'Todos/as'")
  }

  if (!is.null(col_grupo_edad)) {
    data <- rename(data, grupo_edad = !!sym(col_grupo_edad))
  } else {
    data$grupo_edad <- TOTAL_EDAD
    message("   grupo_edad column not found — treating all rows as 'Todas las edades'")
  }

  if (!is.null(col_zona)) {
    data <- rename(data, zona = !!sym(col_zona))
  } else {
    data$zona <- TOTAL_ZONA
    message("   zona column not found — treating all rows as 'Todas las zonas'")
  }

  data <- data |>
    filter(!is.na(anio), !is.na(valor)) |>
    select(anio, valor, sexo, grupo_edad, zona)

  message(glue("   Valid rows after filtering: {nrow(data)}"))

  # ── Normalise aggregate labels ─────────────────────────────────────────────
  total_sexo_patterns <- c("total", "todos", "todas", "todos/as", "all")
  total_edad_patterns <- c(
    "total", "todas", "todas las edades", "todos",
    "all ages", "all", "total edades"
  )
  total_zona_patterns <- c(
    "total", "todas", "todas las zonas", "todos",
    "all zones", "all areas", "all", "total zonas", "urbano y rural"
  )

  data <- data |>
    mutate(
      sexo = ifelse(
        normalize_name(sexo) %in% total_sexo_patterns,
        TOTAL_SEXO, sexo
      ),
      grupo_edad = ifelse(
        normalize_name(grupo_edad) %in% total_edad_patterns,
        TOTAL_EDAD, grupo_edad
      ),
      zona = ifelse(
        normalize_name(zona) %in% total_zona_patterns,
        TOTAL_ZONA, zona
      )
    )

  # ── Separate non-aggregate values ─────────────────────────────────────────
  raw_sexo_vals <- setdiff(unique(data$sexo), TOTAL_SEXO)
  raw_edad_vals <- setdiff(unique(data$grupo_edad), TOTAL_EDAD)
  raw_zona_vals <- setdiff(unique(data$zona), TOTAL_ZONA)

  # ── Compute aggregate rows (all 7 combinations) ───────────────────────────
  # Start from only the finest-granularity rows (all three strats are specific)
  finest <- data |>
    filter(sexo != TOTAL_SEXO, grupo_edad != TOTAL_EDAD, zona != TOTAL_ZONA)

  if (nrow(finest) == 0) {
    # Fallback: use whatever partially-specific rows exist
    finest <- data |>
      filter(
        !(sexo == TOTAL_SEXO & grupo_edad == TOTAL_EDAD & zona == TOTAL_ZONA)
      )
  }

  # Edge case: the Excel only contains fully-aggregated rows (all three totals).
  # In this case, skip recomputing aggregates and use the data as-is so a
  # usable output is always written.
  if (nrow(finest) == 0) {
    message("   All rows are already fully aggregated — skipping aggregate computation.")
    out <- data |>
      select(anio, valor, sexo, grupo_edad, zona) |>
      arrange(anio, zona, sexo, grupo_edad)

    message(glue("   Output rows: {nrow(out)}"))

    dir.create(file.path(output_dir, "csv"),     showWarnings = FALSE, recursive = TRUE)
    dir.create(file.path(output_dir, "parquet"), showWarnings = FALSE, recursive = TRUE)

    csv_file <- file.path(output_dir, "csv", paste0(output_name, ".csv"))
    parquet_file <- file.path(output_dir, "parquet", paste0(output_name, ".parquet"))

    write_csv(out, csv_file)
    write_parquet(out, parquet_file)

    message(glue("💾 {output_name}.csv     → {csv_file}"))
    message(glue("💾 {output_name}.parquet → {parquet_file}"))
    message(glue("✅ Mock {output_name} processing completed."))

    return(invisible(list(data = out, output_files = c(csv_file, parquet_file))))
  }

  # Helper: mean aggregation
  agg <- function(df, ...) {
    df |>
      group_by(anio, ...) |>
      summarise(valor = mean(valor, na.rm = TRUE), .groups = "drop")
  }

  # 1. Total (all three dimensions collapsed)
  agg_total <- agg(finest) |>
    mutate(sexo = TOTAL_SEXO, grupo_edad = TOTAL_EDAD, zona = TOTAL_ZONA)

  # 2. By sexo only (edad + zona collapsed)
  agg_sexo <- agg(finest, sexo) |>
    mutate(grupo_edad = TOTAL_EDAD, zona = TOTAL_ZONA)

  # 3. By grupo_edad only (sexo + zona collapsed)
  agg_edad <- agg(finest, grupo_edad) |>
    mutate(sexo = TOTAL_SEXO, zona = TOTAL_ZONA)

  # 4. By zona only (sexo + edad collapsed)
  agg_zona <- agg(finest, zona) |>
    mutate(sexo = TOTAL_SEXO, grupo_edad = TOTAL_EDAD)

  # 5. By sexo × grupo_edad (zona collapsed)
  agg_sexo_edad <- agg(finest, sexo, grupo_edad) |>
    mutate(zona = TOTAL_ZONA)

  # 6. By sexo × zona (edad collapsed)
  agg_sexo_zona <- agg(finest, sexo, zona) |>
    mutate(grupo_edad = TOTAL_EDAD)

  # 7. By grupo_edad × zona (sexo collapsed)
  agg_edad_zona <- agg(finest, grupo_edad, zona) |>
    mutate(sexo = TOTAL_SEXO)

  # Combine: keep finest-grain rows + all aggregates
  out <- bind_rows(
    finest,
    agg_total,
    agg_sexo,
    agg_edad,
    agg_zona,
    agg_sexo_edad,
    agg_sexo_zona,
    agg_edad_zona
  ) |>
    distinct(anio, sexo, grupo_edad, zona, .keep_all = TRUE) |>
    select(anio, valor, sexo, grupo_edad, zona) |>
    arrange(anio, zona, sexo, grupo_edad)

  message(glue("   Output rows: {nrow(out)}"))
  message(glue("   Years:       {paste(sort(unique(out$anio)),       collapse = ', ')}"))
  message(glue("   Sexo:        {paste(sort(unique(out$sexo)),       collapse = ', ')}"))
  message(glue("   Grupo_edad:  {paste(sort(unique(out$grupo_edad)), collapse = ', ')}"))
  message(glue("   Zona:        {paste(sort(unique(out$zona)),       collapse = ', ')}"))

  # ── Write outputs ─────────────────────────────────────────────────────────
  dir.create(file.path(output_dir, "csv"),     showWarnings = FALSE, recursive = TRUE)
  dir.create(file.path(output_dir, "parquet"), showWarnings = FALSE, recursive = TRUE)

  csv_file <- file.path(output_dir, "csv", paste0(output_name, ".csv"))
  parquet_file <- file.path(output_dir, "parquet", paste0(output_name, ".parquet"))

  write_csv(out, csv_file)
  write_parquet(out, parquet_file)

  message(glue("💾 {output_name}.csv     → {csv_file}"))
  message(glue("💾 {output_name}.parquet → {parquet_file}"))
  message(glue("✅ Mock {output_name} processing completed."))

  invisible(list(data = out, output_files = c(csv_file, parquet_file)))
}
