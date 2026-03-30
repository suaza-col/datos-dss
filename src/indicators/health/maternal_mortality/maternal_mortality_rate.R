# ==============================
# DSS Indicator: Maternal Mortality Rate + Inequity
# ==============================
# Source: Observatorio de Salud del Huila
# Indicator: Mortalidad materna (por 100.000 nacidos vivos)
# Inequity proxy: Deserción escolar municipal (MEN)
# ==============================

library(here)
library(readxl)
library(dplyr)
library(arrow)
library(readr)
library(fs)
library(glue)
library(jsonlite)
library(tidyr)
library(rlang)

process_maternal_mortality_rate <- function(output_dir = here("outputs")) {
  # ── 1. Download and process maternal mortality data ──────────────────────────
  url <- "https://www.huila.gov.co/observatoriosalud/loader.php?lServicio=Tools2&lTipo=descargas&lFuncion=descargar&idFile=85285"
  temp_file <- tempfile(fileext = ".xlsx")

  message("⬇️ Downloading maternal mortality data from Huila observatory...")
  tryCatch(
    download.file(url = url, destfile = temp_file, mode = "wb", quiet = TRUE),
    error = function(e) stop(glue("❌ Failed to download: {conditionMessage(e)}"))
  )

  message("📋 Processing maternal mortality data...")
  mortalidad_materna_raw <- read_excel(temp_file)
  file.remove(temp_file)

  # All municipalities — needed for quintile analysis
  mortalidad_all <- mortalidad_materna_raw |>
    mutate(
      iso3 = "COL",
      valor = `Defunciones maternas` / Nacimientos * 100000,
      indicador = "Mortalidad materna (100.000 nacidos vivos)"
    ) |>
    rename(
      cod_local = `Código DANE`,
      anio = Año
    ) |>
    select(iso3, Territorio, cod_local, anio, valor, Nacimientos)

  # Nacional + Huila only — for the trend line chart
  mortalidad_materna <- mortalidad_all |>
    filter(Territorio %in% c("Nacional", "Huila")) |>
    select(iso3, Territorio, cod_local, anio, valor)

  # ── 2. Download education data (Deserción as SDH proxy) ──────────────────────
  message("⬇️ Downloading education data from datos.gov.co...")
  base_url <- "https://www.datos.gov.co/resource/nudc-7mev.json"

  descargar_socrata_completa <- function(base_url, batch_size = 50000) {
    offset <- 0
    lista_batches <- list()
    i <- 1
    repeat {
      url_batch <- paste0(base_url, "?$limit=", batch_size, "&$offset=", offset)
      message(paste("  Descargando desde offset =", offset))
      batch <- fromJSON(url_batch, flatten = TRUE)
      if (nrow(batch) == 0) break
      lista_batches[[i]] <- batch
      if (nrow(batch) < batch_size) break
      offset <- offset + batch_size
      i <- i + 1
    }
    bind_rows(lista_batches)
  }

  men_educacion_municipio <- descargar_socrata_completa(base_url) |>
    select(
      a_o, c_digo_municipio, municipio, c_digo_departamento, departamento,
      cobertura_bruta, cobertura_neta, deserci_n,
      aprobaci_n, reprobaci_n, repitencia
    ) |>
    rename(
      anio = a_o,
      Territorio = municipio,
      "Cobertura Bruta" = cobertura_bruta,
      "Cobertura Neta" = cobertura_neta,
      "Deserción" = deserci_n,
      "Aprobación" = aprobaci_n,
      "Reprobación" = reprobaci_n,
      "Repitencia" = repitencia
    ) |>
    mutate(
      anio      = as.double(anio),
      cod_local = as.character(c_digo_municipio)
    )

  # ── 3. Join datasets ──────────────────────────────────────────────────────────
  # Join on DANE municipality code to avoid many-to-many matches from repeated
  # municipality names across departments.
  message("🔗 Joining mortality and education data...")
  data_cruce <- mortalidad_all |>
    mutate(cod_local = as.character(cod_local)) |>
    left_join(men_educacion_municipio, by = c("anio", "cod_local")) |>
    filter(!is.na(Deserción)) |>
    rename(
      territorio      = Territorio.x,
      cobertura_bruta = "Cobertura Bruta",
      cobertura_neta  = "Cobertura Neta",
      desercion       = "Deserción",
      aprobacion      = "Aprobación",
      reprobacion     = "Reprobación",
      repitencia      = "Repitencia",
      nacimientos     = Nacimientos
    ) |>
    mutate(across(c(
      valor, cobertura_bruta, cobertura_neta, desercion,
      aprobacion, reprobacion, repitencia, nacimientos
    ), as.numeric)) |>
    select(
      anio, territorio, valor, cobertura_bruta, cobertura_neta,
      desercion, aprobacion, reprobacion, repitencia, nacimientos
    )

  # ── 4. Quintile helper functions ──────────────────────────────────────────────
  crear_quintiles <- function(data, var_quintil, ..., n = 5, nombre_quintil = "quintil") {
    grupos <- enquos(...)
    if (length(grupos) > 0) {
      data <- data |> group_by(!!!grupos)
    }
    data |>
      mutate(
        !!sym(nombre_quintil) := if_else(
          is.na({{ var_quintil }}),
          NA_integer_,
          ntile({{ var_quintil }}, n)
        )
      ) |>
      ungroup()
  }

  calcular_outcome_quintil <- function(data, var_quintil, var_outcome, var_peso,
                                       var_anio = NULL, nombre_quintil = "quintil") {
    q_quintil <- enquo(var_quintil)
    q_outcome <- enquo(var_outcome)
    q_peso <- enquo(var_peso)
    q_anio <- enquo(var_anio)
    usar_anio <- !quo_is_null(q_anio)

    if (usar_anio) {
      resultado <- data |>
        group_by(!!q_anio, !!q_quintil) |>
        summarise(
          tasa_ponderada = sum((!!q_outcome) * (!!q_peso), na.rm = TRUE) /
            sum(ifelse(!is.na(!!q_outcome), !!q_peso, 0), na.rm = TRUE),
          n = sum(!is.na(!!q_outcome) & !is.na(!!q_peso)),
          outcome_grupo = list((!!q_outcome)[!is.na(!!q_outcome) & !is.na(!!q_peso)]),
          pesos_grupo = list((!!q_peso)[!is.na(!!q_outcome) & !is.na(!!q_peso)]),
          total_pob = sum(!!q_peso, na.rm = TRUE),
          .groups = "drop"
        )
    } else {
      resultado <- data |>
        group_by(!!q_quintil) |>
        summarise(
          tasa_ponderada = sum((!!q_outcome) * (!!q_peso), na.rm = TRUE) /
            sum(ifelse(!is.na(!!q_outcome), !!q_peso, 0), na.rm = TRUE),
          n = sum(!is.na(!!q_outcome) & !is.na(!!q_peso)),
          outcome_grupo = list((!!q_outcome)[!is.na(!!q_outcome) & !is.na(!!q_peso)]),
          pesos_grupo = list((!!q_peso)[!is.na(!!q_outcome) & !is.na(!!q_peso)]),
          total_pob = sum(!!q_peso, na.rm = TRUE),
          .groups = "drop"
        )
    }

    resultado |>
      rowwise() |>
      mutate(
        sd_pond = ifelse(
          n > 1,
          sqrt(
            sum(
              (unlist(pesos_grupo) / sum(unlist(pesos_grupo))) *
                (unlist(outcome_grupo) - tasa_ponderada)^2
            ) * (n / (n - 1))
          ),
          NA_real_
        ),
        se = ifelse(n > 1, sd_pond / sqrt(n), NA_real_),
        ic_inf = tasa_ponderada - 1.96 * se,
        ic_sup = tasa_ponderada + 1.96 * se
      ) |>
      ungroup() |>
      rename(!!nombre_quintil := !!q_quintil)
  }

  # ── 5. Calculate quintiles and weighted summary ───────────────────────────────
  message("📊 Calculating quintiles...")
  datos <- crear_quintiles(
    data = data_cruce,
    var_quintil = desercion,
    anio, # unnamed → captured in ... → groups by anio
    n = 5,
    nombre_quintil = "quintil_desercion"
  )

  resumen <- calcular_outcome_quintil(
    data           = datos,
    var_quintil    = quintil_desercion,
    var_outcome    = valor,
    var_peso       = nacimientos,
    var_anio       = anio,
    nombre_quintil = "quintil_dss"
  )

  # Drop list columns — not supported by Parquet / Arrow
  resumen_save <- resumen |>
    select(anio, quintil_dss, tasa_ponderada, n, total_pob, sd_pond, se, ic_inf, ic_sup)

  # ── 6. Calculate absolute and relative gaps (Q5 vs Q1) ───────────────────────
  message("📊 Calculating gaps...")

  calcular_brechas <- function(data, var_estrato, var_valor, grupo_ref, grupo_comp,
                               var_se, var_anio = NULL, var_territorio = NULL,
                               territorios = NULL) {
    q_estrato <- enquo(var_estrato)
    q_valor <- enquo(var_valor)
    q_se <- enquo(var_se)
    q_anio <- enquo(var_anio)
    q_territorio <- enquo(var_territorio)
    usar_anio <- !quo_is_null(q_anio)
    usar_territorio <- !quo_is_null(q_territorio)

    datos_filtrados <- data |>
      filter((!!q_estrato) %in% c(grupo_ref, grupo_comp))

    if (usar_territorio && !is.null(territorios)) {
      datos_filtrados <- datos_filtrados |>
        filter((!!q_territorio) %in% territorios)
    }

    vars_id <- c()
    if (usar_anio) vars_id <- c(vars_id, as_name(q_anio))
    if (usar_territorio) vars_id <- c(vars_id, as_name(q_territorio))

    datos_filtrados |>
      mutate(estrato_tmp = as.character(!!q_estrato)) |>
      select(
        all_of(vars_id),
        estrato_tmp,
        valor_tmp = !!q_valor,
        se_tmp    = !!q_se
      ) |>
      pivot_wider(
        names_from  = estrato_tmp,
        values_from = c(valor_tmp, se_tmp),
        names_sep   = "_"
      ) |>
      mutate(
        valor_ref = .data[[paste0("valor_tmp_", grupo_ref)]],
        valor_comp = .data[[paste0("valor_tmp_", grupo_comp)]],
        se_ref = .data[[paste0("se_tmp_", grupo_ref)]],
        se_comp = .data[[paste0("se_tmp_", grupo_comp)]],
        brecha_absoluta = valor_comp - valor_ref,
        se_brecha_abs = sqrt(se_ref^2 + se_comp^2),
        ic_inf_abs = brecha_absoluta - 1.96 * se_brecha_abs,
        ic_sup_abs = brecha_absoluta + 1.96 * se_brecha_abs,
        brecha_relativa = ifelse(valor_ref > 0 & valor_comp > 0, valor_comp / valor_ref, NA_real_),
        se_log_rr = ifelse(
          valor_ref > 0 & valor_comp > 0,
          sqrt((se_ref / valor_ref)^2 + (se_comp / valor_comp)^2),
          NA_real_
        ),
        ic_inf_rel = ifelse(
          !is.na(se_log_rr),
          exp(log(brecha_relativa) - 1.96 * se_log_rr),
          NA_real_
        ),
        ic_sup_rel = ifelse(
          !is.na(se_log_rr),
          exp(log(brecha_relativa) + 1.96 * se_log_rr),
          NA_real_
        )
      ) |>
      select(
        all_of(vars_id),
        valor_ref, valor_comp,
        brecha_absoluta, ic_inf_abs, ic_sup_abs,
        brecha_relativa, ic_inf_rel, ic_sup_rel
      )
  }

  brecha_quintiles <- calcular_brechas(
    data        = resumen,
    var_estrato = quintil_dss,
    var_valor   = tasa_ponderada,
    var_se      = se,
    grupo_ref   = 1,
    grupo_comp  = 5,
    var_anio    = anio
  )

  # ── 7. Spearman correlation (cross-sectional, last year) ──────────────────────
  # Matches the original R code: filter data_cruce to the most recent year,
  # then correlate each education indicator with maternal mortality across
  # all Huila municipalities (n ≈ 40).
  message("📊 Computing Spearman correlations (last year, cross-sectional)...")

  ultimo_anio  <- max(data_cruce$anio, na.rm = TRUE)
  data_ultimo  <- data_cruce |> filter(anio == ultimo_anio)

  vars_dss_cor <- c(
    "cobertura_bruta",
    "cobertura_neta",
    "desercion",
    "aprobacion",
    "reprobacion",
    "repitencia"
  )

  labels_dss_cor <- c(
    cobertura_bruta = "Cobertura Bruta",
    cobertura_neta  = "Cobertura Neta",
    desercion       = "Deserción",
    aprobacion      = "Aprobación",
    reprobacion     = "Reprobación",
    repitencia      = "Repitencia"
  )

  ic_cor_fn <- function(r, n) {
    if (is.na(r) || n <= 3 || abs(r) >= 1) return(c(NA_real_, NA_real_))
    z     <- 0.5 * log((1 + r) / (1 - r))
    se    <- 1 / sqrt(n - 3)
    z_inf <- z - 1.96 * se
    z_sup <- z + 1.96 * se
    r_inf <- (exp(2 * z_inf) - 1) / (exp(2 * z_inf) + 1)
    r_sup <- (exp(2 * z_sup) - 1) / (exp(2 * z_sup) + 1)
    c(r_inf, r_sup)
  }

  tabla_cor <- lapply(vars_dss_cor, function(v) {
    df <- data_ultimo[, c("valor", v)]
    df <- df[complete.cases(df), ]
    n  <- nrow(df)

    if (n < 5) {
      return(data.frame(
        indicador   = v,
        label       = labels_dss_cor[[v]],
        correlacion = NA_real_,
        ci_lower    = NA_real_,
        ci_upper    = NA_real_,
        p_value     = NA_real_,
        n           = n
      ))
    }

    ct  <- suppressWarnings(cor.test(df[[1]], df[[2]], method = "spearman"))
    r   <- as.numeric(ct$estimate)
    p   <- ct$p.value
    ic  <- ic_cor_fn(r, n)

    data.frame(
      indicador   = v,
      label       = labels_dss_cor[[v]],
      correlacion = r,
      ci_lower    = ic[1],
      ci_upper    = ic[2],
      p_value     = p,
      n           = n
    )
  })

  correlation_data <- bind_rows(tabla_cor) |> arrange(correlacion)

  # ── 8. Save outputs ───────────────────────────────────────────────────────────
  dir_create(file.path(output_dir, "csv"))
  dir_create(file.path(output_dir, "parquet"))

  # Trend line chart data (Nacional + Huila)
  rate_csv <- file.path(output_dir, "csv", "maternal_mortality_rate.csv")
  rate_parquet <- file.path(output_dir, "parquet", "maternal_mortality_rate.parquet")
  write_csv(mortalidad_materna, rate_csv)
  write_parquet(mortalidad_materna, rate_parquet)

  # Quintil summary data (resumen without list columns)
  quintil_csv <- file.path(output_dir, "csv", "maternal_mortality_quintiles.csv")
  quintil_parquet <- file.path(output_dir, "parquet", "maternal_mortality_quintiles.parquet")
  write_csv(resumen_save, quintil_csv)
  write_parquet(resumen_save, quintil_parquet)

  # Gaps data (brecha Q5 vs Q1 over time)
  gaps_csv <- file.path(output_dir, "csv", "maternal_mortality_gaps.csv")
  gaps_parquet <- file.path(output_dir, "parquet", "maternal_mortality_gaps.parquet")
  write_csv(brecha_quintiles, gaps_csv)
  write_parquet(brecha_quintiles, gaps_parquet)

  # Spearman correlation forest-plot data
  forest_csv     <- file.path(output_dir, "csv",     "forest_plot_suaza.csv")
  forest_parquet <- file.path(output_dir, "parquet", "forest_plot_suaza.parquet")
  write_csv(correlation_data,     forest_csv)
  write_parquet(correlation_data, forest_parquet)

  # Cross-sectional scatter data (all municipalities, all years)
  scatter_csv <- file.path(output_dir, "csv", "scatter_maternal.csv")
  scatter_parquet <- file.path(output_dir, "parquet", "scatter_maternal.parquet")
  write_csv(data_cruce, scatter_csv)
  write_parquet(data_cruce, scatter_parquet)

  # Temporal analytics: Huila rate + weighted-mean education indicators by year
  data_edu_agg <- data_cruce |>
    group_by(anio) |>
    summarise(
      cobertura_bruta = weighted.mean(cobertura_bruta, nacimientos, na.rm = TRUE),
      cobertura_neta = weighted.mean(cobertura_neta, nacimientos, na.rm = TRUE),
      desercion = weighted.mean(desercion, nacimientos, na.rm = TRUE),
      aprobacion = weighted.mean(aprobacion, nacimientos, na.rm = TRUE),
      reprobacion = weighted.mean(reprobacion, nacimientos, na.rm = TRUE),
      repitencia = weighted.mean(repitencia, nacimientos, na.rm = TRUE),
      .groups = "drop"
    )

  data_analytics_maternal <- mortalidad_materna |>
    filter(Territorio == "Huila") |>
    select(anio, valor) |>
    inner_join(data_edu_agg, by = "anio") |>
    arrange(anio)

  analytics_maternal_csv <- file.path(output_dir, "csv", "analytics_maternal.csv")
  analytics_maternal_parquet <- file.path(output_dir, "parquet", "analytics_maternal.parquet")
  write_csv(data_analytics_maternal, analytics_maternal_csv)
  write_parquet(data_analytics_maternal, analytics_maternal_parquet)

  message(glue("✅ Maternal mortality data processed and saved to: {output_dir}"))
  message(glue("💾 Rate CSV:              {rate_csv}"))
  message(glue("💾 Quintil CSV:           {quintil_csv}"))
  message(glue("💾 Gaps CSV:              {gaps_csv}"))
  message(glue("💾 Forest plot CSV:       {forest_csv}"))
  message(glue("💾 Scatter CSV:           {scatter_csv}"))
  message(glue("💾 Analytics temporal CSV:{analytics_maternal_csv}"))

  return(list(
    data = mortalidad_materna,
    quintil_data = resumen_save,
    gaps_data = brecha_quintiles,
    correlation_data = correlation_data,
    scatter_data = data_cruce,
    analytics_maternal = data_analytics_maternal,
    output_files = c(
      rate_csv, rate_parquet,
      quintil_csv, quintil_parquet,
      gaps_csv, gaps_parquet,
      scatter_csv, scatter_parquet,
      analytics_maternal_csv, analytics_maternal_parquet
    )
  ))
}

# Main execution — called from Turborepo or command line
if (!interactive()) {
  result <- process_maternal_mortality_rate()
  cat("✅ Maternal mortality rate data processing completed.\n")
}
