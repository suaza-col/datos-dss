# =========================================================
# San Martín del Valle - Frecuencia de transporte público subsidiado
# Cobertura hacia centros de salud
# Tendencia al alza; estratificado por zona y etnia
# =========================================================

# ---------------------------
# 1. Carpeta de salida
# ---------------------------
library(here)

output_dir <- here("outputs")

if (!dir.exists(output_dir)) {
  stop("La carpeta de salida no existe. Revisa la ruta.")
}

# ---------------------------
# 2. Paquetes
# ---------------------------
library(dplyr)
library(tidyr)
library(readr)
library(arrow)

# ---------------------------
# 3. Semilla
# ---------------------------
set.seed(6060)

# ---------------------------
# 4. Leer base territorial
# ---------------------------
sim_csv_file <- file.path(output_dir, "csv", "SMV-map.csv")

if (!file.exists(sim_csv_file)) {
  stop("No existe 'SMV-map.csv'. Primero debes guardar la base territorial.")
}

smv_base <- read_csv(sim_csv_file, show_col_types = FALSE) %>%
  rename(zona_base = tipo_zona) %>%
  mutate(
    zona_base = case_when(
      zona_base %in% c("Urbano central", "urbano", "Urbano") ~ "Urbano",
      zona_base %in% c("Periurbano", "periurbano") ~ "Periurbano",
      zona_base %in% c("Rural", "rural") ~ "Rural",
      TRUE ~ zona_base
    )
  ) %>%
  filter(!Territorio %in% c("Colombia", "Baraya", "San Agustín"))

if (!all(c("Territorio", "zona_base") %in% names(smv_base))) {
  stop("El archivo SMV-map.csv debe contener las columnas 'Territorio' y 'tipo_zona'.")
}

# ---------------------------
# 5. Parámetros generales
# ---------------------------
anios <- 2016:2025
etnias <- c("Indígena", "No indígena")

# ---------------------------
# 6. Efecto aleatorio por barrio-año (ruido persistente + hotspot)
# ---------------------------
set.seed(6061)

efecto_barrio_anual <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio
) %>%
  left_join(
    smv_base %>% select(Territorio, zona_base),
    by = "Territorio"
  ) %>%
  group_by(Territorio) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(
    ruido_anual = rnorm(
      n(),
      mean = 0,
      sd = case_when(
        zona_base == "Urbano" ~ 0.08,
        zona_base == "Periurbano" ~ 0.14,
        zona_base == "Rural" ~ 0.18,
        TRUE ~ 0.10
      )
    ),
    ruido_persistente = as.numeric(stats::filter(
      ruido_anual,
      filter = 0.35,
      method = "recursive"
    ))
  ) %>%
  ungroup() %>%
  group_by(anio) %>%
  mutate(
    hotspot_temporal = rbinom(
      n(),
      1,
      prob = case_when(
        zona_base == "Urbano" ~ 0.08,
        zona_base == "Periurbano" ~ 0.18,
        zona_base == "Rural" ~ 0.22,
        TRUE ~ 0.10
      )
    ),
    efecto_hotspot = hotspot_temporal * runif(
      n(),
      min = 0.05,
      max = 0.16
    )
  ) %>%
  ungroup() %>%
  select(anio, Territorio, ruido_persistente, efecto_hotspot)

# ---------------------------
# 7. Base barrio-año-etnia
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio,
  etnia      = etnias
) %>%
  left_join(smv_base, by = "Territorio") %>%
  left_join(efecto_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    tendencia_anual = case_when(
      anio == 2016 ~ 0.03,
      anio == 2017 ~ 0.08,
      anio == 2018 ~ 0.14,
      anio == 2019 ~ 0.21,
      anio == 2020 ~ 0.23,
      anio == 2021 ~ 0.25,
      anio == 2022 ~ 0.32,
      anio == 2023 ~ 0.42,
      anio == 2024 ~ 0.50,
      anio == 2025 ~ 0.58,
      TRUE ~ NA_real_
    ),
    efecto_zona = case_when(
      zona_base == "Urbano" ~ 0.01,
      zona_base == "Periurbano" ~ 0.00,
      zona_base == "Rural" ~ -0.01,
      TRUE ~ 0
    ),
    efecto_etnia = case_when(
      etnia == "Indígena" ~ -0.04,
      TRUE ~ 0
    ),
    efecto_focalizacion = case_when(
      anio >= 2023 & Territorio == "Ribera Sur" ~ 0.015,
      anio >= 2023 & Territorio %in% c("El Progreso", "Nueva Esperanza") ~ 0.008,
      TRUE ~ 0
    ),
    valor_individual =
      tendencia_anual +
        efecto_zona +
        efecto_etnia +
        efecto_focalizacion +
        ruido_persistente +
        efecto_hotspot,
    valor_individual = pmin(pmax(valor_individual, 0.00), 0.90)
  )

# ---------------------------
# 8. Función de agregación
# ---------------------------
calcular_indicador <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      valor = mean(valor_individual, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------
# 9. Agregaciones municipio
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3       = "COL",
    Territorio = "San Martín del Valle",
    cod_local  = NA_character_,
    zona       = "Total",
    etnia      = "Total"
  )

municipio_zona <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3       = "COL",
    Territorio = "San Martín del Valle",
    cod_local  = NA_character_,
    etnia      = "Total"
  ) %>%
  rename(zona = zona_base)

municipio_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3       = "COL",
    Territorio = "San Martín del Valle",
    cod_local  = NA_character_,
    zona       = "Total"
  )

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, zona_base, etnia) %>%
  mutate(
    iso3       = "COL",
    Territorio = "San Martín del Valle",
    cod_local  = NA_character_
  ) %>%
  rename(zona = zona_base)

# ---------------------------
# 10. Agregaciones barrio
# ---------------------------
barrio_total <- base %>%
  calcular_indicador(anio, Territorio, zona_base) %>%
  mutate(
    iso3      = "COL",
    cod_local = NA_character_,
    etnia     = "Total"
  ) %>%
  rename(zona = zona_base)

barrio_etnia <- base %>%
  calcular_indicador(anio, Territorio, zona_base, etnia) %>%
  mutate(
    iso3      = "COL",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

# ---------------------------
# 11. Output final
# ---------------------------
transport_frequency_final <- bind_rows(
  municipio_total,
  municipio_zona,
  municipio_etnia,
  municipio_zona_etnia,
  barrio_total,
  barrio_etnia
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, Territorio, cod_local, anio, zona, etnia, valor) %>%
  arrange(Territorio, anio, zona, etnia)

# ---------------------------
# 12. Guardar archivos
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

# Global + barrio combined (used by filterEtniaStratifiedRows in the frontend)
write_csv(transport_frequency_final, file.path(csv_dir, "frecuencia-transporte.csv"))
write_parquet(transport_frequency_final, file.path(parquet_dir, "frecuencia-transporte.parquet"))

# Municipal/barrio subset for analytics scatter (excludes San Martín del Valle aggregate)
transport_municipal <- transport_frequency_final %>%
  filter(Territorio != "San Martín del Valle", etnia == "Total")

write_csv(transport_municipal, file.path(csv_dir, "frecuencia-transporte-municipal.csv"))
write_parquet(transport_municipal, file.path(parquet_dir, "frecuencia-transporte-municipal.parquet"))
