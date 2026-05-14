# =========================================================
# Cobertura del programa municipal de apoyo al cuidado infantil
# "Cuidar en Comunidad" en mujeres embarazadas que residen en
# barrios periféricos del Municipio de San Martín del Valle
# =========================================================
# Estratificadores: zona, etnia
# Archivos:
#   outputs/parquet/infant_care_support.parquet
#   outputs/parquet/infant_care_support_municipal.parquet
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
set.seed(7070)

# ---------------------------
# 4. Leer base territorial
# ---------------------------
sim_csv_file <- file.path(output_dir, "csv", "SMV_map.csv")

if (!file.exists(sim_csv_file)) {
  stop("No existe 'SMV_map.csv'. Primero debes guardar la base territorial.")
}

smv_base <- read_csv(sim_csv_file, show_col_types = FALSE) %>%
  mutate(
    tipo_zona = case_when(
      tipo_zona == "Urbano central" ~ "urbano",
      tipo_zona == "Periurbano"     ~ "periurbano",
      tipo_zona == "Rural"          ~ "rural",
      TRUE ~ tolower(tipo_zona)
    )
  )

if (!all(c("NAME_2", "tipo_zona") %in% names(smv_base))) {
  stop("El archivo SMV_map.csv debe contener las columnas 'NAME_2' y 'tipo_zona'.")
}

# ---------------------------
# 5. Parámetros generales
# ---------------------------
anios  <- 2016:2025
etnias <- c("Indígena", "No indígena")

barrios_criticos <- c("El Progreso", "Nueva Esperanza", "Ribera Sur")
barrios_criticos <- intersect(barrios_criticos, smv_base$NAME_2)

# ---------------------------
# 6. Base barrio-año-etnia
# ---------------------------
base <- expand_grid(
  anio   = anios,
  NAME_2 = smv_base$NAME_2,
  etnia  = etnias
) %>%
  left_join(smv_base, by = "NAME_2") %>%
  mutate(
    prop_etnia = case_when(
      tipo_zona == "rural"      & etnia == "Indígena"    ~ 0.45,
      tipo_zona == "rural"      & etnia == "No indígena" ~ 0.55,
      tipo_zona == "periurbano" & etnia == "Indígena"    ~ 0.30,
      tipo_zona == "periurbano" & etnia == "No indígena" ~ 0.70,
      tipo_zona == "urbano"     & etnia == "Indígena"    ~ 0.15,
      tipo_zona == "urbano"     & etnia == "No indígena" ~ 0.85,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 7. Denominador embarazadas
# ---------------------------
peso_barrio <- smv_base %>%
  mutate(
    peso_zona = case_when(
      tipo_zona == "urbano"     ~ 1.80,
      tipo_zona == "periurbano" ~ 1.20,
      tipo_zona == "rural"      ~ 0.55,
      TRUE ~ 1
    ),
    peso_barrio = runif(n(), 0.75, 1.25) * peso_zona
  ) %>%
  select(NAME_2, peso_barrio)

base <- base %>%
  left_join(peso_barrio, by = "NAME_2") %>%
  group_by(anio) %>%
  mutate(
    embarazadas_total_anio = round(
      case_when(
        anio == 2020 ~ 1380,
        anio == 2021 ~ 1420,
        TRUE ~ 1500 - 8 * (anio - 2016)
      )
    ),
    peso_total = peso_barrio * prop_etnia,
    peso_total = peso_total / sum(peso_total, na.rm = TRUE),
    embarazadas = round(embarazadas_total_anio * peso_total)
  ) %>%
  ungroup()

# ---------------------------
# 8. Simular cobertura Cuidar en Comunidad
# ---------------------------
set.seed(7071)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano"     ~  0.03,
        tipo_zona == "periurbano" ~ -0.01,
        tipo_zona == "rural"      ~ -0.04,
        TRUE ~ 0
      ),
      sd = 0.05
    )
  ) %>%
  select(NAME_2, variabilidad_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    valor_individual = case_when(

      # Programa inexistente
      anio <= 2019 ~ NA_real_,

      # Piloto pequeño durante 2020
      anio == 2020 ~ case_when(
        tipo_zona == "urbano"     ~ 0.08,
        tipo_zona == "periurbano" ~ 0.05,
        tipo_zona == "rural"      ~ 0.03,
        TRUE ~ 0.04
      ),

      # Expansión lenta
      anio == 2021 ~ case_when(
        tipo_zona == "urbano"     ~ 0.14,
        tipo_zona == "periurbano" ~ 0.09,
        tipo_zona == "rural"      ~ 0.05,
        TRUE ~ 0.08
      ),

      # Consolidación parcial
      anio == 2022 ~ case_when(
        tipo_zona == "urbano"     ~ 0.24,
        tipo_zona == "periurbano" ~ 0.16,
        tipo_zona == "rural"      ~ 0.09,
        TRUE ~ 0.14
      ),

      # Focalización desde 2023
      anio >= 2023 ~ case_when(
        NAME_2 %in% barrios_criticos & tipo_zona == "periurbano" ~ 0.62,
        NAME_2 %in% barrios_criticos & tipo_zona == "rural"      ~ 0.55,
        NAME_2 %in% barrios_criticos & tipo_zona == "urbano"     ~ 0.66,
        tipo_zona == "urbano"     ~ 0.48,
        tipo_zona == "periurbano" ~ 0.38,
        tipo_zona == "rural"      ~ 0.25,
        TRUE ~ 0.30
      )
    ),

    # Brecha por etnia; menor cobertura indígena al inicio,
    # con mejora progresiva desde 2023 por enfoque intercultural
    efecto_etnia = case_when(
      is.na(valor_individual)                                             ~ NA_real_,
      etnia == "Indígena" & anio <= 2022                                 ~ -0.06,
      etnia == "Indígena" & anio >= 2023 & NAME_2 %in% barrios_criticos ~  0.05,
      etnia == "Indígena" & anio >= 2023                                 ~ -0.01,
      TRUE ~ 0
    ),

    efecto_tendencia_post2023 = case_when(
      anio == 2024 ~ 0.05,
      anio == 2025 ~ 0.10,
      TRUE ~ 0
    ),

    valor_individual = valor_individual +
      efecto_etnia +
      efecto_tendencia_post2023 +
      variabilidad_barrio,

    valor_individual = ifelse(
      is.na(valor_individual),
      NA_real_,
      pmin(pmax(valor_individual, 0.00), 0.90)
    )
  )

# ---------------------------
# 9. Función de agregación
# ---------------------------
calcular_indicador <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      valor = weighted.mean(valor_individual, w = embarazadas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(valor = ifelse(is.nan(valor), NA_real_, valor))
}

# ---------------------------
# 10. Datos globales (municipio)
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total", etnia = "Total"
  )

municipio_total_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona_total <- base %>%
  calcular_indicador(anio, tipo_zona) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, etnia = "Total"
  ) %>%
  rename(zona = tipo_zona)

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, tipo_zona, etnia) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

global_data <- bind_rows(
  municipio_total,
  municipio_total_etnia,
  municipio_zona_total,
  municipio_zona_etnia
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, NAME_2, cod_local, anio, zona, etnia, valor) %>%
  arrange(NAME_2, anio, zona, etnia)

# ---------------------------
# 11. Datos de barrio (municipal)
# ---------------------------
barrio_total <- base %>%
  calcular_indicador(anio, NAME_2, tipo_zona) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_,
    etnia = "Total"
  ) %>%
  rename(zona = tipo_zona)

barrio_etnia <- base %>%
  calcular_indicador(anio, NAME_2, tipo_zona, etnia) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

municipal_data <- bind_rows(barrio_total, barrio_etnia) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, NAME_2, cod_local, anio, zona, etnia, valor) %>%
  arrange(NAME_2, anio, zona, etnia)

# ---------------------------
# 12. Guardar archivos
# ---------------------------
csv_dir     <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir))     dir.create(csv_dir,     recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(global_data,     file.path(csv_dir,     "infant_care_support.csv"))
write_parquet(global_data, file.path(parquet_dir, "infant_care_support.parquet"))

write_csv(municipal_data,     file.path(csv_dir,     "infant_care_support_municipal.csv"))
write_parquet(municipal_data, file.path(parquet_dir, "infant_care_support_municipal.parquet"))
