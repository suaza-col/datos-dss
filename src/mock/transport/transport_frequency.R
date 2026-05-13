# =========================================================
# San Martín del Valle - Frecuencia de transporte público subsidiado
# Cobertura hacia centros de salud desde barrios periféricos
# Genera datos globales (municipio) Y datos de barrio en un solo script
# Estratificadores: zona, etnia
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
set.seed(4321)

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

# Barrios con mejoras focalizadas en rutas de transporte
barrios_focalizados <- c("El Mirador", "Los Pinos", "Nueva Esperanza")
barrios_focalizados <- intersect(barrios_focalizados, smv_base$NAME_2)

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
# 8. Efecto aleatorio por barrio
# ---------------------------
set.seed(4322)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(n(), mean = 0, sd = 0.10)
  ) %>%
  select(NAME_2, variabilidad_barrio)

# ---------------------------
# 9. Simulación cobertura de transporte
# Tendencia al alza moderada + variabilidad barrial
# Mujeres indígenas tienen leve menor acceso (barreras de accesibilidad)
# ---------------------------
base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    tendencia_anual = case_when(
      anio == 2016 ~ 0.25,
      anio == 2017 ~ 0.28,
      anio == 2018 ~ 0.31,
      anio == 2019 ~ 0.34,
      anio == 2020 ~ 0.30,
      anio == 2021 ~ 0.33,
      anio == 2022 ~ 0.37,
      anio == 2023 ~ 0.42,
      anio == 2024 ~ 0.46,
      anio == 2025 ~ 0.50,
      TRUE ~ NA_real_
    ),

    efecto_zona = case_when(
      tipo_zona == "urbano"     ~  0.15,
      tipo_zona == "periurbano" ~  0.00,
      tipo_zona == "rural"      ~ -0.12,
      TRUE ~ 0
    ),

    efecto_etnia = case_when(
      etnia == "Indígena" ~ -0.05,
      TRUE ~ 0
    ),

    efecto_focalizacion = case_when(
      anio >= 2022 & NAME_2 %in% barrios_focalizados ~ 0.05,
      TRUE ~ 0
    ),

    valor_individual =
      tendencia_anual +
      efecto_zona +
      efecto_etnia +
      efecto_focalizacion +
      variabilidad_barrio,

    valor_individual = pmin(pmax(valor_individual, 0.00), 0.95)
  )

# ---------------------------
# 10. Función de agregación
# ---------------------------
calcular_indicador <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      valor = weighted.mean(valor_individual, w = embarazadas, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------
# 11. Datos globales (municipio)
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
# 12. Datos de barrio (municipal)
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
# 13. Guardar archivos
# ---------------------------
csv_dir     <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir))     dir.create(csv_dir,     recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(    global_data,   file.path(csv_dir,     "transport_frequency.csv"))
write_parquet(global_data,   file.path(parquet_dir, "transport_frequency.parquet"))

write_csv(    municipal_data, file.path(csv_dir,     "transport_frequency_municipal.csv"))
write_parquet(municipal_data, file.path(parquet_dir, "transport_frequency_municipal.parquet"))
