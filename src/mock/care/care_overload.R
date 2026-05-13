# =========================================================
# San Martín del Valle - Sobrecarga de cuidados
# Mujeres embarazadas por barrio y etnia
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
set.seed(4040)

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
anios <- 2016:2025
etnias <- c("Indígena", "No indígena")

barrios_criticos <- c(
  "El Progreso",
  "Nueva Esperanza",
  "Ribera Sur"
)
barrios_criticos <- intersect(barrios_criticos, smv_base$NAME_2)

# ---------------------------
# 6. Leer mortalidad materna
# ---------------------------
mortalidad_csv <- file.path(output_dir, "csv", "maternal_mortality_rate.csv")

if (!file.exists(mortalidad_csv)) {
  stop("No existe 'maternal_mortality_rate.csv'. Primero ejecuta el script de mortalidad materna.")
}

mortalidad_barrio <- read_csv(mortalidad_csv, show_col_types = FALSE) %>%
  filter(
    NAME_2 != "San Martín del Valle",
    etnia != "Total"
  ) %>%
  select(anio, NAME_2, etnia, rmm = valor) %>%
  group_by(anio) %>%
  mutate(
    z_mortalidad = ifelse(
      sd(rmm, na.rm = TRUE) > 0,
      (rmm - mean(rmm, na.rm = TRUE)) / sd(rmm, na.rm = TRUE),
      0
    ),
    z_mortalidad = pmin(pmax(z_mortalidad, -2), 2)
  ) %>%
  ungroup() %>%
  select(anio, NAME_2, etnia, z_mortalidad)

# ---------------------------
# 7. Base barrio-año-etnia
# ---------------------------
base <- expand_grid(
  anio = anios,
  NAME_2 = smv_base$NAME_2,
  etnia = etnias
) %>%
  left_join(smv_base, by = "NAME_2") %>%
  left_join(mortalidad_barrio, by = c("anio", "NAME_2", "etnia")) %>%
  mutate(
    z_mortalidad = ifelse(is.na(z_mortalidad), 0, z_mortalidad),

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
# 8. Denominador embarazadas
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
# 9. Simular sobrecarga
# ---------------------------
set.seed(4041)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano"     ~ -0.02,
        tipo_zona == "periurbano" ~  0.03,
        tipo_zona == "rural"      ~  0.07,
        TRUE ~ 0
      ),
      sd = 0.08
    ),
    variabilidad_barrio = case_when(
      NAME_2 %in% barrios_criticos & tipo_zona == "rural"      ~ variabilidad_barrio + 0.10,
      NAME_2 %in% barrios_criticos & tipo_zona == "periurbano" ~ variabilidad_barrio + 0.07,
      TRUE ~ variabilidad_barrio
    )
  ) %>%
  select(NAME_2, variabilidad_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    prob_base_zona = case_when(
      tipo_zona == "urbano"     ~ 0.20,
      tipo_zona == "periurbano" ~ 0.38,
      tipo_zona == "rural"      ~ 0.50,
      TRUE ~ 0.35
    ),

    efecto_etnia = case_when(
      etnia == "Indígena" ~ 0.08,
      TRUE ~ 0
    ),

    efecto_mortalidad = 0.018 * z_mortalidad,

    efecto_pandemia = case_when(
      anio == 2020 ~ 0.030,
      anio == 2021 ~ 0.020,
      TRUE ~ 0
    ),

    mejora_cuidados = case_when(
      anio >= 2023 & tipo_zona == "urbano"     ~ -0.015,
      anio >= 2023 & tipo_zona == "periurbano" ~ -0.010,
      TRUE ~ 0
    ),

    valor_individual =
      prob_base_zona +
      efecto_etnia +
      variabilidad_barrio +
      efecto_mortalidad +
      efecto_pandemia +
      mejora_cuidados,

    valor_individual = pmin(pmax(valor_individual, 0.05), 0.90),

    embarazadas_sobrecarga = rbinom(
      n(),
      size = embarazadas,
      prob = valor_individual
    )
  )

# ---------------------------
# 10. Función agregación
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
# 11. Outputs finales
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    zona = "Total",
    etnia = "Total"
  )

municipio_total_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    zona = "Total"
  )

municipio_zona_total <- base %>%
  calcular_indicador(anio, tipo_zona) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    etnia = "Total"
  ) %>%
  rename(zona = tipo_zona)

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, tipo_zona, etnia) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

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

sobrecarga_cuidados_final <- bind_rows(
  municipio_total,
  municipio_total_etnia,
  municipio_zona_total,
  municipio_zona_etnia,
  barrio_total,
  barrio_etnia
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, NAME_2, cod_local, anio, zona, etnia, valor) %>%
  arrange(NAME_2, anio, zona, etnia)

# ---------------------------
# 12. Guardar archivos
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

archivo_csv <- file.path(csv_dir, "care_overload_municipal.csv")
write_csv(sobrecarga_cuidados_final, archivo_csv)

archivo_parquet <- file.path(parquet_dir, "care_overload_municipal.parquet")
write_parquet(sobrecarga_cuidados_final, archivo_parquet)
