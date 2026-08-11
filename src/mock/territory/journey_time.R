# =========================================================
# San Martín del Valle - Proporción de embarazadas que
# viven a más de 1 hora del centro de salud más cercano
# =========================================================
# Estratificador: etnia
# Formato: iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor
# sexo siempre = "Mujeres" (indicador referido a embarazadas)
#
# Archivo único (global + barrio):
#   outputs/parquet/journey_time.parquet
# =========================================================

library(here)

output_dir <- here("outputs")

if (!dir.exists(output_dir)) {
  stop("La carpeta de salida no existe. Revisa la ruta.")
}

library(dplyr)
library(tidyr)
library(readr)
library(arrow)

set.seed(2026)

# ---------------------------
# 1. Leer base territorial
# ---------------------------
sim_csv_file <- file.path(output_dir, "csv", "SMV-map.csv")

if (!file.exists(sim_csv_file)) {
  stop("No existe 'SMV-map.csv'. Primero ejecuta SMV_map.R.")
}

smv_base <- read_csv(sim_csv_file, show_col_types = FALSE) %>%
  rename(zona_base = tipo_zona) %>%
  mutate(
    zona_base = case_when(
      zona_base == "Urbano central" ~ "urbano",
      zona_base == "Periurbano" ~ "periurbano",
      zona_base == "Rural" ~ "rural",
      TRUE ~ tolower(zona_base)
    )
  ) %>%
  filter(!Territorio %in% c("Colombia", "Baraya", "San Agustín"))

if (!all(c("Territorio", "zona_base") %in% names(smv_base))) {
  stop("SMV-map.csv debe contener las columnas 'Territorio' y 'tipo_zona'.")
}

# ---------------------------
# 2. Parámetros generales
# ---------------------------
anios <- 2016:2025
etnias <- c("Indígena", "No indígena")

barrios_criticos <- intersect(
  c("El Progreso", "Nueva Esperanza", "Ribera Sur"),
  smv_base$Territorio
)

# ---------------------------
# 3. Leer mortalidad materna
# ---------------------------
mortalidad_csv <- file.path(output_dir, "csv", "mortalidad-materna.csv")

if (!file.exists(mortalidad_csv)) {
  stop("No existe 'mortalidad-materna.csv'. Primero ejecuta el script de mortalidad materna.")
}

mortalidad_barrio <- read_csv(mortalidad_csv, show_col_types = FALSE) %>%
  filter(
    Territorio != "San Martín del Valle",
    etnia != "Total"
  ) %>%
  select(anio, Territorio, etnia, rmm = valor) %>%
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
  select(anio, Territorio, etnia, z_mortalidad)

# ---------------------------
# 4. Base barrio-año-etnia
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio,
  etnia      = etnias
) %>%
  left_join(smv_base, by = "Territorio") %>%
  left_join(mortalidad_barrio, by = c("anio", "Territorio", "etnia")) %>%
  mutate(
    z_mortalidad = ifelse(is.na(z_mortalidad), 0, z_mortalidad),
    prop_etnia = case_when(
      zona_base == "rural" & etnia == "Indígena" ~ 0.45,
      zona_base == "rural" & etnia == "No indígena" ~ 0.55,
      zona_base == "periurbano" & etnia == "Indígena" ~ 0.30,
      zona_base == "periurbano" & etnia == "No indígena" ~ 0.70,
      zona_base == "urbano" & etnia == "Indígena" ~ 0.15,
      zona_base == "urbano" & etnia == "No indígena" ~ 0.85,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 5. Denominador embarazadas
# ---------------------------
peso_barrio <- smv_base %>%
  mutate(
    peso_zona = case_when(
      zona_base == "urbano" ~ 1.80,
      zona_base == "periurbano" ~ 1.20,
      zona_base == "rural" ~ 0.55,
      TRUE ~ 1
    ),
    peso_barrio = runif(n(), 0.75, 1.25) * peso_zona
  ) %>%
  select(Territorio, peso_barrio)

base <- base %>%
  left_join(peso_barrio, by = "Territorio") %>%
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
# 6. Simular valor estructural con variabilidad barrio-año
# ---------------------------
set.seed(2027)

variabilidad_barrio_anual <- expand_grid(
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
        zona_base == "urbano" ~ 0.025,
        zona_base == "periurbano" ~ 0.070,
        zona_base == "rural" ~ 0.090,
        TRUE ~ 0.05
      )
    ),
    ruido_persistente = as.numeric(stats::filter(
      ruido_anual,
      filter = 0.45,
      method = "recursive"
    ))
  ) %>%
  ungroup() %>%
  mutate(
    impulso_local = rbinom(
      n(),
      1,
      prob = case_when(
        zona_base == "urbano" ~ 0.06,
        zona_base == "periurbano" ~ 0.18,
        zona_base == "rural" ~ 0.24,
        TRUE ~ 0.10
      )
    ),
    efecto_impulso = impulso_local * runif(
      n(),
      min = 0.04,
      max = 0.14
    )
  ) %>%
  select(anio, Territorio, ruido_persistente, efecto_impulso)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        zona_base == "urbano" ~ -0.02,
        zona_base == "periurbano" ~ 0.03,
        zona_base == "rural" ~ 0.08,
        TRUE ~ 0
      ),
      sd = 0.06
    ),
    variabilidad_barrio = case_when(
      Territorio %in% barrios_criticos & zona_base == "rural" ~ variabilidad_barrio + 0.14,
      Territorio %in% barrios_criticos & zona_base == "periurbano" ~ variabilidad_barrio + 0.10,
      Territorio %in% barrios_criticos & zona_base == "urbano" ~ variabilidad_barrio + 0.05,
      TRUE ~ variabilidad_barrio
    )
  ) %>%
  select(Territorio, variabilidad_barrio)

# ---------------------------
# 7. Simular proporción > 1h
# ---------------------------
base <- base %>%
  left_join(efecto_barrio, by = "Territorio") %>%
  left_join(variabilidad_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    prob_base_zona = case_when(
      zona_base == "urbano" ~ 0.08,
      zona_base == "periurbano" ~ 0.32,
      zona_base == "rural" ~ 0.60,
      TRUE ~ 0.25
    ),
    efecto_etnia = case_when(
      etnia == "Indígena" ~ 0.06,
      TRUE ~ 0
    ),
    efecto_mortalidad = 0.040 * z_mortalidad,
    efecto_pandemia = case_when(
      anio == 2020 ~ 0.045,
      anio == 2021 ~ 0.030,
      anio == 2022 ~ 0.015,
      TRUE ~ 0
    ),
    mejora_postpandemia = case_when(
      anio >= 2023 & zona_base == "urbano" ~ -0.010,
      anio >= 2023 & zona_base == "periurbano" ~ -0.025,
      anio >= 2023 & zona_base == "rural" ~ -0.035,
      TRUE ~ 0
    ),
    valor_individual =
      prob_base_zona +
        efecto_etnia +
        variabilidad_barrio +
        efecto_mortalidad +
        efecto_pandemia +
        mejora_postpandemia +
        ruido_persistente +
        efecto_impulso,
    valor_individual = pmin(pmax(valor_individual, 0.02), 0.90),
    embarazadas_1h = rbinom(
      n(),
      size = embarazadas,
      prob = valor_individual
    )
  )

# ---------------------------
# 8. Función de agregación
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
# 9. Outputs
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Mujeres",
    zona = "Total", etnia = "Total"
  )

municipio_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Mujeres", zona = "Total"
  )

municipio_zona <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Mujeres", etnia = "Total"
  ) %>%
  rename(zona = zona_base)

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, zona_base, etnia) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Mujeres"
  ) %>%
  rename(zona = zona_base)

barrio_total <- base %>%
  calcular_indicador(anio, Territorio, zona_base) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_,
    sexo = "Mujeres", etnia = "Total"
  ) %>%
  rename(zona = zona_base)

barrio_etnia <- base %>%
  calcular_indicador(anio, Territorio, zona_base, etnia) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_, sexo = "Mujeres"
  ) %>%
  rename(zona = zona_base)

journey_time_final <- bind_rows(
  municipio_total,
  municipio_etnia,
  municipio_zona,
  municipio_zona_etnia,
  barrio_total,
  barrio_etnia
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor) %>%
  arrange(Territorio, anio, zona, etnia)

# ---------------------------
# 10. Guardar
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(journey_time_final, file.path(csv_dir, "traslado.csv"))
write_parquet(journey_time_final, file.path(parquet_dir, "traslado.parquet"))
