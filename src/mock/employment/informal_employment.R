# =========================================================
# San Martín del Valle - Proporción de personas con
# empleo informal o sin protección social
# =========================================================
# Estratificador: sexo
# Formato: iso3, NAME_2, cod_local, anio, sexo, zona, valor
# Totales: sexo = "Total", zona = "Total"
#
# Archivo único (global + barrio):
#   outputs/parquet/informal_employment.parquet
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

set.seed(3030)

# ---------------------------
# 1. Leer base territorial
# ---------------------------
sim_csv_file <- file.path(output_dir, "csv", "SMV_map.csv")

if (!file.exists(sim_csv_file)) {
  stop("No existe 'SMV_map.csv'. Primero ejecuta SMV_map.R.")
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
  stop("SMV_map.csv debe contener las columnas 'NAME_2' y 'tipo_zona'.")
}

# ---------------------------
# 2. Parámetros generales
# ---------------------------
anios <- 2016:2025
sexos <- c("Mujeres", "Hombres")

barrios_criticos <- intersect(
  c("El Progreso", "Nueva Esperanza", "Ribera Sur"),
  smv_base$NAME_2
)

# ---------------------------
# 3. Leer mortalidad materna (etnia Total)
# ---------------------------
mortalidad_csv <- file.path(output_dir, "csv", "maternal_mortality_rate.csv")

if (!file.exists(mortalidad_csv)) {
  stop("No existe 'maternal_mortality_rate.csv'. Primero ejecuta el script de mortalidad materna.")
}

mortalidad_barrio <- read_csv(mortalidad_csv, show_col_types = FALSE) %>%
  filter(
    NAME_2 != "San Martín del Valle",
    etnia == "Total"
  ) %>%
  select(anio, NAME_2, rmm = valor) %>%
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
  select(anio, NAME_2, z_mortalidad)

# ---------------------------
# 4. Base barrio-año-sexo
# ---------------------------
base <- expand_grid(
  anio   = anios,
  NAME_2 = smv_base$NAME_2,
  sexo   = sexos
) %>%
  left_join(smv_base, by = "NAME_2") %>%
  left_join(mortalidad_barrio, by = c("anio", "NAME_2")) %>%
  mutate(
    z_mortalidad = ifelse(is.na(z_mortalidad), 0, z_mortalidad),

    prop_sexo = case_when(
      sexo == "Mujeres" ~ 0.51,
      sexo == "Hombres" ~ 0.49,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 5. Denominador personas laborales
# ---------------------------
peso_barrio <- smv_base %>%
  mutate(
    peso_zona = case_when(
      tipo_zona == "urbano"     ~ 2.00,
      tipo_zona == "periurbano" ~ 1.30,
      tipo_zona == "rural"      ~ 0.60,
      TRUE ~ 1
    ),
    peso_barrio = runif(n(), 0.80, 1.20) * peso_zona
  ) %>%
  select(NAME_2, peso_barrio)

base <- base %>%
  left_join(peso_barrio, by = "NAME_2") %>%
  group_by(anio) %>%
  mutate(
    personas_laborales_total_anio = round(
      case_when(
        anio == 2020 ~ 50000,
        anio == 2021 ~ 51000,
        TRUE ~ 52000 + 400 * (anio - 2016)
      )
    ),
    peso_total = peso_barrio * prop_sexo,
    peso_total = peso_total / sum(peso_total, na.rm = TRUE),
    personas_laborales = round(personas_laborales_total_anio * peso_total)
  ) %>%
  ungroup()

# ---------------------------
# 6. Efecto barrio
# ---------------------------
set.seed(3031)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano"     ~ -0.03,
        tipo_zona == "periurbano" ~  0.04,
        tipo_zona == "rural"      ~  0.07,
        TRUE ~ 0
      ),
      sd = 0.09
    ),
    variabilidad_barrio = case_when(
      NAME_2 %in% barrios_criticos & tipo_zona == "rural"      ~ variabilidad_barrio + 0.08,
      NAME_2 %in% barrios_criticos & tipo_zona == "periurbano" ~ variabilidad_barrio + 0.05,
      TRUE ~ variabilidad_barrio
    )
  ) %>%
  select(NAME_2, variabilidad_barrio)

# ---------------------------
# 7. Simular proporción informal
# ---------------------------
base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    prob_base_zona = case_when(
      tipo_zona == "urbano"     ~ 0.30,
      tipo_zona == "periurbano" ~ 0.55,
      tipo_zona == "rural"      ~ 0.68,
      TRUE ~ 0.45
    ),

    efecto_sexo = case_when(
      sexo == "Mujeres" ~ 0.06,
      TRUE ~ 0
    ),

    efecto_mortalidad = 0.025 * z_mortalidad,

    efecto_pandemia = case_when(
      anio == 2020 ~ 0.060,
      anio == 2021 ~ 0.045,
      anio == 2022 ~ 0.020,
      TRUE ~ 0
    ),

    recuperacion_laboral = case_when(
      anio >= 2023 & tipo_zona == "urbano"     ~ -0.020,
      anio >= 2023 & tipo_zona == "periurbano" ~ -0.012,
      anio >= 2023 & tipo_zona == "rural"      ~ -0.005,
      TRUE ~ 0
    ),

    valor_individual =
      prob_base_zona +
      efecto_sexo +
      variabilidad_barrio +
      efecto_mortalidad +
      efecto_pandemia +
      recuperacion_laboral,

    valor_individual = pmin(pmax(valor_individual, 0.05), 0.95),

    personas_informal = rbinom(
      n(),
      size = personas_laborales,
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
      valor = weighted.mean(valor_individual, w = personas_laborales, na.rm = TRUE),
      .groups = "drop"
    )
}

# ---------------------------
# 9. Outputs
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Total", zona = "Total"
  )

municipio_sexo <- base %>%
  calcular_indicador(anio, sexo) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona <- base %>%
  calcular_indicador(anio, tipo_zona) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Total"
  ) %>%
  rename(zona = tipo_zona)

municipio_zona_sexo <- base %>%
  calcular_indicador(anio, tipo_zona, sexo) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

barrio_total <- base %>%
  calcular_indicador(anio, NAME_2, tipo_zona) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_, sexo = "Total"
  ) %>%
  rename(zona = tipo_zona)

barrio_sexo <- base %>%
  calcular_indicador(anio, NAME_2, tipo_zona, sexo) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

informal_employment_final <- bind_rows(
  municipio_total,
  municipio_sexo,
  municipio_zona,
  municipio_zona_sexo,
  barrio_total,
  barrio_sexo
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, zona, valor) %>%
  arrange(NAME_2, anio, zona, sexo)

# ---------------------------
# 10. Guardar
# ---------------------------
csv_dir     <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(    informal_employment_final, file.path(csv_dir,     "informal_employment.csv"))
write_parquet(informal_employment_final, file.path(parquet_dir, "informal_employment.parquet"))

message("✅ informal_employment.parquet guardado")
