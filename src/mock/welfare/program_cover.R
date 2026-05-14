# =========================================================
# San Martín del Valle - Cobertura de programas municipales
# de apoyo social a mujeres embarazadas en barrios periféricos
# =========================================================
# 2016-2018: programa inexistente — NA (sin línea en el gráfico)
# 2019: inicio del programa
# 2020-2021: efecto pandemia
# 2023+: focalización en barrios críticos
#
# Estratificador: zona
# Formato: iso3, NAME_2, cod_local, anio, zona, valor
# Totales: zona = "Total"
#
# Archivo:
#   outputs/parquet/program_cover.parquet
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

set.seed(5050)

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
      tipo_zona == "Periurbano" ~ "periurbano",
      tipo_zona == "Rural" ~ "rural",
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

barrios_criticos <- intersect(
  c("El Progreso", "Nueva Esperanza", "Ribera Sur"),
  smv_base$NAME_2
)

# ---------------------------
# 3. Base barrio-año
# ---------------------------
base <- expand_grid(
  anio   = anios,
  NAME_2 = smv_base$NAME_2
) %>%
  left_join(smv_base, by = "NAME_2")

# ---------------------------
# 4. Denominador embarazadas
# ---------------------------
peso_barrio <- smv_base %>%
  mutate(
    peso_zona = case_when(
      tipo_zona == "urbano" ~ 1.80,
      tipo_zona == "periurbano" ~ 1.20,
      tipo_zona == "rural" ~ 0.55,
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
    peso_total = peso_barrio / sum(peso_barrio, na.rm = TRUE),
    embarazadas = round(embarazadas_total_anio * peso_total)
  ) %>%
  ungroup()

# ---------------------------
# 5. Efecto barrio
# ---------------------------
set.seed(5051)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano" ~ 0.03,
        tipo_zona == "periurbano" ~ -0.02,
        tipo_zona == "rural" ~ -0.04,
        TRUE ~ 0
      ),
      sd = 0.04
    )
  ) %>%
  select(NAME_2, variabilidad_barrio)

# ---------------------------
# 6. Simular cobertura
# ---------------------------
base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    valor_individual = case_when(

      # 2016-2018: programa inexistente
      anio <= 2018 ~ NA_real_,

      # 2019: inicio del programa
      anio == 2019 ~ case_when(
        tipo_zona == "urbano" ~ 0.30,
        tipo_zona == "periurbano" ~ 0.18,
        tipo_zona == "rural" ~ 0.10,
        TRUE ~ 0.15
      ) + variabilidad_barrio,

      # 2020: pandemia
      anio == 2020 ~ case_when(
        tipo_zona == "urbano" ~ 0.24,
        tipo_zona == "periurbano" ~ 0.14,
        tipo_zona == "rural" ~ 0.08,
        TRUE ~ 0.12
      ) + variabilidad_barrio,

      # 2021: pandemia
      anio == 2021 ~ case_when(
        tipo_zona == "urbano" ~ 0.26,
        tipo_zona == "periurbano" ~ 0.16,
        tipo_zona == "rural" ~ 0.10,
        TRUE ~ 0.14
      ) + variabilidad_barrio,

      # 2022: recuperación parcial
      anio == 2022 ~ case_when(
        tipo_zona == "urbano" ~ 0.34,
        tipo_zona == "periurbano" ~ 0.24,
        tipo_zona == "rural" ~ 0.16,
        TRUE ~ 0.20
      ) + variabilidad_barrio,

      # 2023+: focalización territorial en barrios críticos
      anio >= 2023 ~ case_when(
        NAME_2 %in% barrios_criticos & tipo_zona == "periurbano" ~ 0.52,
        NAME_2 %in% barrios_criticos & tipo_zona == "rural" ~ 0.46,
        tipo_zona == "urbano" ~ 0.40,
        tipo_zona == "periurbano" ~ 0.30,
        tipo_zona == "rural" ~ 0.22,
        TRUE ~ 0.28
      ) + variabilidad_barrio
    ),
    valor_individual = ifelse(
      is.na(valor_individual),
      NA_real_,
      pmin(pmax(valor_individual, 0.01), 0.90)
    ),
    embarazadas_programa = ifelse(
      is.na(valor_individual),
      NA_integer_,
      rbinom(n(), size = embarazadas, prob = valor_individual)
    )
  )

# ---------------------------
# 7. Función de agregación
# ---------------------------
calcular_indicador <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      valor = weighted.mean(valor_individual, w = embarazadas, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(valor = ifelse(is.nan(valor) | is.na(valor), NA_real_, valor))
}

# ---------------------------
# 8. Outputs
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona <- base %>%
  calcular_indicador(anio, tipo_zona) %>%
  mutate(
    iso3 = "COL", NAME_2 = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

barrio_total <- base %>%
  calcular_indicador(anio, NAME_2, tipo_zona) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_
  ) %>%
  rename(zona = tipo_zona)

program_cover_final <- bind_rows(
  municipio_total,
  municipio_zona,
  barrio_total
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, NAME_2, cod_local, anio, zona, valor) %>%
  arrange(NAME_2, anio, zona)

# ---------------------------
# 9. Guardar
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(program_cover_final, file.path(csv_dir, "program_cover.csv"))
write_parquet(program_cover_final, file.path(parquet_dir, "program_cover.parquet"))

message("✅ program_cover.parquet guardado")
