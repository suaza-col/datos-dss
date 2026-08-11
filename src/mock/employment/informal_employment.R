# =========================================================
# San Martín del Valle - Proporción de personas con
# empleo informal o sin protección social
# =========================================================
# Estratificador: sexo
# Formato: iso3, Territorio, cod_local, anio, sexo, zona, valor
# Totales: sexo = "Total", zona = "Total"
#
# Archivo único (global + barrio):
#   outputs/parquet/embarazadas-empleo-informal.parquet
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
sexos <- c("Mujeres", "Hombres")

barrios_criticos <- intersect(
  c("El Progreso", "Nueva Esperanza", "Ribera Sur"),
  smv_base$Territorio
)

# ---------------------------
# 3. Leer mortalidad materna (etnia Total)
# ---------------------------
mortalidad_csv <- file.path(output_dir, "csv", "mortalidad-materna.csv")

if (!file.exists(mortalidad_csv)) {
  stop("No existe 'mortalidad-materna.csv'. Primero ejecuta el script de mortalidad materna.")
}

mortalidad_barrio <- read_csv(mortalidad_csv, show_col_types = FALSE) %>%
  filter(
    Territorio != "San Martín del Valle",
    etnia == "Total"
  ) %>%
  select(anio, Territorio, rmm = valor) %>%
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
  select(anio, Territorio, z_mortalidad)

# ---------------------------
# 4. Base barrio-año-sexo
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio,
  sexo       = sexos
) %>%
  left_join(smv_base, by = "Territorio") %>%
  left_join(mortalidad_barrio, by = c("anio", "Territorio")) %>%
  mutate(
    z_mortalidad = ifelse(is.na(z_mortalidad), 0, z_mortalidad)
  )

# ---------------------------
# 5. Denominador personas laborales
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
    personas_laborales_total_anio = round(
      case_when(
        anio <= 2019 ~ 52000 + 200 * (anio - 2016),
        anio == 2020 ~ 51500,
        anio == 2021 ~ 52000,
        TRUE ~ 52500 + 180 * (anio - 2022)
      )
    ),
    prop_sexo = case_when(
      sexo == "Mujeres" ~ 0.51,
      sexo == "Hombres" ~ 0.49,
      TRUE ~ NA_real_
    ),
    peso_total = peso_barrio * prop_sexo,
    peso_total = peso_total / sum(peso_total, na.rm = TRUE),
    personas_laborales = round(personas_laborales_total_anio * peso_total)
  ) %>%
  ungroup()

# ---------------------------
# 6. Simular valor estructural con variabilidad barrio-año
# ---------------------------
set.seed(3031)

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
        zona_base == "periurbano" ~ 0.060,
        zona_base == "rural" ~ 0.075,
        TRUE ~ 0.04
      )
    ),
    ruido_persistente = as.numeric(stats::filter(
      ruido_anual,
      filter = 0.40,
      method = "recursive"
    ))
  ) %>%
  ungroup() %>%
  mutate(
    impulso_local = rbinom(
      n(),
      1,
      prob = case_when(
        zona_base == "urbano" ~ 0.05,
        zona_base == "periurbano" ~ 0.14,
        zona_base == "rural" ~ 0.18,
        TRUE ~ 0.08
      )
    ),
    efecto_impulso = impulso_local * runif(
      n(),
      min = 0.03,
      max = 0.10
    )
  ) %>%
  select(anio, Territorio, ruido_persistente, efecto_impulso)

efecto_barrio <- smv_base %>%
  mutate(
    variabilidad_barrio = rnorm(
      n(),
      mean = case_when(
        zona_base == "urbano" ~ -0.03,
        zona_base == "periurbano" ~ 0.04,
        zona_base == "rural" ~ 0.08,
        TRUE ~ 0
      ),
      sd = 0.07
    ),
    variabilidad_barrio = case_when(
      Territorio %in% barrios_criticos & zona_base == "rural" ~ variabilidad_barrio + 0.10,
      Territorio %in% barrios_criticos & zona_base == "periurbano" ~ variabilidad_barrio + 0.08,
      Territorio %in% barrios_criticos & zona_base == "urbano" ~ variabilidad_barrio + 0.04,
      TRUE ~ variabilidad_barrio
    )
  ) %>%
  select(Territorio, variabilidad_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "Territorio") %>%
  left_join(variabilidad_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    prob_base_zona = case_when(
      zona_base == "urbano" ~ 0.30,
      zona_base == "periurbano" ~ 0.55,
      zona_base == "rural" ~ 0.68,
      TRUE ~ 0.45
    ),
    efecto_sexo = case_when(
      sexo == "Mujeres" ~ 0.06,
      sexo == "Hombres" ~ 0.00,
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
      anio >= 2023 & zona_base == "urbano" ~ -0.020,
      anio >= 2023 & zona_base == "periurbano" ~ -0.012,
      anio >= 2023 & zona_base == "rural" ~ -0.005,
      TRUE ~ 0
    ),
    valor_individual =
      prob_base_zona +
        efecto_sexo +
        variabilidad_barrio +
        efecto_mortalidad +
        efecto_pandemia +
        recuperacion_laboral +
        ruido_persistente +
        efecto_impulso,
    valor_individual = pmin(pmax(valor_individual, 0.10), 0.90),
    personas_informales = rbinom(
      n(),
      size = personas_laborales,
      prob = valor_individual
    )
  )

# ---------------------------
# 7. Función de agregación
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
# 8. Outputs
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Total", zona = "Total"
  )

municipio_total_sexo <- base %>%
  calcular_indicador(anio, sexo) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona_total <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, sexo = "Total"
  ) %>%
  rename(zona = zona_base)

municipio_zona_sexo <- base %>%
  calcular_indicador(anio, zona_base, sexo) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

barrio_total <- base %>%
  calcular_indicador(anio, Territorio, zona_base) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_, sexo = "Total"
  ) %>%
  rename(zona = zona_base)

barrio_sexo <- base %>%
  calcular_indicador(anio, Territorio, zona_base, sexo) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

empleo_informal_final <- bind_rows(
  municipio_total,
  municipio_total_sexo,
  municipio_zona_total,
  municipio_zona_sexo,
  barrio_total,
  barrio_sexo
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, valor) %>%
  arrange(Territorio, anio, zona, sexo)

# ---------------------------
# 9. Guardar
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(empleo_informal_final, file.path(csv_dir, "embarazadas-empleo-informal.csv"))
write_parquet(empleo_informal_final, file.path(parquet_dir, "embarazadas-empleo-informal.parquet"))
