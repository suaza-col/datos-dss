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
# Formato: iso3, Territorio, cod_local, anio, zona, valor
# Totales: zona = "Total"
#
# Archivo:
#   outputs/parquet/apoyo-embarazadas.parquet
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

barrios_criticos <- intersect(
  c("El Progreso", "Nueva Esperanza", "Ribera Sur"),
  smv_base$Territorio
)

# ---------------------------
# 3. Base barrio-año
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio
) %>%
  left_join(smv_base, by = "Territorio")

# ---------------------------
# 4. Denominador embarazadas
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
    peso_total = peso_barrio / sum(peso_barrio, na.rm = TRUE),
    embarazadas = round(embarazadas_total_anio * peso_total)
  ) %>%
  ungroup()

# ---------------------------
# 5. Simular cobertura con variabilidad barrio-año
# ---------------------------
set.seed(5051)

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
        zona_base == "urbano" ~ 0.04,
        zona_base == "periurbano" ~ 0.09,
        zona_base == "rural" ~ 0.11,
        TRUE ~ 0.06
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
        anio <= 2018 ~ 0.00,
        zona_base == "urbano" ~ 0.08,
        zona_base == "periurbano" ~ 0.18,
        zona_base == "rural" ~ 0.16,
        TRUE ~ 0.10
      )
    ),
    efecto_impulso = impulso_local * runif(
      n(),
      min = 0.03,
      max = 0.12
    )
  ) %>%
  select(anio, Territorio, ruido_persistente, efecto_impulso)

base <- base %>%
  left_join(variabilidad_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    valor_individual = case_when(
      anio <= 2018 ~ NA_real_,
      anio == 2019 ~ case_when(
        zona_base == "urbano" ~ 0.30,
        zona_base == "periurbano" ~ 0.18,
        zona_base == "rural" ~ 0.10,
        TRUE ~ 0.15
      ),
      anio == 2020 ~ case_when(
        zona_base == "urbano" ~ 0.24,
        zona_base == "periurbano" ~ 0.14,
        zona_base == "rural" ~ 0.08,
        TRUE ~ 0.12
      ),
      anio == 2021 ~ case_when(
        zona_base == "urbano" ~ 0.26,
        zona_base == "periurbano" ~ 0.16,
        zona_base == "rural" ~ 0.10,
        TRUE ~ 0.14
      ),
      anio == 2022 ~ case_when(
        zona_base == "urbano" ~ 0.34,
        zona_base == "periurbano" ~ 0.24,
        zona_base == "rural" ~ 0.16,
        TRUE ~ 0.20
      ),
      anio >= 2023 ~ case_when(
        Territorio %in% barrios_criticos & zona_base == "periurbano" ~ 0.52,
        Territorio %in% barrios_criticos & zona_base == "rural" ~ 0.46,
        zona_base == "urbano" ~ 0.40,
        zona_base == "periurbano" ~ 0.30,
        zona_base == "rural" ~ 0.22,
        TRUE ~ 0.28
      )
    ),
    valor_individual =
      valor_individual +
        ruido_persistente +
        efecto_impulso,
    valor_individual = ifelse(
      is.na(valor_individual),
      NA_real_,
      pmin(pmax(valor_individual, 0.01), 0.90)
    ),
    embarazadas_programa = ifelse(
      is.na(valor_individual),
      NA,
      rbinom(n(), size = embarazadas, prob = valor_individual)
    )
  )

# ---------------------------
# 6. Función de agregación
# ---------------------------
calcular_indicador <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      valor = ifelse(
        all(is.na(valor_individual)),
        NA_real_,
        weighted.mean(valor_individual, w = embarazadas, na.rm = TRUE)
      ),
      .groups = "drop"
    )
}

# ---------------------------
# 7. Outputs
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

barrio_total <- base %>%
  calcular_indicador(anio, Territorio, zona_base) %>%
  mutate(
    iso3 = "COL", cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

program_cover_final <- bind_rows(
  municipio_total,
  municipio_zona,
  barrio_total
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, Territorio, cod_local, anio, zona, valor) %>%
  arrange(Territorio, anio, zona)

# ---------------------------
# 8. Guardar
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

write_csv(program_cover_final, file.path(csv_dir, "apoyo-embarazadas.csv"))
write_parquet(program_cover_final, file.path(parquet_dir, "apoyo-embarazadas.parquet"))
