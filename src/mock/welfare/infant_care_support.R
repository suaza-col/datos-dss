# =========================================================
# Cobertura del programa municipal de apoyo al cuidado infantil
# "Cuidar en Comunidad" en mujeres embarazadas que residen en
# barrios periféricos del Municipio de San Martín del Valle
# =========================================================
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
set.seed(7070)

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
      zona_base == "Urbano central" ~ "urbano",
      zona_base == "Periurbano" ~ "periurbano",
      zona_base == "Rural" ~ "rural",
      TRUE ~ tolower(zona_base)
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

barrios_criticos <- c("El Progreso", "Nueva Esperanza", "Ribera Sur")
barrios_criticos <- intersect(barrios_criticos, smv_base$Territorio)

# ---------------------------
# 6. Base barrio-año-etnia
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio,
  etnia      = etnias
) %>%
  left_join(smv_base, by = "Territorio") %>%
  mutate(
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
# 7. Denominador embarazadas
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
# 8. Simular cobertura Cuidar en Comunidad
# Con variabilidad barrio-año
# ---------------------------
set.seed(7071)

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
        zona_base == "periurbano" ~ 0.08,
        zona_base == "rural" ~ 0.10,
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
  group_by(anio) %>%
  mutate(
    impulso_local = rbinom(
      n(),
      1,
      prob = case_when(
        anio <= 2019 ~ 0.00,
        zona_base == "urbano" ~ 0.08,
        zona_base == "periurbano" ~ 0.18,
        zona_base == "rural" ~ 0.14,
        TRUE ~ 0.10
      )
    ),
    efecto_impulso = impulso_local * runif(
      n(),
      min = 0.04,
      max = 0.14
    )
  ) %>%
  ungroup() %>%
  select(anio, Territorio, ruido_persistente, efecto_impulso)

base <- base %>%
  left_join(variabilidad_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    valor_individual = case_when(
      anio <= 2019 ~ NA_real_,
      anio == 2020 ~ case_when(
        zona_base == "urbano" ~ 0.08,
        zona_base == "periurbano" ~ 0.05,
        zona_base == "rural" ~ 0.03,
        TRUE ~ 0.04
      ),
      anio == 2021 ~ case_when(
        zona_base == "urbano" ~ 0.14,
        zona_base == "periurbano" ~ 0.09,
        zona_base == "rural" ~ 0.05,
        TRUE ~ 0.08
      ),
      anio == 2022 ~ case_when(
        zona_base == "urbano" ~ 0.24,
        zona_base == "periurbano" ~ 0.16,
        zona_base == "rural" ~ 0.09,
        TRUE ~ 0.14
      ),
      anio >= 2023 ~ case_when(
        Territorio %in% barrios_criticos & zona_base == "periurbano" ~ 0.62,
        Territorio %in% barrios_criticos & zona_base == "rural" ~ 0.55,
        Territorio %in% barrios_criticos & zona_base == "urbano" ~ 0.66,
        zona_base == "urbano" ~ 0.48,
        zona_base == "periurbano" ~ 0.38,
        zona_base == "rural" ~ 0.25,
        TRUE ~ 0.30
      )
    ),
    efecto_etnia = case_when(
      is.na(valor_individual) ~ NA_real_,
      etnia == "Indígena" & anio <= 2022 ~ -0.06,
      etnia == "Indígena" & anio >= 2023 & Territorio %in% barrios_criticos ~ 0.05,
      etnia == "Indígena" & anio >= 2023 ~ -0.01,
      TRUE ~ 0
    ),
    efecto_tendencia_post2023 = case_when(
      anio == 2024 ~ 0.05,
      anio == 2025 ~ 0.10,
      TRUE ~ 0
    ),
    valor_individual =
      valor_individual +
        efecto_etnia +
        efecto_tendencia_post2023 +
        ruido_persistente +
        efecto_impulso,
    valor_individual = ifelse(
      is.na(valor_individual),
      NA_real_,
      pmin(pmax(valor_individual, 0.00), 0.90)
    ),
    embarazadas_cubiertas = ifelse(
      is.na(valor_individual),
      NA,
      rbinom(n(), size = embarazadas, prob = valor_individual)
    )
  )

# ---------------------------
# 9. Función de agregación
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
# 10. Datos globales (municipio)
# ---------------------------
municipio_total <- base %>%
  calcular_indicador(anio) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total", etnia = "Total"
  )

municipio_total_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, zona = "Total"
  )

municipio_zona_total <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_, etnia = "Total"
  ) %>%
  rename(zona = zona_base)

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, zona_base, etnia) %>%
  mutate(
    iso3 = "COL", Territorio = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

global_data <- bind_rows(
  municipio_total,
  municipio_total_etnia,
  municipio_zona_total,
  municipio_zona_etnia
) %>%
  mutate(valor = round(valor, 4)) %>%
  select(iso3, Territorio, cod_local, anio, zona, etnia, valor) %>%
  arrange(Territorio, anio, zona, etnia)

# ---------------------------
# 11. Datos de barrio (municipal)
# ---------------------------
barrio_total <- base %>%
  calcular_indicador(anio, Territorio, zona_base) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_,
    etnia = "Total"
  ) %>%
  rename(zona = zona_base)

barrio_etnia <- base %>%
  calcular_indicador(anio, Territorio, zona_base, etnia) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

municipal_data <- bind_rows(barrio_total, barrio_etnia) %>%
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

write_csv(global_data, file.path(csv_dir, "apoyo-infantil.csv"))
write_parquet(global_data, file.path(parquet_dir, "apoyo-infantil.parquet"))

write_csv(municipal_data, file.path(csv_dir, "apoyo-infantil-municipal.csv"))
write_parquet(municipal_data, file.path(parquet_dir, "apoyo-infantil-municipal.parquet"))
