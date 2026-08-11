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

barrios_criticos <- c(
  "El Progreso",
  "Nueva Esperanza",
  "Ribera Sur"
)
barrios_criticos <- intersect(barrios_criticos, smv_base$Territorio)

# ---------------------------
# 6. Leer mortalidad materna
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
# 7. Base barrio-año-etnia
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
# 8. Denominador embarazadas
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
# 9. Simular sobrecarga con variabilidad barrio-año
# ---------------------------
set.seed(4041)

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
        zona_base == "urbano" ~ 0.030,
        zona_base == "periurbano" ~ 0.075,
        zona_base == "rural" ~ 0.095,
        TRUE ~ 0.050
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
        zona_base == "urbano" ~ 0.05,
        zona_base == "periurbano" ~ 0.18,
        zona_base == "rural" ~ 0.22,
        TRUE ~ 0.10
      )
    ),
    efecto_impulso = impulso_local * runif(
      n(),
      min = 0.03,
      max = 0.13
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
        zona_base == "rural" ~ 0.07,
        TRUE ~ 0
      ),
      sd = 0.08
    ),
    variabilidad_barrio = case_when(
      Territorio %in% barrios_criticos & zona_base == "rural" ~ variabilidad_barrio + 0.10,
      Territorio %in% barrios_criticos & zona_base == "periurbano" ~ variabilidad_barrio + 0.07,
      TRUE ~ variabilidad_barrio
    )
  ) %>%
  select(Territorio, variabilidad_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "Territorio") %>%
  left_join(variabilidad_barrio_anual, by = c("anio", "Territorio")) %>%
  mutate(
    prob_base_zona = case_when(
      zona_base == "urbano" ~ 0.20,
      zona_base == "periurbano" ~ 0.38,
      zona_base == "rural" ~ 0.50,
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
      anio >= 2023 & zona_base == "urbano" ~ -0.015,
      anio >= 2023 & zona_base == "periurbano" ~ -0.010,
      TRUE ~ 0
    ),
    valor_individual =
      prob_base_zona +
        efecto_etnia +
        variabilidad_barrio +
        efecto_mortalidad +
        efecto_pandemia +
        mejora_cuidados +
        ruido_persistente +
        efecto_impulso,
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
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    zona = "Total",
    etnia = "Total"
  )

municipio_total_etnia <- base %>%
  calcular_indicador(anio, etnia) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    zona = "Total"
  )

municipio_zona_total <- base %>%
  calcular_indicador(anio, zona_base) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    etnia = "Total"
  ) %>%
  rename(zona = zona_base)

municipio_zona_etnia <- base %>%
  calcular_indicador(anio, zona_base, etnia) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_
  ) %>%
  rename(zona = zona_base)

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

sobrecarga_cuidados_final <- bind_rows(
  municipio_total,
  municipio_total_etnia,
  municipio_zona_total,
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

write_csv(sobrecarga_cuidados_final, file.path(csv_dir, "sobrecarga-embarazadas.csv"))
write_parquet(sobrecarga_cuidados_final, file.path(parquet_dir, "sobrecarga-embarazadas.parquet"))
