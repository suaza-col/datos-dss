# =========================================================
# San Martín del Valle - Mortalidad Materna
# Simulación con eventos + RMM estructural variable
# Zona: Urbano / Periurbano / Rural — Etnia: Indígena / No indígena
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
paquetes <- c("dplyr", "tidyr", "readr", "arrow")

instalados <- rownames(installed.packages())
for (p in paquetes) {
  if (!(p %in% instalados)) install.packages(p)
}

library(dplyr)
library(tidyr)
library(readr)
library(arrow)

# ---------------------------
# 3. Semilla
# ---------------------------
set.seed(1234)

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
      tipo_zona == "Periurbano" ~ "periurbano",
      tipo_zona == "Rural" ~ "rural",
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
grupos_edad <- c("10-14", "15-19", "20-34", "35-49")
etnias <- c("Indígena", "No indígena")

barrios_criticos <- c(
  "La Cañada",
  "El Mirador",
  "Los Pinos",
  "Nueva Esperanza",
  "Vista Hermosa",
  "Santa Lucía"
)
barrios_criticos <- intersect(barrios_criticos, smv_base$NAME_2)

# ---------------------------
# 6. Base de simulación
# ---------------------------
base <- expand_grid(
  anio = anios,
  NAME_2 = smv_base$NAME_2,
  grupo_edad = grupos_edad,
  etnia = etnias
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
    ),
    prop_edad = case_when(
      grupo_edad == "10-14" ~ 0.03,
      grupo_edad == "15-19" ~ 0.12,
      grupo_edad == "20-34" ~ 0.60,
      grupo_edad == "35-49" ~ 0.25,
      TRUE ~ NA_real_
    )
  )

# ---------------------------
# 7. Simular nacidos vivos
# ---------------------------
base_peso_barrio <- smv_base %>%
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
  left_join(base_peso_barrio, by = "NAME_2") %>%
  group_by(anio) %>%
  mutate(
    nv_total_anio = round(
      case_when(
        anio == 2020 ~ 1380,
        anio == 2021 ~ 1420,
        TRUE ~ 1500 - 8 * (anio - 2016)
      )
    ),
    peso_total = peso_barrio * prop_edad * prop_etnia,
    peso_total = peso_total / sum(peso_total, na.rm = TRUE),
    nacidos_vivos_esperados = nv_total_anio * peso_total,
    nacidos_vivos = rpois(n(), lambda = pmax(nacidos_vivos_esperados, 0.1))
  ) %>%
  ungroup()

# ---------------------------
# 8. Efectos sobre riesgo de muerte materna
# ---------------------------
efecto_zona <- c(
  "urbano"     = 1.00,
  "periurbano" = 1.35,
  "rural"      = 1.90
)

efecto_edad <- c(
  "10-14" = 1.70,
  "15-19" = 1.25,
  "20-34" = 1.00,
  "35-49" = 1.50
)

efecto_etnia <- c(
  "No indígena" = 1.00,
  "Indígena"    = 1.45
)

efecto_barrio <- smv_base %>%
  mutate(
    efecto_barrio = rlnorm(
      n(),
      meanlog = case_when(
        tipo_zona == "urbano"     ~ log(0.90),
        tipo_zona == "periurbano" ~ log(1.00),
        tipo_zona == "rural"      ~ log(1.10),
        TRUE ~ log(1)
      ),
      sdlog = 0.12
    ),
    efecto_barrio = case_when(
      NAME_2 %in% barrios_criticos & tipo_zona == "rural"      ~ efecto_barrio * 1.45,
      NAME_2 %in% barrios_criticos & tipo_zona == "periurbano" ~ efecto_barrio * 1.35,
      NAME_2 %in% barrios_criticos & tipo_zona == "urbano"     ~ efecto_barrio * 1.20,
      TRUE ~ efecto_barrio
    )
  ) %>%
  select(NAME_2, efecto_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    tendencia = 1 - 0.015 * (anio - 2016),

    efecto_pandemia = case_when(
      anio == 2020 ~ 1.65,
      anio == 2021 ~ 1.30,
      anio == 2022 ~ 1.12,
      TRUE ~ 1
    ),

    efecto_pandemia_territorial = case_when(
      anio == 2020 & NAME_2 %in% barrios_criticos ~ 1.25,
      anio == 2021 & NAME_2 %in% barrios_criticos ~ 1.15,
      anio == 2022 & NAME_2 %in% barrios_criticos ~ 1.08,
      TRUE ~ 1
    ),

    efecto_pandemia_edad = case_when(
      anio == 2020 & grupo_edad == "10-14" ~ 1.20,
      anio == 2020 & grupo_edad == "35-49" ~ 1.15,
      anio == 2021 & grupo_edad == "10-14" ~ 1.10,
      anio == 2021 & grupo_edad == "35-49" ~ 1.08,
      TRUE ~ 1
    )
  )

# ---------------------------
# 9. Simular muertes maternas
# ---------------------------
base <- base %>%
  mutate(
    rmm_esperada = 45 *
      efecto_zona[tipo_zona] *
      efecto_edad[grupo_edad] *
      efecto_etnia[etnia] *
      efecto_barrio *
      tendencia *
      efecto_pandemia *
      efecto_pandemia_territorial *
      efecto_pandemia_edad,

    prob_muerte = rmm_esperada / 100000,

    muertes_maternas = rbinom(
      n(),
      size = nacidos_vivos,
      prob = pmin(prob_muerte, 0.01)
    )
  )

# ---------------------------
# 10. Valor estructural por barrio
# Usar rmm_esperada * factor_mapa para evitar ruido
# extremo en celdas con pocos nacidos vivos
# ---------------------------
set.seed(4321)

variabilidad_mapa_barrio <- smv_base %>%
  mutate(
    factor_mapa = rlnorm(n(), meanlog = 0, sdlog = 0.28),
    factor_mapa = case_when(
      tipo_zona == "urbano"     ~ factor_mapa * 0.85,
      tipo_zona == "periurbano" ~ factor_mapa * 1.05,
      tipo_zona == "rural"      ~ factor_mapa * 1.25,
      TRUE ~ factor_mapa
    ),
    factor_mapa = case_when(
      NAME_2 %in% barrios_criticos ~ factor_mapa * 1.25,
      TRUE ~ factor_mapa
    )
  ) %>%
  select(NAME_2, factor_mapa)

rmm_estructural_barrio_etnia <- base %>%
  left_join(variabilidad_mapa_barrio, by = "NAME_2") %>%
  group_by(anio, NAME_2, tipo_zona, etnia) %>%
  summarise(
    valor = weighted.mean(
      rmm_esperada * factor_mapa,
      w = nacidos_vivos,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  rename(zona = tipo_zona)

rmm_estructural_barrio_general <- base %>%
  left_join(variabilidad_mapa_barrio, by = "NAME_2") %>%
  group_by(anio, NAME_2, tipo_zona) %>%
  summarise(
    valor = weighted.mean(
      rmm_esperada * factor_mapa,
      w = nacidos_vivos,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(etnia = "Total") %>%
  rename(zona = tipo_zona)

barrio_nivel <- bind_rows(
  rmm_estructural_barrio_etnia,
  rmm_estructural_barrio_general
) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_,
    sexo = "Mujeres"
  )

# ---------------------------
# 11. Agregaciones municipio
# ---------------------------
total_municipio_general <- barrio_nivel %>%
  filter(etnia == "Total") %>%
  group_by(anio) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    zona = "Total",
    etnia = "Total"
  )

total_municipio_etnia <- barrio_nivel %>%
  filter(etnia != "Total") %>%
  group_by(anio, etnia) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    zona = "Total"
  )

municipio_zona_general <- barrio_nivel %>%
  filter(etnia == "Total") %>%
  group_by(anio, zona) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    etnia = "Total"
  )

# ---------------------------
# 12. Dataset final
# ---------------------------
tasa_mortalidad_materna_final <- bind_rows(
  total_municipio_general,
  total_municipio_etnia,
  municipio_zona_general,
  barrio_nivel
) %>%
  mutate(valor = round(valor, 1)) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, zona, etnia, valor) %>%
  arrange(NAME_2, anio, zona, etnia)

# ---------------------------
# 13. Guardar archivos
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

archivo_csv <- file.path(csv_dir, "maternal_mortality_rate.csv")
write_csv(tasa_mortalidad_materna_final, archivo_csv)

archivo_parquet <- file.path(parquet_dir, "maternal_mortality_rate.parquet")
write_parquet(tasa_mortalidad_materna_final, archivo_parquet)
