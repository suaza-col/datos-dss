# =========================================================
# San Martín del Valle - Mortalidad Materna
# Simulación con riesgo espacial-temporal dinámico
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

# ---------------------------
# 5. Eliminar territorios no usados
# ---------------------------
territorios_excluir <- c(
  "Colombia",
  "Baraya",
  "San Agustín"
)

smv_base <- smv_base %>%
  filter(!Territorio %in% territorios_excluir)

if (!all(c("Territorio", "tipo_zona") %in% names(smv_base))) {
  stop("El archivo SMV_map.csv debe contener las columnas 'Territorio' y 'tipo_zona'.")
}

# ---------------------------
# 6. Parámetros generales
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

barrios_criticos <- intersect(barrios_criticos, smv_base$Territorio)

# ---------------------------
# 7. Base de simulación
# ---------------------------
base <- expand_grid(
  anio       = anios,
  Territorio = smv_base$Territorio,
  grupo_edad = grupos_edad,
  etnia      = etnias
) %>%
  left_join(smv_base, by = "Territorio")

# ---------------------------
# 7.1 Riesgo espacial-temporal barrio-año
# ---------------------------
set.seed(999)

riesgo_estructural_barrio <- smv_base %>%
  mutate(
    riesgo_estructural = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano" ~ -0.20,
        tipo_zona == "periurbano" ~ 0.35,
        tipo_zona == "rural" ~ 0.70,
        TRUE ~ 0
      ),
      sd = case_when(
        tipo_zona == "urbano" ~ 0.25,
        tipo_zona == "periurbano" ~ 0.45,
        tipo_zona == "rural" ~ 0.60,
        TRUE ~ 0.30
      )
    ),
    riesgo_estructural = case_when(
      Territorio %in% barrios_criticos ~ riesgo_estructural + 0.45,
      TRUE ~ riesgo_estructural
    )
  ) %>%
  select(Territorio, tipo_zona, riesgo_estructural)

hotspots_persistentes <- riesgo_estructural_barrio %>%
  filter(tipo_zona %in% c("periurbano", "rural")) %>%
  slice_sample(prop = 0.25) %>%
  pull(Territorio)

shock_barrio_anual <- expand_grid(
  Territorio = smv_base$Territorio,
  anio       = anios
) %>%
  left_join(riesgo_estructural_barrio, by = "Territorio") %>%
  group_by(Territorio) %>%
  arrange(anio, .by_group = TRUE) %>%
  mutate(
    ruido_anual = rnorm(
      n(),
      mean = 0,
      sd = case_when(
        tipo_zona == "urbano" ~ 0.35,
        tipo_zona == "periurbano" ~ 0.75,
        tipo_zona == "rural" ~ 0.95,
        TRUE ~ 0.45
      )
    ),
    ruido_persistente = as.numeric(stats::filter(
      ruido_anual,
      filter = 0.45,
      method = "recursive"
    ))
  ) %>%
  ungroup() %>%
  group_by(anio) %>%
  mutate(
    prob_hotspot = case_when(
      tipo_zona == "urbano" ~ 0.08,
      tipo_zona == "periurbano" ~ 0.25,
      tipo_zona == "rural" ~ 0.35,
      TRUE ~ 0.10
    ),
    hotspot_dinamico = rbinom(n(), 1, prob_hotspot),
    efecto_hotspot = hotspot_dinamico * rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano" ~ 0.45,
        tipo_zona == "periurbano" ~ 0.90,
        tipo_zona == "rural" ~ 1.20,
        TRUE ~ 0.50
      ),
      sd = 0.35
    ),
    efecto_persistente = case_when(
      Territorio %in% hotspots_persistentes & anio %in% c(2019, 2020, 2021) ~ 0.75,
      Territorio %in% hotspots_persistentes & anio == 2022 ~ 0.35,
      TRUE ~ 0
    ),
    efecto_pandemia_local = case_when(
      anio == 2020 & tipo_zona == "urbano" ~ rnorm(n(), 0.30, 0.20),
      anio == 2020 & tipo_zona == "periurbano" ~ rnorm(n(), 0.70, 0.35),
      anio == 2020 & tipo_zona == "rural" ~ rnorm(n(), 0.95, 0.45),
      anio == 2021 & tipo_zona == "periurbano" ~ rnorm(n(), 0.35, 0.25),
      anio == 2021 & tipo_zona == "rural" ~ rnorm(n(), 0.45, 0.30),
      TRUE ~ 0
    ),
    riesgo_latente = riesgo_estructural +
      ruido_persistente +
      efecto_hotspot +
      efecto_persistente +
      efecto_pandemia_local,
    riesgo_std = as.numeric(scale(riesgo_latente)),
    shock_factor_anual = exp(
      case_when(
        anio == 2020 ~ 0.65 * riesgo_std,
        TRUE ~ 0.48 * riesgo_std
      )
    ),
    shock_factor_anual = pmin(pmax(shock_factor_anual, 0.35), 4.50)
  ) %>%
  ungroup() %>%
  select(Territorio, anio, shock_factor_anual, riesgo_latente, hotspot_dinamico)

base <- base %>%
  left_join(shock_barrio_anual, by = c("Territorio", "anio")) %>%
  mutate(
    prop_etnia = case_when(
      tipo_zona == "rural" & etnia == "Indígena" ~ 0.45,
      tipo_zona == "rural" & etnia == "No indígena" ~ 0.55,
      tipo_zona == "periurbano" & etnia == "Indígena" ~ 0.30,
      tipo_zona == "periurbano" & etnia == "No indígena" ~ 0.70,
      tipo_zona == "urbano" & etnia == "Indígena" ~ 0.15,
      tipo_zona == "urbano" & etnia == "No indígena" ~ 0.85,
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
# 8. Simular nacidos vivos
# ---------------------------
base_peso_barrio <- smv_base %>%
  mutate(
    peso_zona = case_when(
      tipo_zona == "urbano" ~ 1.80,
      tipo_zona == "periurbano" ~ 1.20,
      tipo_zona == "rural" ~ 0.55,
      TRUE ~ 1
    ),
    peso_barrio = runif(n(), 0.75, 1.25) * peso_zona
  ) %>%
  select(Territorio, peso_barrio)

base <- base %>%
  left_join(base_peso_barrio, by = "Territorio") %>%
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
# 9. Efectos sobre riesgo de muerte materna
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
        tipo_zona == "urbano" ~ log(0.90),
        tipo_zona == "periurbano" ~ log(1.00),
        tipo_zona == "rural" ~ log(1.10),
        TRUE ~ log(1)
      ),
      sdlog = 0.12
    ),
    efecto_barrio = case_when(
      Territorio %in% barrios_criticos & tipo_zona == "rural" ~ efecto_barrio * 1.45,
      Territorio %in% barrios_criticos & tipo_zona == "periurbano" ~ efecto_barrio * 1.35,
      Territorio %in% barrios_criticos & tipo_zona == "urbano" ~ efecto_barrio * 1.20,
      TRUE ~ efecto_barrio
    )
  ) %>%
  select(Territorio, efecto_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "Territorio") %>%
  mutate(
    tendencia = 1 - 0.015 * (anio - 2016),
    efecto_pandemia = case_when(
      anio == 2020 ~ 1.65,
      anio == 2021 ~ 1.30,
      anio == 2022 ~ 1.12,
      TRUE ~ 1
    ),
    efecto_pandemia_territorial = case_when(
      anio == 2020 & Territorio %in% barrios_criticos ~ 1.25,
      anio == 2021 & Territorio %in% barrios_criticos ~ 1.15,
      anio == 2022 & Territorio %in% barrios_criticos ~ 1.08,
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
# 10. Simular muertes maternas
# ---------------------------
base <- base %>%
  mutate(
    rmm_esperada =
      45 *
        efecto_zona[tipo_zona] *
        efecto_edad[grupo_edad] *
        efecto_etnia[etnia] *
        efecto_barrio *
        tendencia *
        efecto_pandemia *
        efecto_pandemia_territorial *
        efecto_pandemia_edad *
        shock_factor_anual,
    rmm_esperada = pmax(rmm_esperada, 5),
    prob_muerte = rmm_esperada / 100000,
    muertes_maternas = rbinom(
      n(),
      size = nacidos_vivos,
      prob = pmin(prob_muerte, 0.01)
    )
  )

# ---------------------------
# 11. Base final + valor único calibrado
# ---------------------------
tendencia_municipal <- tibble(
  anio = anios,
  mortalidad_media_objetivo = c(
    42, 41, 43, 42,
    76,
    60, 52, 46, 41, 38
  )
)

rmm_estructural_barrio <- base %>%
  left_join(tendencia_municipal, by = "anio") %>%
  group_by(anio, Territorio, tipo_zona, etnia) %>%
  summarise(
    valor_raw = weighted.mean(
      rmm_esperada,
      w = nacidos_vivos,
      na.rm = TRUE
    ),
    mortalidad_media_objetivo = first(mortalidad_media_objetivo),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    valor = valor_raw *
      mortalidad_media_objetivo / mean(valor_raw, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  rename(zona = tipo_zona) %>%
  select(anio, Territorio, zona, etnia, valor)

rmm_estructural_barrio_general <- base %>%
  left_join(tendencia_municipal, by = "anio") %>%
  group_by(anio, Territorio, tipo_zona) %>%
  summarise(
    valor_raw = weighted.mean(
      rmm_esperada,
      w = nacidos_vivos,
      na.rm = TRUE
    ),
    mortalidad_media_objetivo = first(mortalidad_media_objetivo),
    .groups = "drop"
  ) %>%
  group_by(anio) %>%
  mutate(
    valor = valor_raw *
      mortalidad_media_objetivo / mean(valor_raw, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(etnia = "Total") %>%
  rename(zona = tipo_zona) %>%
  select(anio, Territorio, zona, etnia, valor)

tasa_mortalidad_materna_final <- bind_rows(
  rmm_estructural_barrio,
  rmm_estructural_barrio_general
) %>%
  mutate(
    iso3      = "COL",
    cod_local = NA_character_,
    sexo      = "Mujeres",
    valor     = round(valor, 1)
  ) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor)

# ---------------------------
# 12. Agregaciones municipio
# ---------------------------
total_municipio_general <- tasa_mortalidad_materna_final %>%
  filter(etnia == "Total") %>%
  group_by(anio) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    zona = "Total",
    etnia = "Total"
  ) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor)

total_municipio_etnia <- tasa_mortalidad_materna_final %>%
  filter(etnia != "Total") %>%
  group_by(anio, etnia) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    zona = "Total"
  ) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor)

municipio_zona_general <- tasa_mortalidad_materna_final %>%
  filter(etnia == "Total") %>%
  group_by(anio, zona) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    Territorio = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    etnia = "Total"
  ) %>%
  select(iso3, Territorio, cod_local, anio, sexo, zona, etnia, valor)

# ---------------------------
# 13. Dataset final
# ---------------------------
tasa_mortalidad_materna_final <- bind_rows(
  total_municipio_general,
  total_municipio_etnia,
  municipio_zona_general,
  tasa_mortalidad_materna_final
) %>%
  arrange(Territorio, anio, zona, etnia)

# ---------------------------
# 14. Guardar archivos
# ---------------------------
csv_dir <- file.path(output_dir, "csv")
parquet_dir <- file.path(output_dir, "parquet")

if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)
if (!dir.exists(parquet_dir)) dir.create(parquet_dir, recursive = TRUE)

archivo_csv <- file.path(csv_dir, "maternal_mortality_rate.csv")
write_csv(tasa_mortalidad_materna_final, archivo_csv)

archivo_parquet <- file.path(parquet_dir, "maternal_mortality_rate.parquet")
write_parquet(tasa_mortalidad_materna_final, archivo_parquet)
