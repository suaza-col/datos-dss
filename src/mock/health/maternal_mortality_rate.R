# =========================================================
# San Martín del Valle - Simulación de Mortalidad Materna
# =========================================================

library(here)

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
paquetes <- c("dplyr", "tidyr", "readr", "writexl", "ggplot2")

instalados <- rownames(installed.packages())
for (p in paquetes) {
  if (!(p %in% instalados)) install.packages(p)
}

library(dplyr)
library(tidyr)
library(readr)
library(writexl)
library(ggplot2)

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

# 3 rurales + 1 periurbano
barrios_criticos <- c(
  "La Cañada",
  "El Mirador",
  "Los Pinos",
  "Nueva Esperanza"
)

# ---------------------------
# 6. Base barrio-año-edad
# ---------------------------
base <- expand_grid(
  anio = anios,
  NAME_2 = smv_base$NAME_2,
  grupo_edad = grupos_edad
) %>%
  left_join(smv_base, by = "NAME_2")

# ---------------------------
# 7. Efectos estructurales
# ---------------------------
# Gradiente esperado:
# urbano < periurbano < rural

efecto_zona <- c(
  "urbano" = 0,
  "periurbano" = 22,
  "rural" = 48
)

efecto_edad <- c(
  "10-14" = 28,
  "15-19" = 12,
  "20-34" = 0,
  "35-49" = 18
)

# variabilidad intra-zona por barrio
efecto_barrio <- smv_base %>%
  mutate(
    efecto_barrio = rnorm(
      n(),
      mean = case_when(
        tipo_zona == "urbano" ~ -3,
        tipo_zona == "periurbano" ~ 3,
        tipo_zona == "rural" ~ 8,
        TRUE ~ 0
      ),
      sd = 6
    ),
    efecto_barrio = ifelse(
      NAME_2 %in% barrios_criticos,
      efecto_barrio + 14,
      efecto_barrio
    )
  ) %>%
  select(NAME_2, efecto_barrio)

base <- base %>%
  left_join(efecto_barrio, by = "NAME_2") %>%
  mutate(
    t = anio - min(anios),
    tendencia_anual = -1.2 * t
  )

# ---------------------------
# 8. Simular tasa directamente
# ---------------------------
# Esto evita que todo salga 0 en mapas por barrio

base <- base %>%
  mutate(
    valor = 35 +
      efecto_zona[tipo_zona] +
      efecto_edad[grupo_edad] +
      efecto_barrio +
      tendencia_anual +
      rnorm(n(), 0, 7),
    valor = pmax(valor, 5)
  )

# ---------------------------
# 9. Salidas finales
# ---------------------------

# 9.1 Total municipio
total_municipio <- base %>%
  group_by(anio) %>%
  summarise(
    valor = weighted.mean(
      valor,
      w = case_when(
        grupo_edad == "10-14" ~ 0.03,
        grupo_edad == "15-19" ~ 0.12,
        grupo_edad == "20-34" ~ 0.60,
        grupo_edad == "35-49" ~ 0.25
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    grupo_edad = "Todas las edades",
    zona = "Total"
  ) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# 9.2 Municipio por edad
municipio_edad <- base %>%
  group_by(anio, grupo_edad) %>%
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
  ) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# 9.3 Municipio por zona
municipio_zona <- base %>%
  group_by(anio, tipo_zona) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres",
    grupo_edad = "Todas las edades"
  ) %>%
  rename(zona = tipo_zona) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# 9.4 Municipio por zona y edad
municipio_zona_edad <- base %>%
  group_by(anio, tipo_zona, grupo_edad) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    NAME_2 = "San Martín del Valle",
    cod_local = NA_character_,
    sexo = "Mujeres"
  ) %>%
  rename(zona = tipo_zona) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# 9.5 Barrio total
barrio_total <- base %>%
  group_by(anio, NAME_2, tipo_zona) %>%
  summarise(
    valor = weighted.mean(
      valor,
      w = case_when(
        grupo_edad == "10-14" ~ 0.03,
        grupo_edad == "15-19" ~ 0.12,
        grupo_edad == "20-34" ~ 0.60,
        grupo_edad == "35-49" ~ 0.25
      ),
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_,
    sexo = "Mujeres",
    grupo_edad = "Todas las edades"
  ) %>%
  rename(zona = tipo_zona) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# 9.6 Barrio por edad
barrio_edad <- base %>%
  group_by(anio, NAME_2, tipo_zona, grupo_edad) %>%
  summarise(
    valor = mean(valor, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    iso3 = "COL",
    cod_local = NA_character_,
    sexo = "Mujeres"
  ) %>%
  rename(zona = tipo_zona) %>%
  select(iso3, NAME_2, cod_local, anio, sexo, grupo_edad, zona, valor)

# Unir todo
tasa_mortalidad_materna_final <- bind_rows(
  total_municipio,
  municipio_edad,
  municipio_zona,
  municipio_zona_edad,
  barrio_total,
  barrio_edad
) %>%
  arrange(NAME_2, anio, zona, grupo_edad)

# ---------------------------
# 10. Guardar archivos
# ---------------------------
archivo_csv <- file.path(output_dir, "tasa_mortalidad_materna_SMV.csv")
write_csv(tasa_mortalidad_materna_final, archivo_csv)
