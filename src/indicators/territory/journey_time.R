# ==============================
# MOCK DSS Indicator: Tiempo promedio de traslado a centro de salud
# ==============================
# Source: packages/data-r/mock/tiempo_traslado.xlsx
#
# Stratifiers: sexo, grupo_edad, zona
#
# Output column order (matches TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Aggregate labels:
#   sexo      → "Todos/as"
#   grupo_edad → "Todas las edades"
#   zona       → "Todas las zonas"
#
# Files generated:
#   outputs/csv/journey_time.csv
#   outputs/parquet/journey_time.parquet
# ==============================

library(here)

source(here("packages/data-r/src/mock/stratified_indicator_mock.R"))

process_mock_journey_time <- function(
    mock_file = here("packages/data-r/mock/tiempo_traslado.xlsx"),
    output_dir = here("outputs")) {
  process_mock_stratified_indicator(
    mock_file   = mock_file,
    output_name = "journey_time",
    output_dir  = output_dir
  )
}

# ── Command-line execution ─────────────────────────────────────────────────────
if (!interactive()) {
  process_mock_journey_time()
}
