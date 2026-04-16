# ==============================
# MOCK DSS Indicator: Porcentaje de mujeres embarazadas con sobrecarga de cuidados
# ==============================
# Source: packages/data-r/mock/sobrecarga_cuidados.xlsx
#
# Stratifiers: sexo, grupo_edad, zona
#
# Output column order (matches TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Files generated:
#   outputs/csv/care_overload.csv
#   outputs/parquet/care_overload.parquet
# ==============================

library(here)

source(here("packages/data-r/src/mock/stratified_indicator_mock.R"))

process_mock_care_overload <- function(
    mock_file = here("packages/data-r/mock/sobrecarga_cuidados.xlsx"),
    output_dir = here("outputs")) {
  process_mock_stratified_indicator(
    mock_file   = mock_file,
    output_name = "care_overload",
    output_dir  = output_dir
  )
}

# ── Command-line execution ─────────────────────────────────────────────────────
if (!interactive()) {
  process_mock_care_overload()
}
