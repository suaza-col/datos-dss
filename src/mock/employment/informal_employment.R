# ==============================
# MOCK DSS Indicator: Porcentaje de mujeres embarazadas con empleo informal
# ==============================
# Source: packages/data-r/mock/empleo_informal.xlsx
#
# Stratifiers: sexo, grupo_edad, zona
#
# Output column order (matches TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Files generated:
#   outputs/csv/informal_employment.csv
#   outputs/parquet/informal_employment.parquet
# ==============================

library(here)

source(here("packages/data-r/src/mock/stratified_indicator_mock.R"))

process_mock_informal_employment <- function(
    mock_file = here("packages/data-r/mock/empleo_informal.xlsx"),
    output_dir = here("outputs")) {
  process_mock_stratified_indicator(
    mock_file   = mock_file,
    output_name = "informal_employment",
    output_dir  = output_dir
  )
}

# ── Command-line execution ─────────────────────────────────────────────────────
if (!interactive()) {
  process_mock_informal_employment()
}
