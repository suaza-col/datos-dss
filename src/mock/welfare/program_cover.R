# ==============================
# MOCK DSS Indicator: Porcentaje de cobertura de programa apoyo a embarazadas
# ==============================
# Source: packages/data-r/mock/cobertura_programa.xlsx
#
# Stratifiers: sexo, grupo_edad, zona
#
# Output column order (matches TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Files generated:
#   outputs/csv/program_cover.csv
#   outputs/parquet/program_cover.parquet
# ==============================

library(here)

source(here("packages/data-r/src/mock/stratified_indicator_mock.R"))

process_mock_welfare_program_cover <- function(
    mock_file = here("packages/data-r/mock/cobertura_programa.xlsx"),
    output_dir = here("outputs")) {
  process_mock_stratified_indicator(
    mock_file   = mock_file,
    output_name = "program_cover",
    output_dir  = output_dir
  )
}

# ── Command-line execution ─────────────────────────────────────────────────────
if (!interactive()) {
  process_mock_welfare_program_cover()
}
