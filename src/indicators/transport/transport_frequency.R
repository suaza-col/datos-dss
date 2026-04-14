# ==============================
# MOCK DSS Indicator: Frecuencia de transporte público
# ==============================
# Source: packages/data-r/mock/frecuencia_transporte.xlsx
#
# Stratifiers: sexo, grupo_edad, zona
#
# Output column order (matches TypeScript filterStratifiedRows):
#   anio[0], valor[1], sexo[2], grupo_edad[3], zona[4]
#
# Files generated:
#   outputs/csv/transport_frequency.csv
#   outputs/parquet/transport_frequency.parquet
# ==============================

library(here)

source(here("packages/data-r/src/mock/stratified_indicator_mock.R"))

process_mock_transport_frequency <- function(
    mock_file = here("packages/data-r/mock/frecuencia_transporte.xlsx"),
    output_dir = here("outputs")) {
  process_mock_stratified_indicator(
    mock_file   = mock_file,
    output_name = "transport_frequency",
    output_dir  = output_dir
  )
}

# ── Command-line execution ─────────────────────────────────────────────────────
if (!interactive()) {
  process_mock_transport_frequency()
}
