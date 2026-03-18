# ==============================
# Tests for utility functions
# ==============================

test_that("normalize_text works correctly", {
  expect_equal(
    normalize_text("TASA DE DESNUTRICIÓN CRÓNICA"),
    "tasa de desnutricion cronica"
  )
  expect_equal(normalize_text("Mujeres y Hombres"), "mujeres y hombres")
  expect_equal(normalize_text("  Multiple   Spaces  "), "multiple spaces")
})

test_that("clean_indicator_names removes X prefix from years", {
  df <- data.frame(
    "Departamento" = c("Lima", "Cusco"),
    "X2020" = c(10, 15),
    "x2021" = c(12, 17),
    "other_col" = c(1, 2)
  )

  cleaned <- clean_indicator_names(df)

  expect_true("2020" %in% names(cleaned))
  expect_true("2021" %in% names(cleaned))
  expect_false("X2020" %in% names(cleaned))
  expect_false("x2021" %in% names(cleaned))
  expect_true("other_col" %in% names(cleaned))
})

test_that("parse_indicator_numbers handles various formats", {
  expect_equal(parse_indicator_numbers(c("10.5", "15.3", "20")), c(10.5, 15.3, 20))
  expect_equal(parse_indicator_numbers(c("NA", "", "5.5")), c(NA, NA, 5.5))
})

# ==============================
# Integration tests for indicator processing
# ==============================

# packages/data-r/tests/testthat/test-indicators.R

# test_that("chronic malnutrition indicator config is valid", {
#   config_path <- here::here("packages/data-r/config/indicators.yml")

#   skip_if_not(file.exists(config_path), "Config file not found")

#   config <- yaml::read_yaml(config_path)

#   expect_true("chronic_malnutrition_under5" %in% names(config))

#   malnut_config <- config$chronic_malnutrition_under5

#   # Check required fields
#   expect_true("id" %in% names(malnut_config))
#   expect_true("names" %in% names(malnut_config))
#   expect_true("source" %in% names(malnut_config))
#   expect_true("processing" %in% names(malnut_config))
#   expect_true("output" %in% names(malnut_config))

#   # Check names have both languages
#   expect_true("es" %in% names(malnut_config$names))
#   expect_true("en" %in% names(malnut_config$names))
# })

# Mock test for indicator processing (doesn't require internet)
test_that("indicator processing handles missing URLs gracefully", {
  # This test uses a mock configuration to test error handling
  mock_config <- list(
    test_indicator = list(
      names = list(es = "Test", en = "Test"),
      source = list(
        page_url = "https://nonexistent-url.com",
        target_title = "Nonexistent Indicator",
        keywords = c("test")
      ),
      processing = list(sheet_number = 1),
      output = list(format = c("csv"))
    )
  )

  # Save temporary config
  temp_config <- tempfile(fileext = ".yml")
  yaml::write_yaml(mock_config, temp_config)

  # Expect error when URL doesn't exist
  expect_error(
    process_indicator("test_indicator", config_path = temp_config),
    "Could not find Excel file"
  )

  file.remove(temp_config)
})
