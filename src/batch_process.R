# packages/data-r/src/batch_process.R

# ==============================
# Batch Processing Script for All Indicators
# This script processes multiple indicators efficiently
# ==============================

library(here)
library(purrr)
library(dplyr)
source(here("packages/data-r/R/process_indicator.R"))

#' Process all configured indicators
#'
#' @param config_path Path to indicators configuration file
#' @param output_dir Directory for outputs
#' @param parallel Whether to use parallel processing
#' @param max_workers Maximum number of parallel workers
#' @return List of processing results
#' @export
process_all_indicators <- function(config_path = here("packages/data-r/config/indicators.yml"),
                                   output_dir = here("outputs"),
                                   parallel = FALSE,
                                   max_workers = 2) {
  message("🚀 Starting batch processing of all indicators...")

  # Load configuration
  config <- yaml::read_yaml(config_path)
  indicator_ids <- names(config)

  message(glue::glue("📋 Found {length(indicator_ids)} indicators to process"))
  message(glue::glue("📁 Output directory: {output_dir}"))

  # Create processing function
  process_single <- function(indicator_id) {
    message(glue::glue("🔄 Processing: {indicator_id}"))

    tryCatch(
      {
        result <- process_indicator(
          indicator_id = indicator_id,
          config_path = config_path,
          output_dir = output_dir
        )

        message(glue::glue("✅ Completed: {indicator_id}"))
        return(result)
      },
      error = function(e) {
        message(glue::glue("❌ Failed: {indicator_id} - {e$message}"))
        return(list(
          indicator_id = indicator_id,
          error = e$message,
          processed_at = Sys.time()
        ))
      }
    )
  }

  # Process indicators
  if (parallel && length(indicator_ids) > 1) {
    message(glue::glue("⚡ Using parallel processing with {max_workers} workers"))

    # Set up parallel processing
    future::plan(future::multisession, workers = min(max_workers, length(indicator_ids)))

    results <- furrr::future_map(indicator_ids, process_single, .progress = TRUE)
    names(results) <- indicator_ids
  } else {
    message("🔄 Using sequential processing")
    results <- purrr::map(indicator_ids, process_single)
    names(results) <- indicator_ids
  }

  # Summary
  successful <- sum(purrr::map_lgl(results, ~ is.null(.x$error)))
  failed <- length(results) - successful

  message("📊 BATCH PROCESSING SUMMARY")
  message(glue::glue("✅ Successful: {successful}/{length(results)}"))
  message(glue::glue("❌ Failed: {failed}/{length(results)}"))

  if (failed > 0) {
    failed_indicators <- names(results)[purrr::map_lgl(results, ~ !is.null(.x$error))]
    message("❌ Failed indicators:")
    purrr::walk(failed_indicators, ~ message(glue::glue("   - {.x}")))
  }

  return(results)
}

#' Process only priority indicators for Suaza
#'
#' @param output_dir Directory for outputs
#' @return Processing results
process_suaza_priorities <- function(output_dir = here("outputs")) {
  # Priority indicators for Suaza implementation
  priority_indicators <- c(
    "suicide_huila",
    "analytics_suaza",
    "huila_map",
    # Add more as they're configured
  )

  message("🇨🇴 Processing priority indicators for Suaza, Colombia")
  message(glue::glue("📋 Processing {length(priority_indicators)} priority indicators"))

  results <- purrr::map(priority_indicators, ~ {
    message(glue::glue("🔄 Processing priority indicator: {.x}"))
    process_indicator(.x, output_dir = output_dir)
  })

  names(results) <- priority_indicators

  message("✅ Suaza priority indicators completed!")

  return(results)
}

#' Generate processing report
#'
#' @param results Results from batch processing
#' @return Data frame with processing summary
generate_processing_report <- function(results) {
  report <- purrr::imap_dfr(results, ~ {
    if (is.null(.x$error)) {
      # Successful processing
      tibble::tibble(
        indicator_id = .y,
        status = "success",
        n_rows = nrow(.x$data %||% NA),
        output_files = length(.x$output_files %||% 0),
        processed_at = .x$processed_at,
        error_message = NA_character_
      )
    } else {
      # Failed processing
      tibble::tibble(
        indicator_id = .y,
        status = "failed",
        n_rows = NA_integer_,
        output_files = 0L,
        processed_at = .x$processed_at,
        error_message = .x$error
      )
    }
  })

  return(report)
}

# Command-line execution
if (!interactive()) {
  args <- commandArgs(trailingOnly = TRUE)

  if (length(args) == 0 || args[1] == "all") {
    # Process all indicators
    results <- process_all_indicators()
    report <- generate_processing_report(results)
    readr::write_csv(report, here("outputs/processing_report.csv"))
  } else if (args[1] == "suaza") {
    # Process Suaza priorities
    results <- process_suaza_priorities()
  } else {
    # Process specific indicator
    indicator_id <- args[1]
    result <- process_indicator(indicator_id)
    message(glue::glue("✅ Processed {indicator_id} successfully"))
  }
}
