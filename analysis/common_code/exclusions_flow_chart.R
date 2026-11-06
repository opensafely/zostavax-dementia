library(tidyverse)
library(readr)
library(arrow)
library(fs)
library(here)

exclusion_flow_chart <- function(vaccine_name, analysis = "main") {

  # Define exclusion criteria
  exclusions <- c(
    exclude_not_mf_sex = "Non male/female sex",
    exclude_missing_imd = "Missing IMD",
    exclude_missing_region = "Missing region"
  )

  # Additional exclusions for main population
  if (analysis == "main") {
    exclusions <- c(
      exclusions,
      shingles_before_threshold = "Shingles 28 days before threshold",
      exclude_past_vax = "Previous shingles vax",
      exclude_immunosuppressed = "Immunosuppressed",
      dementia_exclude_before_threshold = "History of dementia before threshold date",
      dementia_exclude_bw_threshold_index = "Dementia diagnosis between threshold and index dates"
    )
  }

  # Read data
  df <- read_feather(
    here::here(
      "output", vaccine_name, "processed",
      paste0("dataset_processed_", vaccine_name, "_", analysis, ".arrow")
    )
  ) %>%
    mutate(
      across(
        all_of(names(exclusions)),
        ~ coalesce(.x, FALSE)
      )
    )

  # Initialise flow chart
  flow_chart <- tibble(
    step = "Full population",
    excluded = NA_integer_,
    remaining = nrow(df)
  )

  # Apply exclusions sequentially
  for (variable in names(exclusions)) {

    n_before <- nrow(df)

    df <- df %>%
      filter(!.data[[variable]])

    flow_chart <- add_row(
      flow_chart,
      step = exclusions[[variable]],
      excluded = n_before - nrow(df),
      remaining = nrow(df)
    )
  }

  # Apply disclosure control
  flow_chart <- flow_chart %>%
    mutate(
      across(
        c(excluded, remaining),
        ~ sdc.rounding(replace_na(as.numeric(.x), 0), 5)
      )
    )

  # Export flow chart
  write_csv(
    flow_chart,
    here::here(
      "output", vaccine_name, "processed",
      paste0("flow_chart_", vaccine_name, "_", analysis, ".csv")
    )
  )

}