library(tidyverse)
library(readr)
library(arrow)
library(fs)
library(here)

exclusion_flow_chart <- function(vaccine_name, analysis){
  
  df <- read_feather(
    here::here("output", vaccine_name, "processed",
               paste0("dataset_processed_", vaccine_name, "_", analysis, ".arrow")
    )
  )
  
  flow_chart <- tibble(
    step = "Full population",
    excluded = NA_integer_,
    remaining = nrow(df)
  )
  
  ### Not M/F sex ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!exclude_not_mf_sex)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Non male/female sex",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Missing IMD ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!exclude_missing_imd)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Missing IMD",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Missing region ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!exclude_missing_region)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Missing region",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Recent shingles ###
  n_before <- nrow(df)

  df <- df %>%
    filter(!shingles_before_threshold)

  flow_chart <- add_row(
    flow_chart,
    step = "Shingles 28 days before threshold",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )

  ### Previous shingles vaccine ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!exclude_past_vax)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Previous shingles vax",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Immunosuppressed ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!exclude_immunosuppressed)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Immunosuppressed",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Dementia before threshold date ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!dementia_exclude_before_threshold)
  
  flow_chart <- add_row(
    flow_chart,
    step = "History of dementia before threshold date",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  ### Dementia between threshold date an index date ###
  n_before <- nrow(df)
  
  df <- df %>%
    filter(!dementia_exclude_bw_threshold_index)
  
  flow_chart <- add_row(
    flow_chart,
    step = "Dementia diagnosis between threshold and index dates",
    excluded = n_before - nrow(df),
    remaining = nrow(df)
  )
  
  # View flow_chart
  flow_chart <- flow_chart %>%
        mutate(
          
          excluded = as.numeric(excluded),
          remaining = as.numeric(remaining), 
          
          # replace NA with zeroes
          across(
            c(excluded, remaining),
            ~ replace_na(.x, 0)
          ),
          
          # Apply disclosure control
          across(
            c(excluded, remaining),
            ~ sdc.rounding(.x, 5)
          )
        )
      
  write_csv(
    flow_chart,
    here::here(
      "output", vaccine_name, "processed",
      paste0("flow_chart_", vaccine_name, "_", analysis, ".csv")
    )
  )

}