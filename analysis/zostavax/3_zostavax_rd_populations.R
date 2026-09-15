# Load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")
library("checkmate")

# Specify arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  population <- "general"
} else {
  population <- args[[1]]
}

# Check arguments ---
assert_choice(
  population,
  c(
    "general",
    "reduced_excl",
    "aged78_81",
    "aged77_82",
    "women",
    "men",
    "cogimp_yes",
    "cogimp_no",
    "thresholdm_no"
  )
)

# Specify analysis parameters ----
## Analysis specific parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
lcd <- as.Date("2025-09-02")

## Define index_date specific dates ----
if (analysis == "main") {
  index_date <- as.Date("2013-09-01")
  dob_threshold_date <- as.Date("1933-09-01")
}

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))

# Create output directory ----
output_dir <- here("output", vaccine_name, "populations")
fs::dir_create(output_dir)

# Load data ----
df_processed <- read_feather(here(
  "output",
  vaccine_name,
  "processed",
  glue("dataset_processed_{vaccine_name}_{analysis}.arrow")
))

# Create population: general ----
if (population == "general") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE)
}

# Create population: reduced exclusions ----
if (population == "reduced_excl") {
  df_population <- df_processed |>
    filter(common_exclusions_wo_dementia == FALSE)
}

# Create population: aged 78 to 81 ----
if (population == "aged78_81") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(age >= 78 & age <= 81)
}

# Create population: aged 77 to 82 ----
if (population == "aged77_82") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(age >= 77 & age <= 82)
}

# Create population: women ----
if (population == "women") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(sex == "female")
}

# Create population: men ----
if (population == "men") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(sex == "male")
}

# Create population: evidence of cognitive impairment ----
if (population == "cogimp_yes") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(cognitive_impair_before_threshold == TRUE)
}

# Create population: no evidence of cognitive impairment ----
if (population == "cogimp_no") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    filter(cognitive_impair_before_threshold == FALSE)
}

# Create population: exclude people born in the threshold month ----
if (population == "thresholdm_no") {
  df_population <- df_processed |>
    filter(common_exclusions_w_dementia == FALSE) |>
    mutate(
      month_diff_threshold = as.integer(time_length(
        interval(dob_threshold_date, date_of_birth),
        unit = "months"
      ))
    ) |>
    filter(month_diff_threshold != 0)
}

# Make analysis ready dataset ----

df_analysis <- df_population %>%
  mutate(
    month_diff_threshold = as.integer(
      time_length(
        interval(dob_threshold_date, date_of_birth),
        unit = "months"
      )
    ),
    pat_end_date = pmin(lcd, date_of_death, reg_end_date, na.rm = TRUE)
  ) %>%
  select(
    patient_id,
    date_of_birth,
    pat_end_date,
    month_diff_threshold,
    zostavax_date_1,
    dementia_first_date_ever,
    shingles_first_date_after,
    neuralgia_first_date_ever,
    date_of_death,
    dementia_charlson_gp_first_date_ever,
    dementia_gp_first_date_ever,
    asthma_gp_first_date_ever,
    asthma_before_threshold,
    afib_gp_first_date_ever,
    afib_before_threshold,
    chd_gp_first_date_ever,
    chd_before_threshold,
    ckd_gp_first_date_ever,
    ckd_before_threshold,
    copd_gp_first_date_ever,
    copd_before_threshold,
    depression_gp_first_date_ever,
    depression_before_threshold,
    t2dm_gp_first_date_ever,
    t2dm_before_threshold,
    epilepsy_gp_first_date_ever,
    epilepsy_before_threshold,
    hf_gp_first_date_ever,
    hf_before_threshold,
    hypothyroid_gp_first_date_ever,
    hypothyroid_before_threshold,
    osteoporosis_gp_first_date_ever,
    osteoporosis_before_threshold,
    pad_gp_first_date_ever,
    pad_before_threshold,
    ra_gp_first_date_ever,
    ra_before_threshold,
    smi_gp_first_date_ever,
    smi_before_threshold,
    obese_gp_first_date_ever,
    obese_before_threshold,
    stroke_gp_first_date_after,
    tia_gp_first_date_after,
    alzheimers_first_date_ever,
    vascular_first_date_ever
  )

# Save ----
arrow::write_feather(
  df_analysis,
  here::here(
    "output",
    vaccine_name,
    "populations",
    paste0(
      "dataset_analysis_",
      vaccine_name,
      "_",
      analysis,
      "_",
      population,
      ".arrow"
    )
  ),
  compression = "zstd"
)
