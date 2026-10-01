# Load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")

# Specify parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
population <- "general"
lcd <- as.Date("2025-09-02")
msebw_outcome <- "dementia_first_date_ever"
if (analysis == "main") {
  index_date <- as.Date("2013-09-01")
  dob_threshold_date <- as.Date("1933-09-01")
}

# Create output directory ----
output_dir <- here("output", vaccine_name, "setup")
fs::dir_create(output_dir)

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))
source(here::here("analysis", "common_code", "rd_msebw.R"))

# Load data ----
df_population <- open_dataset(
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
  format = "ipc"
) |>
  select(
    patient_id,
    pat_end_date,
    month_diff_threshold,
    glue("{vaccine_name}_date_1"),
    msebw_outcome
  ) |>
  collect()

## Define end date ----
end <- lcd

# Perform bandwidth selection ----
rd_msebw <- rd_msebw(
  data = df_population,
  dependent = msebw_outcome,
  running = "month_diff_threshold",
  end = end
)

# Save ----
write_csv(
  rd_msebw,
  here::here(
    "output",
    vaccine_name,
    "setup/rd_msebw.csv"
  )
)
