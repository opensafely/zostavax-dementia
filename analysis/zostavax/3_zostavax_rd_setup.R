# Load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")

# Specify analysis parameters ----
## Analysis specific parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
## Define index_date specific dates ----
if (analysis == "main") {
  index_date <- as.Date("2013-09-01")
  dob_threshold_date <- as.Date("1933-09-01")
}

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))
source(here::here("analysis", "common_code", "rd_msebw.R"))

# Create output directory ----
output_dir <- here("output", vaccine_name, "setup")
fs::dir_create(output_dir)

# Load data ----
df_processed <- read_feather(here(
  "output",
  vaccine_name,
  "processed",
  glue("dataset_processed_{vaccine_name}_{analysis}.arrow")
))

# Create core analysis dataset with exclusions applied ----
df_analysis <-
  df_processed |>
  filter(no_common_exclusions) |>
  mutate(
    eligibility = .data[[glue("zostavax_eligibility_{analysis}")]],
    month_of_birth = floor_date(date_of_birth, "month"), # this should be an identity because currently in OS birth dates are rounded down to 1st of the month
    month_diff_threshold = as.integer(time_length(
      interval(dob_threshold_date, date_of_birth),
      unit = "months"
    )),
    day_diff_threshold = as.integer(date_of_birth - dob_threshold_date),
    registration_duration = as.numeric(index_date - reg_start_date + 1),
    censor_date = pmin(reg_end_date, date_of_death, na.rm = TRUE)
  )

# Perform bandwidth selection ----
rd_msebw <- rd_msebw(
  data = df_analysis,
  dependent = "dementia_first_date_ever",
  running = "month_diff_threshold"
)

# Save ----
## Analysis dataset ----
arrow::write_feather(
  df_analysis,
  here::here(
    "output",
    vaccine_name,
    "setup",
    paste0("dataset_analysis_", vaccine_name, "_", analysis, ".arrow")
  ),
  compression = "zstd"
)
## Bandwidth ----
write_csv(
  rd_msebw,
  here::here(
    "output",
    vaccine_name,
    "setup/rd_msebw.csv"
  )
)
