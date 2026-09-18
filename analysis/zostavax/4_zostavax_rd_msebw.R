# Load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")

# Specify parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
population <- "general"
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
) %>%
  select(
    patient_id,
    pat_end_date,
    month_diff_threshold,
    zostavax_date_1,
    dementia_first_date_ever
  ) %>%
  collect()

# Remove dementia events that occur after patient end date ---

df_analysis <- df_population %>%
  mutate(
    dementia_first_date_ever = if_else(
      dementia_first_date_ever > pat_end_date,
      as.Date(NA),
      dementia_first_date_ever
    )
  )

# Perform bandwidth selection ----
rd_msebw <- rd_msebw(
  data = df_analysis,
  dependent = "dementia_first_date_ever",
  running = "month_diff_threshold"
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
