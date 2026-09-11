# Load libraries ----
library("arrow")
library("here")
library("glue")

# Specify analysis parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
lcd <- as.Date("2025-09-02")
threshold <- as.Date("2013-09-01")
dob_threshold_date <- as.Date("1933-09-01")
primary_outcomes <- c(
  "dementia_first_date_ever",
  "shingles_first_date_after",
  "neuralgia_first_date_ever",
  "date_of_death"
)

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))
source(here::here("analysis", "common_code", "rd_analysis.R"))

# Load data ----
df_analysis <- read_feather(here(
  "output",
  vaccine_name,
  "setup",
  glue("dataset_analysis_{vaccine_name}_{analysis}.arrow")
))

# Load MSE-optimal bandwidth ----
rd_msebw <- read.csv("output/zostavax/setup/rd_msebw.csv")

# Run RD analysis ----
result <- NULL
for (dependent in primary_outcomes) {
  rd <- rd_analysis(
    data = df_analysis,
    design = c("sharp", "fuzzy"),
    bandwidth = c(rd_msebw$h_left, rd_msebw$h_right),
    threshold = threshold,
    kernel = "triangular",
    polynomial = 1,
    dependent = dependent,
    running = "month_diff_threshold",
    fuzzy_date = "zostavax_date_1",
    end = lcd
  )
  rd$dependent <- dependent
  rd$running <- "month_diff_threshold"
  rd$fuzzy_date <- "zostavax_date_1"
  result <- rbind(result, rd)
}
