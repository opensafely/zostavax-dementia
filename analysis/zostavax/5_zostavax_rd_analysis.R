# Load libraries ----
library("arrow")
library("here")
library("glue")
library("magrittr")
library("dplyr")
library("readr")
library("checkmate")

# Specify command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  analysis_group_arg <- "primary"
} else {
  analysis_group_arg <- args[[1]]
}
assert_choice(
  analysis_group_arg,
  c(
    "primary",
    "alt_outcome",
    "alt_kernel",
    "alt_msebw",
    "alt_polynomial",
    "annual_fup",
    "excl_thresholdm",
    "fixed_bw1",
    "fixed_bw2",
    "reduced_excl",
    "sub_cogimp",
    "sub_sex"
  )
)

# Specify parameters ----
vaccine_name <- "zostavax"
analysis <- "main"
lcd <- as.Date("2025-09-02")
dob_threshold_date <- as.Date("1933-09-01")

# Create output directory ----
output_dir <- here("output", vaccine_name, "results")
fs::dir_create(output_dir)

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))
source(here::here("analysis", "common_code", "extract_rd_results.R"))
source(here::here("analysis", "common_code", "rd_analysis.R"))

# Load and filter analyses table ----
analyses <- read_csv("lib/zostavax.csv") |>
  filter(analysis_group == analysis_group_arg) |>
  mutate(analysis_id = row_number())

# Determine population to use ----
population <- unique(analyses$population)
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

# Load MSE-optimal bandwidth ----
rd_msebw <- read.csv("output/zostavax/setup/rd_msebw.csv")

# Load data ----
df_population <- read_feather(here::here(
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
))

# Run RD analysis ----
results <- vector("list", nrow(analyses))

for (i in seq_len(nrow(analyses))) {
  ## Make analysis ready dataset ----
  df_analysis <- df_population %>%
    select(
      patient_id,
      pat_end_date,
      month_diff_threshold,
      zostavax_date_1,
      analyses$outcome[i]
    )

  ## Define threshold ----
  threshold <- as.Date(
    paste0("01-09-", analyses$threshold_year[i]),
    format = "%d-%m-%Y"
  )

  ## Define end date ----
  if (!is.na(analyses$end_date_years[i])) {
    end <- threshold %m+% years(analyses$end_date_years[i])
  } else {
    end <- lcd
  }

  ## Define age bandwidth ----
  if (!is.na(analyses$msebw_multiplier[i])) {
    h_left <- analyses$msebw_multiplier[i] * rd_msebw$h_left
    h_right <- analyses$msebw_multiplier[i] * rd_msebw$h_right
  } else if (grepl("fixed_bw", analysis_group_arg)) {
    bw <- case_match(
      analysis_group_arg,
      "fixed_bw1" ~ 12,
      "fixed_bw2" ~ 24,
      .default = NA_real_
    )
    h_left <- bw
    h_right <- bw
  }

  ## Set design ----
  if (analysis_group_arg == "reduced_excl") {
    design <- "sharp"
  } else {
    design <- c("sharp", "fuzzy")
  }

  ## Run analysis ----
  rd <- rd_analysis(
    data = df_analysis,
    design = design,
    bandwidth = c(h_left, h_right),
    threshold = threshold,
    kernel = analyses$kernel[i],
    polynomial = analyses$polynomial[i],
    dependent = analyses$outcome[i],
    running = "month_diff_threshold",
    fuzzy_date = "zostavax_date_1",
    end = end
  )

  ## Record additional info ----
  rd <- rd |>
    mutate(
      analysis_id = analyses$analysis_id[i],
      running = "month_diff_threshold",
      fuzzy_date = "zostavax_date_1"
    )

  ## Record results ----
  results[[i]] <- rd
}

# Flatten results and record analyses info ----
all_results <- bind_rows(results)
output <- analyses |>
  left_join(all_results, by = "analysis_id") |>
  select(-analysis_id)

# Save output ----
write_csv(
  output,
  here::here(
    "output",
    vaccine_name,
    paste0("results/rd_results_", analysis_group_arg, ".csv")
  )
)
