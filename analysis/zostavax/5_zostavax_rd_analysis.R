# Load libraries ----
library("arrow")
library("here")
library("glue")
library("magrittr")
library("dplyr")
library("readr")
library("checkmate")
library("lubridate")

# Specify command-line arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  population_arg <- "general"
  analysis_group_arg <- "primary"
} else {
  population_arg <- args[[1]]
  analysis_group_arg <- args[[2]]
}

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

# Check arguments ----
assert_choice(
  population_arg,
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

assert_choice(
  analysis_group_arg,
  c(
    "primary",
    "alt_outcome",
    "alt_kernel",
    "alt_msebw",
    "alt_polynomial",
    "annual_fup",
    "reduced_excl",
    "fixed_bw_aged78_81",
    "fixed_bw_aged77_82",
    "sub_sex_women",
    "sub_sex_men",
    "sub_cogimp_yes",
    "sub_cogimp_no",
    "thresholdm_no"
  )
)

# Load and filter analyses table ----
analyses <- read_csv(glue("lib/{vaccine_name}.csv")) |>
  filter(analysis_group == analysis_group_arg) |>
  mutate(analysis_id = row_number())

# Load MSE-optimal bandwidth ----
rd_msebw <- read.csv(glue("output/{vaccine_name}/setup/rd_msebw.csv"))

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
    population_arg,
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
      glue("{vaccine_name}_date_1"),
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
      "fixed_bw_aged78_81" ~ 12,
      "fixed_bw_aged77_82" ~ 24,
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
    fuzzy_date = glue("{vaccine_name}_date_1"),
    end = end
  )

  ## Record additional info ----
  rd <- rd |>
    mutate(
      analysis_id = analyses$analysis_id[i],
      running = "month_diff_threshold",
      fuzzy_date = glue("{vaccine_name}_date_1")
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
    glue("results/rd_results_{analysis_group_arg}.csv")
  )
)
