# Load libraries ----
library("arrow")
library("here")
library("glue")

# Source functions ----
source(here::here("analysis", "common_code", "rd_input_checks.R"))
source(here::here("analysis", "common_code", "rd_analysis.R"))

# Specify arguments ----
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  analysis_group <- "fixed_bw2"
} else {
  analysis_group <- args[[1]]
}

# Specify analysis parameters ----
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016
lcd <- as.Date("2025-09-02")
dob_threshold_date <- as.Date("1933-09-01")

# Load and filter analyses ----
analyses <- read_csv("lib/zostavax.csv")
analyses <- analyses[analyses$analysis_group == analysis_group, ]
population <- unique(analyses$population)

# Check population ----
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
result <- NULL

for (i in 1:nrow(analyses)) {
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
  } else if (grepl("fixed_bw", analysis_group)) {
    bw <- case_match(
      analysis_group,
      "fixed_bw1" ~ 12,
      "fixed_bw2" ~ 24,
      .default = NA_real_
    )
    h_left <- bw
    h_right <- bw
  }
  ## Run analysis ----
  rd <- rd_analysis(
    data = df_analysis,
    design = analyses$design[i],
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
  rd <- rd %>%
    mutate(
      analysis_group = analyses$analysis_group[i],
      population = analyses$population[i],
      design = analyses$design[i],
      msebw_multiplier = analyses$msebw_multiplier[i],
      threshold_year = analyses$threshold_year[i],
      end_date_years = analyses$end_date_years[i],
      outcome = analyses$outcome[i],
      running = "month_diff_threshold",
      fuzzy_date = "zostavax_date_1"
    )

  ## Record results ----
  result <- rbind(result, rd)
}
