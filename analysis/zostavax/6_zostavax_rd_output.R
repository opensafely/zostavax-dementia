# Specify parameters ----
vaccine_name <- "zostavax"
output_cols <- c(
  "analysis_group",
  "population",
  "msebw_multiplier",
  "threshold_year",
  "end_date_years",
  "kernel",
  "polynomial",
  "outcome",
  "rd",
  "n_obs_left",
  "n_obs_right",
  "conv_coef",
  "conv_se",
  "conv_p",
  "bc_coef",
  "bc_se",
  "bc_p",
  "robust_coef",
  "robust_se",
  "robust_p",
  "stage1_conv_coef",
  "stage1_conv_se",
  "stage1_conv_p",
  "stage1_bc_coef",
  "stage1_bc_se",
  "stage1_bc_p",
  "stage1_robust_coef",
  "stage1_robust_se",
  "stage1_robust_p"
)

# Create output directory ----
output_dir <- here("output", vaccine_name, "output")
fs::dir_create(output_dir)

# Source functions ----
source(here::here("analysis", "common_code", "utility.R"))

# Load analyses ----
analyses <- read_csv(glue("lib/{vaccine_name}.csv"))
analysis_group <- unique(analyses$analysis_group)

# Combine results into list

results <- list()

for (i in analysis_group) {
  tmp <- read.csv(glue(
    "output/{vaccine_name}/results/rd_results_{i}.csv"
  ))
  tmp <- tmp[, output_cols]
  results[[i]] <- tmp
}

# Flatten results ----
all_results <- bind_rows(results)

# Perform disclosure control ----

all_results$n_obs_left_rounded <- sdc.rounding(
  all_results$n_obs_left,
  sdc.threshold
)
all_results$n_obs_right_rounded <- sdc.rounding(
  all_results$n_obs_right,
  sdc.threshold
)

# Save output ----
all_results$vaccine <- vaccine_name

write_csv(
  all_results,
  here::here(
    "output",
    vaccine_name,
    "output/rd_results.csv"
  )
)

write_csv(
  all_results[, !names(all_results) %in% c("n_obs_left", "n_obs_right")],
  here::here(
    "output",
    vaccine_name,
    "output/rd_results_rounded.csv"
  )
)
