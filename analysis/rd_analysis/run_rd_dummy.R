# Load libraries ----
library(checkmate)
library(data.table)
library(lubridate)
library(rdrobust)

# Source functions ----
files <- list.files(path = "analysis", pattern = "fn-*", full.names = TRUE)
invisible(lapply(files, source))

# Specify parameters ----
threshold <- as.Date("2013-09-01")

# Make dummy data ----
data <- dummy_data(
  threshold = threshold,
  n = 100000,
  p_elig = 0.5,
  p_vax = 0.6,
  p_out_vax = 0.2,
  p_out_unvax = 0.3
)

# Get MSE-optimal bandwidth ----
mse <- rd_msebw(data = data, dependent = "out_date", running = "elig_run")

# Run RD analysis with fixed 2 year bw ----
rd_fix <- rd_analysis(
  data = data,
  design = c("sharp", "fuzzy"),
  bandwidth = c(24, 24),
  threshold = threshold,
  kernel = "triangular",
  polynomial = 1,
  dependent = "out_date",
  running = "elig_run",
  fuzzy_date = "vax_date",
  end = threshold %m+% years(1)
)

# Run RD analysis with mse-optimal bw ----
rd_mse <- rd_analysis(
  data = data,
  design = c("sharp", "fuzzy"),
  bandwidth = c(mse$h_left, mse$h_right),
  threshold = threshold,
  kernel = "triangular",
  polynomial = 1,
  dependent = "out_date",
  running = "elig_run",
  fuzzy_date = "vax_date",
  end = threshold %m+% years(1)
)
