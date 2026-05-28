library(tidyverse)


shingrix <- read_csv(here::here("output/shingrix/dataset_shingrix.csv.gz"),
    col_types = cols(
      dob = col_date(format = "%Y-%m-%d"),
      dod = col_date(format = "%Y-%m-%d"),
      reg_start_date = col_date(format = "%Y-%m-%d"),
      reg_end_date = col_date(format = "%Y-%m-%d"),
      dementia_outcome_gp_date =col_date(format = "%Y-%m-%d"),
      shingles_outcome_gp_date = col_date(format = "%Y-%m-%d"),
      neuralgia_outcome_gp_date = col_date(format = "%Y-%m-%d"),
      shingrix_date_1 = col_date(format = "%Y-%m-%d"),
      shingrix_date_2 = col_date(format = "%Y-%m-%d")
  ))


