library(dplyr)
library(readr)
library(arrow)
library(fs)
library(here)

# Create output directory
fs::dir_create(
  here::here("output", "zostavax", "processed"),
  recurse = TRUE
)

processing <- function(df, threshold_date, index_date, vaccine_name, analysis) {
  
  control_conditions <- c(
    "asthma", "afib", "chd", "ckd",
    "copd", "depression", "t2dm",
    "epilepsy", "hf", "hypothyroid",
    "smi", "obese", "osteoporosis",
    "pad", "ra", "stroke"
  )
  
  control_date_cols <- paste0(control_conditions, "_gp_first_date_ever")
  
  processed_df <- df %>%
    mutate(
      across(contains("date"), as.Date),
      
      # Earliest date of outcomes
      dementia_first_date_ever = pmin(
        dementia_gp_first_date_ever,
        dementia_hosp_first_date_ever,
        dementia_ons_date,
        na.rm = TRUE
      ),
      alzheimers_first_date_ever = pmin(
        alzheimers_gp_first_date_ever,
        alzheimers_hosp_first_date_ever,
        alzheimers_ons_date,
        na.rm = TRUE
      ),
      vascular_first_date_ever = pmin(
        vascular_gp_first_date_ever,
        vascular_hosp_first_date_ever,
        vascular_ons_date,
        na.rm = TRUE
      ),
      shingles_first_date_after = pmin(
        shingles_gp_first_date_after,
        shingles_hosp_first_date_after,
        na.rm = TRUE
      ),
      shingles_last_date_before = pmax(
        shingles_gp_last_date_before,
        shingles_hosp_last_date_before,
        na.rm = TRUE
      ),
      neuralgia_first_date_ever = pmin(
        neuralgia_gp_first_date_ever,
        neuralgia_hosp_first_date_ever,
        na.rm = TRUE
      ),
      
      # Missing data
      exclude_not_mf_sex = !sex %in% c("male", "female"),
      exclude_missing_imd = imd_quintile == "unknown",
      exclude_missing_region = is.na(region),
      
      # Previous immunosuppression / vaccination
      exclude_immunosuppressed = immunosupp_gp_any_before,
      exclude_past_vax = coalesce(
        zostavax_date_1 < threshold_date |
          shingrix_date_1 < threshold_date,
        FALSE
      ),
      
      # Outcomes before threshold
      dementia_before_threshold = coalesce(
        dementia_exclude_gp_first_date_ever <= threshold_date,
        FALSE
      ),
      dementia_bw_threshold_index = coalesce(
        dementia_first_date_ever > threshold_date &
          dementia_first_date_ever <= index_date,
        FALSE
      ),
      shingles_before_threshold = coalesce(
        shingles_last_date_before >= threshold_date - 28,
        FALSE
      ),
      neuralgia_before_threshold = coalesce(
          neuralgia_first_date_ever <= threshold_date,
        FALSE
      ),
      
      # Control conditions before threshold
      across(
        all_of(control_date_cols),
        ~ coalesce(.x <= threshold_date, FALSE),
        .names = "{.col}_before_threshold"
      ),
      
      # Vaccinations/prescriptions before threshold
      fluvax_before_threshold = coalesce(
        fluvax_last_date_before >= threshold_date - 1825
        & fluvax_last_date_before <= threshold_date,
        FALSE
      ),
      pneumovax_before_threshold = coalesce(
        pneumovax_last_date_before >= threshold_date - 1825
        & pneumovax_last_date_before <= threshold_date,
        FALSE
      ),
      statins_before_threshold = coalesce(
        statins_rx_last_date_before >= threshold_date - 1825
        & statins_rx_last_date_before <= threshold_date,
        FALSE
      ),
      antihypertensives_before_threshold = coalesce(
        antihypertensives_rx_last_date_before >= threshold_date - 1825
        & antihypertensives_rx_last_date_before <= threshold_date,
        FALSE
      ),
    ) %>%
    select(-c(immunosupp_gp_any_before,antihypertensives_rx_last_date_before,
              statins_rx_last_date_before,fluvax_last_date_before,pneumovax_last_date_before))
  
  write_csv(
    processed_df,
    here::here(
      "output", vaccine_name, "processed",
      paste0("dataset_", vaccine_name, "_", analysis, ".csv.gz")
    )
  )
}


df_main <- read_feather(here::here("output","zostavax","dataset_zostavax_main.arrow"))
processing(df_main, as.Date("2013-09-01"), as.Date("2014-02-01"), "zostavax","main")

df_2010 <- read_feather(here::here("output","zostavax","dataset_zostavax_2010.arrow"))
processing(df_2010, as.Date("2010-09-01"), as.Date("2011-02-01"), "zostavax","2010")

df_2016 <- read_feather(here::here("output","zostavax","dataset_zostavax_2016.arrow"))
processing(df_2016, as.Date("2016-09-01"), as.Date("2017-02-01"), "zostavax","2016")
