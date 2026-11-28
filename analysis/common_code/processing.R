library(dplyr)
library(readr)
library(arrow)
library(fs)
library(here)
library(lubridate)

source(here::here("analysis", "common_code", "utility.R"))

processing <- function(threshold_date, index_date, vaccine_name, analysis) {
  
  chronic_conditions <- c(
    "dementia", "alzheimers", "vascular", 
    "asthma", "afib", "chd", "ckd", "copd", "depression", 
    "t2dm", "epilepsy", "hf", "hypothyroid", 
    "smi", "osteoporosis", "pad", "ra", "obese"
  )

  recurrent_conditions <- c(
    "stroke", "tia"
  )

  df <- read_feather(
    here::here("output", vaccine_name, paste0("dataset_",vaccine_name,"_",analysis,".arrow"))
    )
  
  processed_df <- df %>%
    
    mutate(
      across(contains("date"), as.Date),

      # vaccine eligibility, according to date / age threshold
      zostavax_eligibility_20130901 = between(
        date_of_birth,
        as.Date("1933-09-02"),
        as.Date("1934-09-01")
      ) |
        between(date_of_birth, as.Date("1942-09-02"), as.Date("1943-09-01")),
      zostavax_eligibility_20140901 = between(
        date_of_birth,
        as.Date("1934-09-02"),
        as.Date("1936-09-01")
      ) |
        zostavax_eligibility_20130901,

      zostavax_eligibility_main = zostavax_eligibility_20130901,
      zostavax_eligibility_2010 = between(
        date_of_birth,
        as.Date("1933-09-02") + years(-3),
        as.Date("1934-09-01") + years(-3)
      ) |
        between(
          date_of_birth,
          as.Date("1942-09-02") + years(-3),
          as.Date("1943-09-01") + years(-3)
        ),
      zostavax_eligibility_2016 = between(
        date_of_birth,
        as.Date("1933-09-02") + years(3),
        as.Date("1934-09-01") + years(3)
      ) |
        between(
          date_of_birth,
          as.Date("1942-09-02") + years(3),
          as.Date("1943-09-01") + years(3)
        ),

      # Earliest date of outcomes
      !!!rlang::set_names(
          lapply(chronic_conditions, function(x) first_date_ever(df, x)),
          paste0(chronic_conditions, "_first_date_ever")
        ),

      # Create flag if first date is before threshold 
      across(
        all_of(paste0(chronic_conditions, "_first_date_ever")),
        ~ !is.na(.x) & .x < threshold_date,
        .names = "{sub('_first_date_ever$', '_before_threshold', .col)}"
        ),
      
      # Create flag if first date is before threshold for recurrent conditions
      !!!rlang::set_names(
          lapply(recurrent_conditions, function(x) last_date_before(df, x)),
          paste0(recurrent_conditions, "_last_date_before")
        ),

      across(
        all_of(paste0(recurrent_conditions, "_last_date_before")),
        ~ !is.na(.x) & .x < threshold_date,
        .names = "{sub('_last_date_before$', '_before_threshold', .col)}"
        ),
      
      # First date after
      !!!rlang::set_names(
          lapply(recurrent_conditions, function(x) first_date_after(df, x)),
          paste0(recurrent_conditions, "_first_date_after")
        ),

      dementia_charlson_first_date_ever = pmin(
        dementia_charlson_gp_first_date_ever,
        dementia_hosp_first_date_ever,
        dementia_ons_date,
        na.rm = TRUE
      ),
      dementia_exclude_first_date_ever = pmin(
        dementia_exclude_gp_first_date_ever,
        dementia_hosp_first_date_ever,
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
      shingles_before_threshold = coalesce(
        (shingles_last_date_before <= threshold_date),
        na.rm = TRUE
      ),
      exclude_shingles = coalesce(
        shingles_last_date_before >= threshold_date - 28,
        FALSE
      ),
      neuralgia_first_date_after = pmin(
        neuralgia_gp_first_date_after,
        neuralgia_hosp_first_date_after,
        na.rm = TRUE
      ),
      neuralgia_before_threshold = coalesce(
        (neuralgia_gp_last_date_before <= threshold_date)
        | (neuralgia_hosp_last_date_before <= threshold_date),
        na.rm = TRUE
      ),
      cognitive_impair_before_threshold = coalesce(
        (cognitive_impair_gp_last_date_before <= threshold_date)
        | (cognitive_impair_hosp_last_date_before <= threshold_date),
        na.rm = TRUE
      ),

      # Vaccinations/prescriptions 5 years before threshold
      fluvax_before_threshold = coalesce(
        fluvax_last_date_before >= threshold_date - years(5) &
          fluvax_last_date_before <= threshold_date,
        FALSE
      ),
      pneumovax_before_threshold = coalesce(
        pneumovax_last_date_before >= threshold_date - years(5) &
          pneumovax_last_date_before <= threshold_date,
        FALSE
      ),
      statins_before_threshold = coalesce(
        statins_rx_last_date_before >= threshold_date - years(5) &
          statins_rx_last_date_before <= threshold_date,
        FALSE
      ),
      antihypertensives_before_threshold = coalesce(
        antihypertensives_rx_last_date_before >= threshold_date - years(5) &
          antihypertensives_rx_last_date_before <= threshold_date,
        FALSE
      ),
      smoker_before_threshold = coalesce(
        smoker_gp_last_date_before <= threshold_date |
          past_smoker_gp_last_date_before <= threshold_date,
        FALSE
      ),

      # Missing data
      exclude_not_mf_sex = coalesce(!sex %in% c("male", "female"), FALSE),
      exclude_missing_imd = coalesce(imd_quintile == "unknown" | is.na(imd_quintile), FALSE),
      exclude_missing_region = coalesce(is.na(region), FALSE),

      # Previous immunosuppression / vaccination
      exclude_immunosuppressed = coalesce(immunosupp_gp_any_before, FALSE),
      exclude_past_vax = coalesce(
        zostavax_date_1 < threshold_date |
          shingrix_date_1 < threshold_date |
            shvacgp_first_date_ever < threshold_date,
        FALSE
      ),

      # Outcomes before threshold
      dementia_exclude_before_threshold = coalesce(
        dementia_exclude_first_date_ever <= threshold_date |
        dementia_hosp_first_date_ever <= threshold_date,
        FALSE
      ),
      dementia_exclude_bw_threshold_index = coalesce(
        (dementia_exclude_first_date_ever > threshold_date & 
          dementia_exclude_first_date_ever <= index_date) |
        (dementia_hosp_first_date_ever > threshold_date &
          dementia_hosp_first_date_ever <= index_date),
        FALSE
      ),

      # Died / deregistered between threshold and index date
      exclude_died = coalesce(!is.na(date_of_death) & (date_of_death <= index_date), FALSE),
      exclude_deregister = coalesce(!is.na(reg_end_date) & (reg_end_date <= index_date), FALSE),

      # Flag for exclusions common to all analyses
      common_exclusions_w_dementia = coalesce(
        exclude_shingles |
          dementia_exclude_before_threshold |
          dementia_exclude_bw_threshold_index |
          exclude_died |
          exclude_deregister |
          exclude_not_mf_sex |
          exclude_past_vax |
          exclude_immunosuppressed |
          exclude_missing_imd |
          exclude_missing_region,
        FALSE
      ),
      common_exclusions_wo_dementia = coalesce(
          exclude_shingles |
          exclude_immunosuppressed |
          exclude_missing_imd |
          exclude_missing_region |
          exclude_not_mf_sex |
          exclude_past_vax |
          exclude_deregister |
          exclude_died, 
        FALSE
      ),
      no_common_exclusions = coalesce(
        !exclude_shingles &
          !dementia_exclude_before_threshold &
          !dementia_exclude_bw_threshold_index &
          !exclude_not_mf_sex &
          !exclude_past_vax &
          !exclude_immunosuppressed &
          !exclude_missing_imd &
          !exclude_missing_region &
          !exclude_died &
          !exclude_deregister,
        FALSE
      ),
      no_exclusions_control = coalesce(
          !exclude_not_mf_sex &
          !exclude_missing_imd &
          !exclude_missing_region &
          !exclude_died &
          !exclude_deregister,
        FALSE
      ),

    ) %>%
    select(
      -c(
        immunosupp_gp_any_before,
        antihypertensives_rx_last_date_before,
        statins_rx_last_date_before,
        fluvax_last_date_before,
        pneumovax_last_date_before,
        shingles_gp_last_date_before,
        neuralgia_gp_last_date_before,
        alzheimers_gp_first_date_ever,
        alzheimers_hosp_first_date_ever,
        alzheimers_ons_date,
        vascular_gp_first_date_ever,
        vascular_hosp_first_date_ever,
        vascular_ons_date,
        dementia_exclude_gp_first_date_ever,
        dementia_hosp_first_date_ever,
        dementia_ons_date,
        neuralgia_hosp_first_date_after,
        neuralgia_hosp_last_date_before,
        shingles_hosp_first_date_after,
        shingles_hosp_last_date_before,
        lrti_gp_last_date_before,
        smoker_gp_last_date_before,
        past_smoker_gp_last_date_before,
        cognitive_impair_gp_last_date_before,
        cognitive_impair_hosp_last_date_before,
        dementia_exclude_first_date_ever,
        stroke_gp_last_date_before,
        tia_gp_last_date_before,
        stroke_hosp_last_date_before,
        tia_hosp_last_date_before,
        stroke_gp_first_date_after,
        tia_gp_first_date_after,
        stroke_hosp_first_date_after,
        tia_hosp_first_date_after,
        stroke_ons_date,
        tia_ons_date,
        asthma_gp_first_date_ever,
        asthma_hosp_first_date_ever,
        asthma_ons_date,
        afib_gp_first_date_ever,
        afib_hosp_first_date_ever,
        afib_ons_date,
        chd_gp_first_date_ever,
        chd_hosp_first_date_ever,
        chd_ons_date,
        ckd_gp_first_date_ever,
        ckd_hosp_first_date_ever,
        ckd_ons_date,
        copd_gp_first_date_ever,
        copd_hosp_first_date_ever,
        copd_ons_date,
        depression_gp_first_date_ever,
        depression_hosp_first_date_ever,
        depression_ons_date,
        t2dm_gp_first_date_ever,
        t2dm_hosp_first_date_ever,
        t2dm_ons_date,
        epilepsy_gp_first_date_ever,
        epilepsy_hosp_first_date_ever,
        epilepsy_ons_date,
        hf_gp_first_date_ever,
        hf_hosp_first_date_ever,
        hf_ons_date,
        hypothyroid_gp_first_date_ever,
        hypothyroid_hosp_first_date_ever,
        hypothyroid_ons_date,
        smi_gp_first_date_ever,
        smi_hosp_first_date_ever,
        smi_ons_date,
        osteoporosis_gp_first_date_ever,
        osteoporosis_hosp_first_date_ever,
        osteoporosis_ons_date,
        pad_gp_first_date_ever,
        pad_hosp_first_date_ever,
        pad_ons_date,
        ra_gp_first_date_ever,
        ra_hosp_first_date_ever,
        ra_ons_date,
        obese_gp_first_date_ever,
        obese_hosp_first_date_ever,
        obese_ons_date,
      )
    ) 

  arrow::write_feather(
    processed_df,
    here::here(
      "output", vaccine_name, "processed",
      paste0("dataset_processed_", vaccine_name, "_", analysis, ".arrow")
      ),
    compression="zstd"
    )

    return(processed_df)
  
}