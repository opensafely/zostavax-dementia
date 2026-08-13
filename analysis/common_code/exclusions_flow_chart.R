library(tidyverse)
library(lubridate)
library(arrow)
library(fs)
library(here)
library(data.table)

# list of exclusions:
# - immunosuppressed
# - past dementia
# - past shingles 
# - past shingles vaccine
# - missing IMD 
# - missing region
# - non-male/female sex

dir_create(here::here("output", "zostavax", "processed"), showWarnings = FALSE, recurse = TRUE)

exclusions <- function(df, threshold_date, index_date, vaccine_name, analysis) {
    
    flag_cols <- c(
      "exclude_not_mf_sex",
      "exclude_missing_imd",
      "exclude_missing_region",
      "exclude_immuno",
      "exclude_dementia_before_threshold",
      "exclude_dementia_before_index",
      "exclude_past_shingles",
      "exclude_past_vax",
      "exclude_threshold_any",
      "exclude_index_any",
      "exclude_none_threshold",
      "exclude_none_index"
    )
    
    file_with_exclusions <- df %>%
      mutate(

        #earliest date of outcomes
        dementia_first_date_ever = pmin(dementia_gp_first_date_ever, dementia_hosp_first_date_ever),
        dementia_exclude_first_date_ever = pmin(dementia_exclude_gp_first_date_ever, dementia_exclude_hosp_first_date_ever),
        shingles_first_date_after = pmin(shingles_gp_first_date_after, shingles_hosp_first_date_after),
        shingles_last_date_before = pmin(shingles_gp_last_date_before, shingles_hosp_last_date_before),
        neuralgia_first_date_ever = pmin(neuralgia_gp_first_date_ever, neuralgia_hosp_first_date_ever),

        exclude_not_mf_sex = !(sex %in% c("male", "female")),
        exclude_missing_imd = imd_quintile == "unknown",
        exclude_missing_region = is.na(region),
        exclude_immuno = immunosupp_gp_any_before,
        
        exclude_dementia_before_threshold =
          coalesce(
            dementia_exclude_first_date_ever <= threshold_date,
            FALSE
          ),
        
        exclude_dementia_before_index =
          coalesce(
            dementia_first_date_ever > threshold_date &
            dementia_first_date_ever <= index_date),
            FALSE
          ),
        
        exclude_past_shingles =
          coalesce(
            shingles_last_date_before >= (threshold_date - 28),
            FALSE
          ),
        
        exclude_shingles_before_index =
          coalesce(
            (shingles_first_date_after > threshold_date &
               shingles_first_date_after <= index_date),
            FALSE
          ),

        exclude_past_vax =
          coalesce(
            zostavax_date_1 < threshold_date |
              shingrix_date_1 < threshold_date,
            FALSE
          ),
        
        exclude_threshold_any = if_any(
          c(
            exclude_not_mf_sex,
            exclude_missing_imd,
            exclude_missing_region,
            exclude_dementia_before_threshold,
            exclude_past_shingles,
            exclude_past_vax
          ),
          identity
        ),
        
        exclude_none_threshold = !exclude_threshold_any,
        exclude_none_index = !exclude_threshold_any & !exclude_dementia_before_index,
        
        exclude_past_neuralgia =
          coalesce(
            neuralgia_first_date_ever <= threshold_date,
            FALSE
          ),
        
        exclude_neuralgia_before_index =
          coalesce(
            neuralgia_first_date_ever > threshold_date &
            neuralgia_first_date_ever <= index_date,
            FALSE
          ),
      )
    
    write_csv(
      file_with_exclusions,
      here::here(
        "output", vaccine_name, "processed",
        paste0("dataset_exclusions_", vaccine_name, "_", analysis, ".csv.gz")
      )
    )
    
    # Create table showing combinations of exclusion criteria for flow chart
    x <- file_with_exclusions %>%
      select(all_of(flag_cols)) %>%
      mutate(across(everything(), ~ coalesce(.x, FALSE))) %>%
      as.matrix()
    
    for_flow_chart <- as.data.frame(crossprod(x)) %>%
      tibble::rownames_to_column("exclusion") 
    
    write_csv(
      for_flow_chart,
      here::here("output", vaccine_name, paste0(vaccine_name, "_exclusions.csv"))
    )
    
    invisible(list(
      flow_chart = for_flow_chart
    ))
  }
  

exclusions(
  read_feather(here::here("analysis", "dummy_data", "dummy_dataset_zostavax.arrow")),
  as.Date("2013-09-01"), 
  as.Date("2014-02-01"),
  "zostavax",
  "main"
  )


mutate(across(contains("date"), as.Date)) %>%
mutate(
  month_birth_threshold = time_length(interval(date_column, threshold_date),"month"),
  dementia_first_date_ever = (
    pmin(dementia_gp_first_date_ever, dementia_hosp_first_date_ever, dementia_ons_date)
  ),
  alzheimers_first_date_ever = (
    pmin(alzheimers_gp_first_date_ever, alzheimers_hosp_first_date_ever, alzheimers_ons_date)
  ),
  vascular_first_date_ever = (
    pmin(vascular_gp_first_date_ever, vascular_hosp_first_date_ever, vascular_ons_date)
  ),
  neuralgia_first_date_ever = (
    pmin(neuralgia_gp_first_date_ever, neuralgia_hosp_first_date_ever)
  ),
)

select(!c(immunosupp_gp_any_before, dementia_hosp_first_date_ever, alzheimers_hosp_first_date_ever,
          dementia_gp_first_date_ever, alzheimers_gp_first_date_ever))
