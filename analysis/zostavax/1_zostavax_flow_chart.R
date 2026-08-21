library(tidyverse)

source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "exclusions_flow_chart.R"))

exclusions_zostavax_main <- exclusion_flow_chart(
  "zostavax",
  "main",
  c("exclude_not_mf_sex",
    "exclude_missing_imd",
    "exclude_missing_region",
    "shingles_before_threshold",
    "dementia_exclude_before_threshold",
    "dementia_exclude_bw_threshold_index",
    "exclude_past_vax",
    "exclude_immunosuppressed",
    "common_exclusions_w_dementia",
    "common_exclusions_wo_dementia",
    "no_common_exclusions")
)

exclusions_zostavax_2010 <- exclusion_flow_chart(
  "zostavax",
  "2010",
  c("exclude_not_mf_sex",
    "exclude_missing_imd",
    "exclude_missing_region",
    "shingles_before_threshold",
    "dementia_exclude_before_threshold",
    "dementia_exclude_bw_threshold_index",
    "exclude_past_vax",
    "exclude_immunosuppressed",
    "common_exclusions_w_dementia",
    "common_exclusions_wo_dementia",
    "no_common_exclusions")
)

exclusions_zostavax_2016 <- exclusion_flow_chart(
  "zostavax",
  "2016",
  c("exclude_not_mf_sex",
    "exclude_missing_imd",
    "exclude_missing_region",
    "shingles_before_threshold",
    "dementia_exclude_before_threshold",
    "dementia_exclude_bw_threshold_index",
    "exclude_past_vax",
    "exclude_immunosuppressed",
    "common_exclusions_w_dementia",
    "common_exclusions_wo_dementia",
    "no_common_exclusions")
)
