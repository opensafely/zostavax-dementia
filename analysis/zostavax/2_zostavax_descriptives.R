# load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")
library("survival")

# Specify analysis parameters ----

# analysis specific parameters
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016

# ensure skim outputs do not wrap to new line
options(width = 200)

# define threshold dates
if (analysis == "main") {
  dob_threshold_date <- as.Date("1933-09-01")
  threshold_date <- as.Date("2013-09-01")
  end_date <- threshold_date + years(12) - days(1)
}

if (analysis == "2010") {
  dob_threshold_date <- as.Date("1933-09-01") + years(-3)
  threshold_date <- as.Date("2013-09-01") + years(-3)
  end_date <- threshold_date + years(12) - days(1)
}

if (analysis == "2016") {
  dob_threshold_date <- as.Date("1933-09-01") + years(3)
  threshold_date <- as.Date("2013-09-01") + years(3)
  end_date <- threshold_date + years(12) - days(1)
}

index_date <- threshold_date + months(5)

# run functions ----
source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "descriptives.R"))

# create output directory ----
output_dir <- here("output", vaccine_name, "descriptives", analysis)
fs::dir_create(output_dir)

# read and prepare data ----
df_processed <- read_feather(here(
  "output",
  vaccine_name,
  "processed",
  glue("dataset_processed_{vaccine_name}_{analysis}.arrow")
))

# add some variables convenient for descriptives
df_processed <-
  df_processed |>
  mutate(
    age = as.numeric(age),
    age_fct = as.factor(age),
    eligibility = .data[[glue("zostavax_eligibility_{analysis}")]],
    month_of_birth = floor_date(date_of_birth, "month"), # this should be an identity because currently in OS birth dates are rounded down to 1st of the month
    month_of_birth_fct = fct_reorder(
      scales::label_date(format = "%b '%y")(month_of_birth),
      month_of_birth
    ), # convert to factor
    month_diff_threshold = as.integer(time_length(
      interval(dob_threshold_date, date_of_birth),
      unit = "months"
    )),
    day_diff_threshold = as.integer(date_of_birth - dob_threshold_date),
    registration_duration = as.integer(threshold_date - reg_start_date + 1L),
    censor_date = pmin(reg_end_date, date_of_death, na.rm = TRUE),
    fup_availability = as.numeric(
      pmin(censor_date, end_date, na.rm = TRUE) - index_date + 1L
    )
  )

# print summary of processed dataset - not disclosure controlled, not for release
capture.output(
  skimr::skim_without_charts(df_processed),
  file = fs::path(output_dir, "df_processed_skim.txt"),
  split = FALSE
)

# create core analysis dataset with exclusions applied
df_analysis <-
  df_processed |> 
  filter(no_common_exclusions)

# print summary of analysis dataset - not disclosure controlled, not for release
capture.output(
  skimr::skim_without_charts(df_analysis),
  file = fs::path(output_dir, "df_analysis_skim.txt"),
  split = FALSE
)

# create dataset same as analysis dataset but without excluding prior dementia
df_analysis_keep_prior_dementia <-
  df_processed |> 
  filter(!common_exclusions_wo_dementia)

# many variables names are as follows:
# - [name]_first_date_ever - for outcomes where we are only interested in the very first event (i.e. exclude people with the event before threshold);
# - [name]_first_date_after - for recurrent outcomes where we don't care about events before threshold;
# - [name]_before_threshold - T/F columns which indicates whether an individual had the event/diagnosis before the threshold date (either ever or a set time period, depending on protocol)

# look-up list for nice variable names
variable_labels <-
  lst(
    # Total numbers
    N = "Total N",

    # primary exposure
    eligibility = "Eligibility",

    # demographics
    age = "Age",
    age_fct = "Age (cat)",
    sex = "Sex",
    ethnicity6 = "Ethnicity",
    imd_quintile = "Deprivation (by IMD quintile)",
    region = "Region",
    registration_duration = "GP registration duration",

    #prior outcomes
    shingles_before_threshold = "Prior Shingles",
    neuralgia_before_threshold = "Prior Neuralgia",

    # prior disease
    dementia_exclude_before_threshold = "Dementia",
    asthma_before_threshold = "Asthma",
    afib_before_threshold = "Atrial fibrillation",
    chd_before_threshold = "Coronary heart disease",
    ckd_before_threshold = "Chronic kidney disease",
    copd_before_threshold = "Chronic obstructive pulmonary disease",
    depression_before_threshold = "Depression",
    t2dm_before_threshold = "Type 2 diabetes",
    epilepsy_before_threshold = "Epilepsy",
    hf_before_threshold = "Heart Failure",
    hypothyroid_before_threshold = "Hypothyroidism",
    osteoporosis_before_threshold = "Osteoporosis",
    pad_before_threshold = "peripheral artery disease",
    ra_before_threshold = "rheumatoid arthritis",
    stroke_before_threshold = "Stroke",
    tia_before_threshold = "Transient ischaemic attack",
    smi_before_threshold = "Serious mental illness",
    obese_before_threshold = "Obesity",
    cognitive_impair_before_threshold = "Cognitive impairment",

    # prior healthcare
    statins_before_threshold = "Statin use",
    pneumovax_before_threshold = "Pneumococcal vaccination",
    fluvax_before_threshold = "Influenza vaccination",
    antihypertensives_before_threshold = "Antihypertensive use"
  )


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Baseline pre-outcome and covariate balance ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# create a table of baseline differences between exposed / not exposed
# using existing function based on gtsummary package but which also does redaction

table_balance <-
  df_analysis |>
  mutate(
    N = 1L,
  ) %>%
  table1_summary(
    group = eligibility,
    label = variable_labels,
    threshold = sdc.threshold
  )

# reformat to wide
table_balance_wide <-
  table_balance |>
  filter(!is.na(group1_level)) |>
  pivot_wider(
    id_cols = c(variable, variable_label, variable_level, context),
    names_from = group1_level,
    names_sep = "_",
    values_from = c(n, N, p, mean, sd, p10, p25, median, p75, p90),
    names_vary = "slowest"
  )

# save to disk
write_csv(table_balance_wide, fs::path(output_dir, "table_balance.csv"))


# as table of baseline differences above, but INCLUDING people who may have prior dementia diagnosis
table_balance_keep_prior_dementia <-
  df_analysis_keep_prior_dementia |>
  mutate(
    N = 1L,
  ) %>%
  table1_summary(
    group = eligibility,
    label = variable_labels,
    threshold = sdc.threshold
  )

# reformat to wide
table_balance_keep_prior_dementia_wide <-
  table_balance_keep_prior_dementia |>
  filter(!is.na(group1_level)) |>
  pivot_wider(
    id_cols = c(variable, variable_label, variable_level, context),
    names_from = group1_level,
    names_sep = "_",
    values_from = c(n, N, p, mean, sd, p10, p25, median, p75, p90),
    names_vary = "slowest"
  )

# save to disk
write_csv(
  table_balance_keep_prior_dementia_wide,
  fs::path(output_dir, "table_balance_keep_prior_dementia.csv")
)


# create a table of follow-up availability (time to deregistration, death or end of follow up) after index date
table_followup <-
  df_analysis |>
  group_by(age_fct) |>
  summarise(
    N = n(),
    mean = mean(fup_availability),
    sd = sd(fup_availability),
    p10 = quantile(fup_availability, 0.1),
    p25 = quantile(fup_availability, 0.25),
    median = quantile(fup_availability, 0.5),
    p75 = quantile(fup_availability, 0.75),
    p90 = quantile(fup_availability, 0.90),
  ) |>
  mutate(
    N = sdc.rounding(N, sdc.threshold),
    mean = if_else(N == 0, NA_real_, mean),
    sd = if_else(N == 0, NA_real_, sd),
    p10 = if_else(N == 0, NA_real_, p10),
    p25 = if_else(N == 0, NA_real_, p25),
    median = if_else(N == 0, NA_real_, median),
    p75 = if_else(N == 0, NA_real_, p75),
    p90 = if_else(N == 0, NA_real_, p90)
  )

# save to disk
write_csv(table_followup, fs::path(output_dir, "table_followup.csv"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Check pre-index date discontinuities: report rate of event X by month of birth ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "shingles_before_threshold", "Prior Shingles")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "asthma_before_threshold", "Asthma")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "afib_before_threshold", "Atrial fibrillation")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "chd_before_threshold", "Coronary heart disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "ckd_before_threshold", "Chronic kidney disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "copd_before_threshold", "Chronic obstructive pulmonary disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "depression_before_threshold", "Depression")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "t2dm_before_threshold", "Type 2 diabetes")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "epilepsy_before_threshold", "Epilepsy")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "hf_before_threshold", "Heart Failure")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "hypothyroid_before_threshold", "Hypothyroidism")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "osteoporosis_before_threshold", "Osteoporosis")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "pad_before_threshold", "Peripheral artery disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "ra_before_threshold", "Rheumatoid arthritis")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "smi_before_threshold", "Serious mental illness")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "stroke_before_threshold", "Stroke")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "tia_before_threshold", "Transient ischaemic attack")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "obese_before_threshold", "Obesity")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "cognitive_impair_before_threshold", "Cognitive impairment")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "statins_before_threshold", "Statin use")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "pneumovax_before_threshold", "Pneumococcal vaccination")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "fluvax_before_threshold", "Influenza vaccination")
check_discontinuity_pre(df_analysis, dob_threshold_date, threshold_date, "antihypertensives_before_threshold", "Antihypertensive use")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Check ascertainment of shingle vaccine from product codes in the vaccinations table versus snomed codes in the clinical events table ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# simple analysis to count how much overlap exists between product codes and snomed codes, within 1 year

df_analysis |>
  transmute(
    zostavax_within1year = coalesce(
      zostavax_date_1 <= threshold_date + 365,
      FALSE
    ),
    shvacgp_within1year = coalesce(
      shvacgp_first_date_ever <= threshold_date + 365,
      FALSE
    ),
  ) |>
  count(zostavax_within1year, shvacgp_within1year) |>
  mutate(
    pct_of_zostavax = (zostavax_within1year) * n / sum(n[zostavax_within1year]),
    pct_of_shvacgp = (shvacgp_within1year) * n / sum(n[shvacgp_within1year])
  ) |>
  write_csv(file = fs::path(output_dir, "product codes vs snomed codes.csv"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Cumulative incidence of vaccination since index date ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Cumulative uptake of Zostavax by age in months up to one year after the threshold date
cumulative_events(df_analysis |> filter(between(age, 79, 80)), "month_of_birth_fct", precision=7, start_date = threshold_date, index_date = threshold_date, 52*7, "zostavax_date_1", "Zostavax by month")

# Cumulative uptake of Zostavax by eligibility (born either side of threshold date)
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = threshold_date, 52*7, "zostavax_date_1", "Zostavax")

# Cumulative uptake of "shingles vaccine snomed code" by age in months up to one year after the threshold date
cumulative_events(df_analysis |> filter(between(age, 79, 80)), "month_of_birth_fct", precision=7, start_date = threshold_date, index_date = threshold_date, 52*7, "shvacgp_first_date_after", "Shingles vaccine snomed code by month")

# Cumulative uptake of "shingles vaccine snomed code" by eligibility (born either side of threshold date)
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = threshold_date, 52*7, "shvacgp_first_date_after", "Shingles vaccine snomed code")

# Cumulative uptake of "shingles vaccine snomed code" by eligibility (born either side of threshold date)
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = threshold_date, 365, "varicella_first_date_after", "Varicella vaccine")

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## Post-baseline information
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Cumulative incidence of all other outcomes by eligibility (born either side of threshold date)

cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "shingles_first_date_after", "Shingles")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "shingles_gp_first_date_after", "Shingles (GP only)")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "dementia_first_date_ever", "Dementia")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "neuralgia_first_date_after", "Neuralgia")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "neuralgia_gp_first_date_after", "Neuralgia (GP only)")

cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "alzheimers_first_date_ever", "Alzheimers dementia")
cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "vascular_first_date_ever", "Vascular dementia")
cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "dementia_charlson_first_date_ever", "Dementia (Charlson)")
cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "dementia_gp_first_date_ever", "Dementia (GP only)")

cumulative_events(df_analysis |> filter(!asthma_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "asthma_gp_first_date_ever", "Asthma")
cumulative_events(df_analysis |> filter(!afib_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "afib_gp_first_date_ever", "Atrial fibrillation")
cumulative_events(df_analysis |> filter(!chd_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "chd_gp_first_date_ever", "Coronary heart disease")
cumulative_events(df_analysis |> filter(!ckd_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "ckd_gp_first_date_ever", "Chronic kidney disease")
cumulative_events(df_analysis |> filter(!copd_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "copd_gp_first_date_ever", "Chronic obstructive pulmonary disease")
cumulative_events(df_analysis |> filter(!depression_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "depression_gp_first_date_ever", "Depression")
cumulative_events(df_analysis |> filter(!t2dm_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "t2dm_gp_first_date_ever", "Type 2 diabetes")
cumulative_events(df_analysis |> filter(!epilepsy_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "epilepsy_gp_first_date_ever", "Epilepsy")
cumulative_events(df_analysis |> filter(!hf_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "hf_gp_first_date_ever", "Heart Failure")
cumulative_events(df_analysis |> filter(!hypothyroid_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "hypothyroid_gp_first_date_ever", "Hypothyroidism")
cumulative_events(df_analysis |> filter(!osteoporosis_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "osteoporosis_gp_first_date_ever", "Osteoporosis")
cumulative_events(df_analysis |> filter(!pad_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "pad_gp_first_date_ever", "Peripheral artery disease")
cumulative_events(df_analysis |> filter(!ra_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "ra_gp_first_date_ever", "Rheumatoid arthritis")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "stroke_gp_first_date_after", "Stroke")
cumulative_events(df_analysis, "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "tia_gp_first_date_after", "Transient ischaemic attack")
cumulative_events(df_analysis |> filter(!smi_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "smi_gp_first_date_ever", "Serious mental illness")
cumulative_events(df_analysis |> filter(!obese_before_threshold), "eligibility", precision=7, start_date = threshold_date, index_date = index_date, 365, "obese_gp_first_date_ever", "Obesity")


# Cumulative incidence of outcomes by month of birth, up to 7 years after threshold

# zostavax - start follow up immeidately after threshold
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, threshold_date, time_horizon_years=7, "zostavax_date_1", "Zostavax")

# outcomes - start follow up from index date
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "shingles_first_date_after", "Shingles")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "varicella_first_date_after", "Varicella vaccine")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "neuralgia_first_date_after", "Neuralgia")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "alzheimers_first_date_ever", "Alzheimers dementia")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "vascular_first_date_ever", "Vascular dementia")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "dementia_first_date_ever", "Dementia")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "date_of_death", "All-cause death")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "date_of_death_gp", "All-cause death (TPP only)")

# other covariates - start follow-up from index date
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!asthma_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "asthma_gp_first_date_ever", "Asthma")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!afib_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "afib_gp_first_date_ever", "Atrial fibrillation")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!chd_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "chd_gp_first_date_ever", "Coronary heart disease")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!ckd_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "ckd_gp_first_date_ever", "Chronic kidney disease")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!copd_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "copd_gp_first_date_ever", "Chronic obstructive pulmonary disease")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!depression_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "depression_gp_first_date_ever", "Depression")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!t2dm_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "t2dm_gp_first_date_ever", "Type 2 diabetes")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!epilepsy_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "epilepsy_gp_first_date_ever", "Epilepsy")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!hf_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "hf_gp_first_date_ever", "Heart Failure")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!hypothyroid_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "hypothyroid_gp_first_date_ever", "Hypothyroidism")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!osteoporosis_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "osteoporosis_gp_first_date_ever", "Osteoporosis")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!pad_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "pad_gp_first_date_ever", "Peripheral artery disease")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!ra_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "ra_gp_first_date_ever", "Rheumatoid arthritis")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "stroke_gp_first_date_after", "Stroke")
yearly_cumulative_incidence_by_dob(df_analysis, dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "tia_gp_first_date_after", "Transient ischaemic attack")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!smi_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "smi_gp_first_date_ever", "Serious mental illness")
yearly_cumulative_incidence_by_dob(df_analysis |> filter(!obese_before_threshold), dob_threshold_date, threshold_date, index_date, time_horizon_years=7, "obese_gp_first_date_ever", "Obesity")
