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

# define index_date specific dates
if(analysis == "main"){
  index_date <- as.Date("2013-09-01")
  dob_threshold_date <- as.Date("1934-09-01")
}

if(analysis == "2010"){
  index_date <- as.Date("2013-09-01") + years(-3)
  dob_threshold_date <- as.Date("1934-09-01") + years(-3)
}

if(analysis == "2016"){
  index_date <- as.Date("2013-09-01") + years(3)
  dob_threshold_date <- as.Date("1934-09-01") + years(3)
}

# run functions ----
source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "descriptives.R"))


# create output directory ----
output_dir <- here("output", vaccine_name, "descriptives", analysis)
fs::dir_create(output_dir)

# read and prepare data ----

df_processed <- read_feather(here("output", vaccine_name, "processed", glue("dataset_processed_{vaccine_name}_{analysis}.arrow")))

# print summary of processed dataset - not disclosure controlled, not for release
capture.output(
  skimr::skim_without_charts(df_processed),
  file = fs::path(output_dir, "df_processed_skim.txt"),
  split = FALSE
)

# create core analysis dataset with exclusions applied
df_analysis <- 
  df_processed |> 
  filter(no_common_exclusions) |>
  mutate(
    eligibility = .data[[glue("zostavax_eligibility_{analysis}")]],
    month_of_birth = floor_date(date_of_birth, "month"), # this should be an identity because currently in OS birth dates are rounded down to 1st of the month
    month_diff_threshold = as.integer(time_length(interval(dob_threshold_date, date_of_birth), unit = "months")),
    day_diff_threshold = as.integer(date_of_birth - dob_threshold_date),
    registration_duration = as.numeric(as.Date("2013-09-02") - reg_start_date),
    censor_date = pmin(reg_end_date, date_of_death, na.rm = TRUE)
  )

# print summary of analysis dataset - not disclosure controlled, not for release
capture.output(
  skimr::skim_without_charts(df_analysis),
  file = fs::path(output_dir, "df_analysis_skim.txt"),
  split = FALSE
)

# many variables names are as follows:
# - [name]_first_date_ever - for outcomes where we are only interested in the very first event (i.e. exclude people with the event before threshold);
# - [name]_first_date_after - for recurrent outcomes where we don't care about events before threshold;
# - [name]_before_threshold - T/F columns which indicates whether an individual had the event/diagnosis before the threshold date (either ever or a set time period, depending on protocol)

# look-up list for nice variable names
variable_labels <-
  lst(

    # Total numbers
    N  = "Total N",

    # primary exposure
    eligibility = "Eligibility",

    # demographics
    age = "Age",
    sex = "Sex",
    ethnicity6 = "Ethnicity",
    imd_quintle = "Deprivation (by IMD quintile)",
    region = "Region",
    registration_duration = "GP registration duration",

    #prior outcomes 
    shingles_before_threshold = "Prior Shingles",
    neuralgia_before_threshold = "Prior Neuralgia",
    varicella_before_threshold = "Prior varicella",

    # prior disease
    asthma_before_threshold = "Asthma",
    afib_before_threshold = "Atrial fibulation",
    chd_before_threshold = "Chronic heart disease",
    ckd_before_threshold = "Chronic kidney disease",
    copd_before_threshold = "Chronic obstructive pulmonary disease",
    depression_before_threshold = "Depression",
    t2dm_before_threshold = "Type 2 diabetes",
    epilepsy_before_threshold = "Epilepsy",
    hf_before_threshold = "Heart Failure",
    hypothyroid_before_threshold = "Hypothyroidism",
    osteoporosis_before_threshold = "Osteoporosis",
    pad_before_threshold =  "peripheral arterial disease",
    ra_before_threshold = "rheumatoid arthritis",
    stroke_before_threshold = "Stroke",
    smi_before_threshold = "Serious mental illness",
    obese_before_threshold = "Obesity",
    cognitive_impair_before_threshold = "Cognitive impairment",

    # prior healthcare
    statins_before_threshold = "Statin use",
    pneumovax_before_threshold = "Pnumuococcal vaccination",
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
  filter(!is.na(group1_level))  |>
    pivot_wider(
      id_cols = c(variable, variable_label, variable_level, context),
      names_from = group1_level,
      names_sep = "_",
      values_from = c(n, N, p, mean, sd, p10, p25, median, p75, p90),
      names_vary = "slowest"
    )

# save to disk
write_csv(table_balance_wide, fs::path(output_dir, "table_balance.csv"))


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Check pre-index date discontinuities: report rate of event X by week of birth ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

check_discontinuity_pre <- function(.data, dob_threshold_date, index_date, event_col, event_name) {
  
  dat_summary <-
    .data |>
    transmute(
      month_of_birth,
      event = .data[[event_col]],
     ) |>
    group_by(month_of_birth) |>
    summarise(
      n =n(),
      event_n = sum(event),
      event_rate = mean(event)
    ) |>
    ungroup() |> 
    # apply SDC
    mutate(
      n = sdc.rounding(n, sdc.threshold),
      event_n = sdc.rounding(event_n, sdc.threshold),
      event_rate = event_n / n
    ) 

  plot_summary <-
    ggplot(dat_summary) +
    geom_point(aes(x = month_of_birth, y = event_rate))+
    geom_vline(aes(xintercept = dob_threshold_date), linetype="dashed")+
    scale_x_date(
      
      # Three possible options for scale of horizontal axis 
      name = "Date of birth", labels = ~ scales::label_date("%d %b %y")(.), breaks = dob_threshold_date + months(seq(-10,10)*12),
      #name = glue("Month of birth (relative to {scales::label_date('%d %b %y')(dob_threshold_date)})"), labels = ~ interval(dob_threshold_date, .) %/% months(1), breaks = dob_threshold_date + months(seq(-10,10)*6),
      #name = glue("Age at {scales::label_date('%d %b %y')(index_date)}"), labels = ~ (interval(., index_date) %/% months(1))/12, breaks = dob_threshold_date + months(seq(-10,10)*6),
        
    )+
    scale_y_continuous(labels = scales::label_percent())+
    labs(
      y = glue("Proportion")
    )+
    theme_bw()
  
  
  print(plot_summary)

  # save to disk
  ggsave(plot_summary, filename=glue("discontinuity_pre_{event_name}.png"), path=output_dir)
}

check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "shingles_before_threshold", "Prior Shingles")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "asthma_before_threshold", "Asthma")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "afib_before_threshold", "Atrial fibulation")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "chd_before_threshold", "Chronic heart disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "ckd_before_threshold", "Chronic kidney disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "copd_before_threshold", "Chronic obstructive pulmonary disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "depression_before_threshold", "Depression")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "t2dm_before_threshold", "Type 2 diabetes")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "epilepsy_before_threshold", "Epilepsy")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "hf_before_threshold", "Heart Failure")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "hypothyroid_before_threshold", "Hypothyroidism")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "osteoporosis_before_threshold", "Osteoporosis")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "pad_before_threshold", "Peripheral arterial disease")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "ra_before_threshold", "Rheumatoid arthritis")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "smi_before_threshold", "Serious mental illness")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "stroke_before_threshold", "Stroke")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "obese_before_threshold", "Obesity")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "cognitive_impair_before_threshold", "Cognitive impairment")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "statins_before_threshold", "Statin use")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "pneumovax_before_threshold", "Pnumuococcal vaccination")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "fluvax_before_threshold", "Influenza vaccination")
check_discontinuity_pre(df_analysis, dob_threshold_date, index_date, "antihypertensives_before_threshold", "Antihypertensive use")




# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Cumulative incidence of vaccination since index date ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# calculate KM estimates, save data, and plot curves

cumulative_events <- function(.data, group, precision, time_horizon, event_date_col, event_name){
  
  df_km <- km(.data, group, index_date, precision, time_horizon, event_date_col, "censor_date")

  write_csv(df_km, fs::path(output_dir, glue("cumulative_incidence_{event_name}_{time_horizon}.csv")))

  df_time0 <-
    df_km |>
    mutate(
      lagtime = lag(time, 1, 0), # assumes the time-origin is zero
    ) %>%
    group_modify(
      ~ add_row(
        .x,
        time = 0, # assumes time origin is zero
        lagtime = 0,
        cmlinc = 0,
        cmlinc.low = 0,
        cmlinc.high = 0,
        .before = 0
      )
    )
  
   plot_km <-
    ggplot(df_time0, aes(group = group, colour = group, fill = group)) +
    geom_step(aes(x = time, y = cmlinc), direction = "vh") +
    geom_step(aes(x = time, y = cmlinc), direction = "vh", linetype = "dashed", alpha = 0.5) +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high), alpha = 0.1, colour = "transparent") +
    scale_color_discrete() +
    scale_fill_discrete(guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Time",
      y = "Cumulative Incidence",
      colour = NULL,
      fill = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(.05, .95),
      legend.justification = c(0, 1),
    )
  
  print(plot_km)
  
  ggsave(plot_km, filename=glue("cumulative_incidence_{event_name}_{time_horizon}.png"), path=output_dir, width = 15, height = 15, units = "cm")

}

cumulative_events(df_analysis |> mutate(month_of_birth = scales::label_date(format = "%y %b")(month_of_birth)) |> filter(between(age, 79, 80)), "month_of_birth", precision=7, 365, "zostavax_date_1", "Zostavax")

cumulative_events(df_analysis, "eligibility", precision=7, 365, "zostavax_date_1", "Zostavax")
cumulative_events(df_analysis, "eligibility", precision=7, 365, "shingles_first_date_after", "Shingles")
cumulative_events(df_analysis, "eligibility", precision=7, 365, "dementia_first_date_ever", "Dementia")
cumulative_events(df_analysis, "eligibility", precision=7, 365, "varicella_first_date_after", "Varicella")
cumulative_events(df_analysis |> filter(!neuralgia_before_threshold), "eligibility", precision=7, 365, "neuralgia_first_date_ever", "Neuralgia") 
cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, 365, "alzheimers_first_date_ever", "Alzheimer's")  
cumulative_events(df_analysis |> filter(!dementia_exclude_before_threshold), "eligibility", precision=7, 365, "vascular_first_date_ever", "Vascular Dementia")  

cumulative_events(df_analysis |> filter(!asthma_before_threshold), "eligibility", precision=7, 365, "asthma_gp_first_date_ever", "Asthma")
cumulative_events(df_analysis |> filter(!afib_before_threshold), "eligibility", precision=7, 365, "afib_gp_first_date_ever", "Arterial fibulation")
cumulative_events(df_analysis |> filter(!chd_before_threshold), "eligibility", precision=7, 365, "chd_gp_first_date_ever", "Chronic heart disease")
cumulative_events(df_analysis |> filter(!ckd_before_threshold), "eligibility", precision=7, 365, "ckd_gp_first_date_ever", "Chronic kidney disease")
cumulative_events(df_analysis |> filter(!copd_before_threshold), "eligibility", precision=7, 365, "copd_gp_first_date_ever", "Chronic obstructive pulmonary disease")
cumulative_events(df_analysis |> filter(!depression_before_threshold), "eligibility", precision=7, 365, "depression_gp_first_date_ever", "Depression")
cumulative_events(df_analysis |> filter(!t2dm_before_threshold), "eligibility", precision=7, 365, "t2dm_gp_first_date_ever", "Type 2 diabetes")
cumulative_events(df_analysis |> filter(!epilepsy_before_threshold), "eligibility", precision=7, 365, "epilepsy_gp_first_date_ever", "Epilepsy")
cumulative_events(df_analysis |> filter(!hf_before_threshold), "eligibility", precision=7, 365, "hf_gp_first_date_ever", "Heart Failure")
cumulative_events(df_analysis |> filter(!hypothyroid_before_threshold), "eligibility", precision=7, 365, "hypothyroid_gp_first_date_ever", "Hypothyroidism")
cumulative_events(df_analysis |> filter(!osteoporosis_before_threshold), "eligibility", precision=7, 365, "osteoporosis_gp_first_date_ever", "Osteoporosis")
cumulative_events(df_analysis |> filter(!pad_before_threshold), "eligibility", precision=7, 365, "pad_gp_first_date_ever", "Peripheral arterial disease")
cumulative_events(df_analysis |> filter(!ra_before_threshold), "eligibility", precision=7, 365, "ra_gp_first_date_ever", "Rheumatoid arthritis")
cumulative_events(df_analysis |> filter(!stroke_before_threshold), "eligibility", precision=7, 365, "stroke_gp_first_date_ever", "Stroke")
cumulative_events(df_analysis |> filter(!smi_before_threshold), "eligibility", precision=7, 365, "smi_gp_first_date_ever", "Serious mental illness")
cumulative_events(df_analysis |> filter(!obese_before_threshold), "eligibility", precision=7, 365, "obese_gp_first_date_ever", "Obesity")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
# Check post-index date discontinuities: report rate of event X by week of birth ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

check_discontinuity_post <- function(.data, dob_threshold_date, index_date, time_horizon, event_date_col, event_name) {
  
  index_date <- index_date - 1L

  dat_summary <-
    .data |>
    transmute(
      month_of_birth,
      day_diff_threshold,
      event_date = .data[[event_date_col]],
      censor_date = pmin(reg_end_date, date_of_death, na.rm=TRUE),
      event_time = as.integer(pmin(event_date, censor_date, index_date + time_horizon, na.rm=TRUE) - index_date),
      event_indicator = event_time < time_horizon
    ) |>
    group_by(month_of_birth) |>
    summarise(
      n = n(),
      cmlinc = 1 - km_at_t(event_time = event_time, event_indicator = event_indicator, time_horizon = time_horizon)
    ) |> 
    ungroup() |>
    # apply SDC
    mutate(
      n = sdc.rounding(n, sdc.threshold),
      cmlinc = sdc.rounding(cmlinc*n, sdc.threshold) / n, 
    )

  plot_summary <-
    ggplot(dat_summary) +
    geom_point(aes(x = month_of_birth, y = cmlinc))+
    geom_vline(aes(xintercept = dob_threshold_date), linetype="dashed")+
    scale_x_date(
      
      # Three possible options for scale of horizontal axis 

      name = "Date of birth", labels = ~ scales::label_date("%d %b %y")(.), breaks = dob_threshold_date + months(seq(-10,10)*12),
      #name = glue("Month of birth (relative to {scales::label_date('%d %b %y')(dob_threshold_date)})"), labels = ~ interval(dob_threshold_date, .) %/% months(1), breaks = dob_threshold_date + months(seq(-10,10)*6),
      #name = glue("Age at {scales::label_date('%d %b %y')(index_date)}"), labels = ~ (interval(., index_date) %/% (months(1))/12, breaks = dob_threshold_date + months(seq(-10,10)*6),
      
      ## For some reason using a secondary axis like this doesn't work! very frustrating
      #sec.axis = sec_axis(
      #  name = glue("Age at {scales::label_date('%d %b %y')(index_date)}"), 
      #  transform = ~ . , 
      #  labels = ~ (interval(., index_date) %/% (months(1)))/12,
      #  breaks = dob_threshold_date + months(seq(-10,10)*6)
      #)
    )+
    scale_y_continuous(labels = scales::label_percent())+
    labs(
      y = glue("Cumulative incidence of event within {time_horizon} days")
    )+
    theme_bw()
  
  
  print(plot_summary)

  ggsave(plot_summary, filename=glue("discontinuity_{event_name}_{time_horizon}.png"), path=output_dir)
}

## 1 year after

# outcomes
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365, "zostavax_date_1", "Zostavax")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365, "shingles_first_date_after", "Shingles")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365, "varicella_first_date_after", "Varicella")
check_discontinuity_post(df_analysis |> filter(!neuralgia_before_threshold), dob_threshold_date, index_date, 365, "neuralgia_first_date_ever", "Neuralgia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365, "alzheimers_first_date_ever", "Alzheimer's dementia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365, "vascular_first_date_ever", "Vascular dementia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365, "dementia_first_date_ever", "Dementia")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365, "date_of_death", "All-cause death")

# other covariates
check_discontinuity_post(df_analysis |> filter(!asthma_before_threshold), dob_threshold_date, index_date, 365, "asthma_gp_first_date_ever", "Asthma")
check_discontinuity_post(df_analysis |> filter(!afib_before_threshold), dob_threshold_date, index_date, 365, "afib_gp_first_date_ever", "Arterial fibulation")
check_discontinuity_post(df_analysis |> filter(!chd_before_threshold), dob_threshold_date, index_date, 365, "chd_gp_first_date_ever", "Chronic heart disease")
check_discontinuity_post(df_analysis |> filter(!ckd_before_threshold), dob_threshold_date, index_date, 365, "ckd_gp_first_date_ever", "Chronic kidney disease")
check_discontinuity_post(df_analysis |> filter(!copd_before_threshold), dob_threshold_date, index_date, 365, "copd_gp_first_date_ever", "Chronic obstructive pulmonary disease")
check_discontinuity_post(df_analysis |> filter(!depression_before_threshold), dob_threshold_date, index_date, 365, "depression_gp_first_date_ever", "Depression")
check_discontinuity_post(df_analysis |> filter(!t2dm_before_threshold), dob_threshold_date, index_date, 365, "t2dm_gp_first_date_ever", "Type 2 diabetes")
check_discontinuity_post(df_analysis |> filter(!epilepsy_before_threshold), dob_threshold_date, index_date, 365, "epilepsy_gp_first_date_ever", "Epilepsy")
check_discontinuity_post(df_analysis |> filter(!hf_before_threshold), dob_threshold_date, index_date, 365, "hf_gp_first_date_ever", "Heart Failure")
check_discontinuity_post(df_analysis |> filter(!hypothyroid_before_threshold), dob_threshold_date, index_date, 365, "hypothyroid_gp_first_date_ever", "Hypothyroidism")
check_discontinuity_post(df_analysis |> filter(!osteoporosis_before_threshold), dob_threshold_date, index_date, 365, "osteoporosis_gp_first_date_ever", "Osteoporosis")
check_discontinuity_post(df_analysis |> filter(!pad_before_threshold), dob_threshold_date, index_date, 365, "pad_gp_first_date_ever", "Peripheral arterial disease")
check_discontinuity_post(df_analysis |> filter(!ra_before_threshold), dob_threshold_date, index_date, 365, "ra_gp_first_date_ever", "Rheumatoid arthritis")
check_discontinuity_post(df_analysis |> filter(!stroke_before_threshold), dob_threshold_date, index_date, 365, "stroke_gp_first_date_ever", "Stroke")
check_discontinuity_post(df_analysis |> filter(!smi_before_threshold), dob_threshold_date, index_date, 365, "smi_gp_first_date_ever", "Serious mental illness")
check_discontinuity_post(df_analysis |> filter(!obese_before_threshold), dob_threshold_date, index_date, 365, "obese_gp_first_date_ever", "Obesity")


## 2 years after

#outcomes
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365*2, "zostavax_date_1", "Zostavax")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365*2, "shingles_first_date_after", "Shingles")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365*2, "varicella_first_date_after", "Varicella")
check_discontinuity_post(df_analysis |> filter(!neuralgia_before_threshold), dob_threshold_date, index_date, 365*2, "neuralgia_first_date_ever", "Neuralgia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365*2, "alzheimers_first_date_ever", "Alzheimer's dmenetia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365*2, "vascular_first_date_ever", "Vascular dementia")
check_discontinuity_post(df_analysis |> filter(!dementia_exclude_before_threshold), dob_threshold_date, index_date, 365*2, "dementia_first_date_ever", "Dementia")
check_discontinuity_post(df_analysis, dob_threshold_date, index_date, 365*2, "date_of_death", "All-cause death")

# other covariates
check_discontinuity_post(df_analysis |> filter(!asthma_before_threshold), dob_threshold_date, index_date, 365*2, "asthma_gp_first_date_ever", "Asthma")
check_discontinuity_post(df_analysis |> filter(!afib_before_threshold), dob_threshold_date, index_date, 365*2, "afib_gp_first_date_ever", "Arterial fibulation")
check_discontinuity_post(df_analysis |> filter(!chd_before_threshold), dob_threshold_date, index_date, 365*2, "chd_gp_first_date_ever", "Chronic heart disease")
check_discontinuity_post(df_analysis |> filter(!ckd_before_threshold), dob_threshold_date, index_date, 365*2, "ckd_gp_first_date_ever", "Chronic kidney disease")
check_discontinuity_post(df_analysis |> filter(!copd_before_threshold), dob_threshold_date, index_date, 365*2, "copd_gp_first_date_ever", "Chronic obstructive pulmonary disease")
check_discontinuity_post(df_analysis |> filter(!depression_before_threshold), dob_threshold_date, index_date, 365*2, "depression_gp_first_date_ever", "Depression")
check_discontinuity_post(df_analysis |> filter(!t2dm_before_threshold), dob_threshold_date, index_date, 365*2, "t2dm_gp_first_date_ever", "Type 2 diabetes")
check_discontinuity_post(df_analysis |> filter(!epilepsy_before_threshold), dob_threshold_date, index_date, 365*2, "epilepsy_gp_first_date_ever", "Epilepsy")
check_discontinuity_post(df_analysis |> filter(!hf_before_threshold), dob_threshold_date, index_date, 365*2, "hf_gp_first_date_ever", "Heart Failure")
check_discontinuity_post(df_analysis |> filter(!hypothyroid_before_threshold), dob_threshold_date, index_date, 365*2, "hypothyroid_gp_first_date_ever", "Hypothyroidism")
check_discontinuity_post(df_analysis |> filter(!osteoporosis_before_threshold), dob_threshold_date, index_date, 365*2, "osteoporosis_gp_first_date_ever", "Osteoporosis")
check_discontinuity_post(df_analysis |> filter(!pad_before_threshold), dob_threshold_date, index_date, 365*2, "pad_gp_first_date_ever", "Peripheral arterial disease")
check_discontinuity_post(df_analysis |> filter(!ra_before_threshold), dob_threshold_date, index_date, 365*2, "ra_gp_first_date_ever", "Rheumatoid arthritis")
check_discontinuity_post(df_analysis |> filter(!stroke_before_threshold), dob_threshold_date, index_date, 365*2, "stroke_gp_first_date_ever", "Stroke")
check_discontinuity_post(df_analysis |> filter(!smi_before_threshold), dob_threshold_date, index_date, 365*2, "smi_gp_first_date_ever", "Serious mental illness")
check_discontinuity_post(df_analysis |> filter(!obese_before_threshold), dob_threshold_date, index_date, 365*2, "obese_gp_first_date_ever", "Obesity")

