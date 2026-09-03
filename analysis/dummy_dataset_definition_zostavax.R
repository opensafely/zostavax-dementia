
# load packages ----
library("tidyverse")
library("simDAG")
library("arrow")
library("here")

data_ehrql <- read_feather(here("output/zostavax/dataset_zostavax_main.arrow"))

# select cohort size
N <- 500000

# bandwidth_size
bandwidth_days <- 365*3

# thresholds
threshold_date <- as.Date("2013-09-01")
index_date <- as.Date("2014-02-01")
dob_threshold_date <- threshold_date - years(80)

latest_date <- as.Date("2026-06-01")


# custom node functions ----

# simulate n birth dates uniformly within the range [origin_date + min, origin_date + max]
dob_node <- function(n, min, max, origin_date){
  dob <- floor_date(as.Date(runif(n, min = min, max = max), origin = origin_date), unit="months")
  return(dob)
}

# simulate n death dates exponentially, at rate rate_per_day, starting at origin_date, and censored at max_date
dod_node <- function(n, rate_per_day, max_date, origin_date){
  days_to_death <- rexp(n, rate = rate_per_day)
  max_days <- max_date - threshold_date
  censored_days_to_death <- if_else(days_to_death > max_days, NA_real_, days_to_death)
  dod <- as.Date(censored_days_to_death, origin = origin_date)
  return(dod)
}

# simulate n censored dates, occurring uniformly between min_date and max_date, but occurring at rate observed_rate
runif_censored_date <- function(n, min_date, max_date, observed_rate){
  observed <- runif(n) < observed_rate
  date <- as.Date(runif(n, max = max_date - min_date), origin = min_date)
  censored_date <- if_else(observed, date, as.Date(NA))
  return(censored_date)
}

# initiate DAG ----
dag <- empty_dag() +

  # demographics

  node("date_of_birth", type=dob_node, min = -bandwidth_days, max = bandwidth_days, origin_date = dob_threshold_date) +
  node("age", type="identity", formula = ~as.integer(floor(time_length(interval(date_of_birth, index_date), "year")))) + # ehrQL returns age as an integer in years
  node("sex", type="rcategorical", prob=c(0.5,0.5), labels=c("female", "male"), output="rcategorical") +
  node(
    "imd_quintile", type="rcategorical", 
    prob = rep(1/11, 11), 
    labels = c("1 (most deprived)", 2:4, "5 (least deprived)", "unknown"), 
    output = "rcategorical"
  ) +
  node(
    "ethnicity6", type="rcategorical", 
    prob = c(0.8, 0.025, 0.025, 0.025, 0.025, 0.05, 0.05), 
    labels = c(
      "White",
      "Mixed",
      "South Asian",
      "Black",
      "Other",
      "Not stated",
      "Unknown"
    ), 
    output="rcategorical"
  ) +
  node("date_of_death", type=dod_node, rate_per_day = 0.0001, max_date = latest_date, origin_date = threshold_date) +
  node("reg_start_date", type="identity", formula = ~ as.Date(-runif(n = N, max=365*15), origin = threshold_date))+
  node("reg_end_date", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.5, min_date = threshold_date, max_date = latest_date))+
  node(
    "region", type="rcategorical", 
    prob = c(0.2, 0.2, 0.3, 0.05, 0.05, 0.05, 0.05, 0.05, 0.05),
    labels = c(
      "North East",
      "North West",
      "Yorkshire and The Humber",
      "East Midlands",
      "West Midlands",
      "East",
      "London",
      "South East",
      "South West"
    ), 
    output="rcategorical"
  ) +
  
  # outcomes

  node("dementia_exclude_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.01, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("dementia_gp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.1), as.Date(NA), dementia_exclude_gp_first_date_ever))+
  node("dementia_charlson_gp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.1), as.Date(NA), dementia_gp_first_date_ever))+
  node("alzheimers_gp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_gp_first_date_ever))+
  node("vascular_gp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_gp_first_date_ever))+
  node("neuralgia_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  
  node("shingles_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.01, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("shingles_gp_first_date_after", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = threshold_date, max_date = latest_date)) +
    
  node("dementia_hosp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("alzheimers_hosp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_hosp_first_date_ever)) +
  node("vascular_hosp_first_date_ever", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_hosp_first_date_ever)) +
  node("neuralgia_hosp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +

  node("shingles_hosp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("shingles_hosp_first_date_after", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = threshold_date, max_date = latest_date)) +
  
  node("dementia_ons_date", type="identity", formula = ~ if_else(runif(n = N)<0.1, date_of_death, as.Date(NA))) +
  node("alzheimers_ons_date", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_ons_date)) +
  node("vascular_ons_date", type="identity", formula = ~ if_else(rbernoulli(n = N, 0.3), as.Date(NA), dementia_ons_date)) +
  
  node("immunosupp_gp_any_before", type="rbernoulli", p=0.05) +

  # vaccines
    
  node("zostavax_eligible", type="identity", 
    formula = ~ if_else(
      between(date_of_birth, as.Date("1933-09-02"), as.Date("1934-09-01")), TRUE, FALSE
    )
  ) +    
  node("zostavax_date_1", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = if_else(zostavax_eligible, 0.5, 0.01), min_date = threshold_date, max_date = threshold_date + years(2))) +
  #node("zostavax_date_2", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = if_else(!is.na(zostavax_date_1), 0.1, 0), min_date = zostavax_date_1, max_date = zostavax_date_1 + years(2)))
  node("shingrix_date_1", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = if_else(age<80, 0.01, 0.5), min_date = threshold_date, max_date = threshold_date + years(2))) +
  node("shingrix_date_2", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = if_else(!is.na(shingrix_date_1), 0.1, 0), min_date = shingrix_date_1, max_date = shingrix_date_1 + years(2)))+
  node("varicella_first_date_after", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.4, min_date = threshold_date, max_date = threshold_date + years(2)))+

  # health status and health care
    
  node("asthma_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("afib_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("chd_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("ckd_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("copd_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("depression_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("t2dm_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("epilepsy_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("hf_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("hypothyroid_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("osteoporosis_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("pad_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("ra_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("smi_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("obese_gp_first_date_ever", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = latest_date)) +
  node("lrti_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("smoker_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("past_smoker_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("antihypertensives_rx_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("statins_rx_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("fluvax_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("pneumovax_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("cognitive_impair_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("stroke_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("tia_gp_last_date_before", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = date_of_birth + years(60), max_date = threshold_date)) +
  node("stroke_gp_first_date_after", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = threshold_date, max_date = latest_date)) +
  node("tia_gp_first_date_after", type="identity", formula = ~ runif_censored_date(n = N, observed_rate = 0.1, min_date = threshold_date, max_date = latest_date))

## simulate from DAG ----

# set seed
set.seed(42)

# simulate data
sim_dat <- 
  sim_from_dag(dag, n_sim=N) |>
  mutate(
    patient_id = seq_len(N),
    .before = 1L,
  ) |>
  select(
    -zostavax_eligible
  )

# define and create output directory
output_dir <- here("analysis", "dummy_data")
fs::dir_create(output_dir)

# save to disk
write_feather(sim_dat, sink = here(output_dir, "dummy_dataset_zostavax.arrow"))
