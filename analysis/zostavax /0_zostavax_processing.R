library(tidyverse)
library(lubridate)
library(arrow)


index_date <- "2014-02-01"
threshold_date <- "2013-09-01"

# Currently the latest available copmplete SUS data is in November 2011
last_data_collection_date <- as.Date("2025-11-01")


exclusions <- function(df, threshold_date, index_date){
  
  df <- mutate(
    prior_zostavax = (zostavax_date_1 < index_date),
    immunosupp_exclude = immunosupp_gp_any_before,
    neuralgia_gp_first_date_ever
  )
    
}



zostavax_main <- read_feather(here::here("output/zostavax/dataset_zostavax_main.arrow")) %>%
  mutate(across(contains("date"), ymd)) %>% # convert all dates to date 
  mutate(last_date = min(date_of_death, reg_end_date, last_data_collection_date),
         
         
         
         dementia_any = (
           (dementia_gp_first_date_ever > index_date &
            dementia_gp_first_date_ever <= last_date) |
           (dementia_hosp_first_date_ever > index_date &
              dementia_hosp_first_date_ever <= last_date)
         ),
         
  )
  




# date of first dementia diagnosis (after index date)
# date of first shingles vax (after index date)
# prior history of dementia
# 