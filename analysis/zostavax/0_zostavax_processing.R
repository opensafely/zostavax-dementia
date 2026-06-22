library(tidyverse)
library(lubridate)

zostavax_main <- read_csv(here::here("output/zostavax/dataset_zostavax_main.csv.gz")) %>%
  mutate(across(contains("date"), ymd)) # convert all dates to date 

