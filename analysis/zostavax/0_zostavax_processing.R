library(tidyverse)
library(lubridate)
library(arrow)
library(fs)

source(here::here("analysis", "common_code", "processing.R"))

# Create output directory
fs::dir_create(
  here::here("output", "zostavax", "processed"),
  recurse = TRUE
)

df_processed_main <- processing(as.Date("2013-09-01"), as.Date("2014-02-01"), "zostavax","main")

df_processed_2010 <- processing(as.Date("2010-09-01"), as.Date("2011-02-01"), "zostavax","2010")

df_processed_2016 <- processing(as.Date("2016-09-01"), as.Date("2017-02-01"), "zostavax","2016")
