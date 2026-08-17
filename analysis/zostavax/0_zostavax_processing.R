library(tidyverse)
library(lubridate)
library(arrow)

source(here::here("analysis", "common_code", "processing.R"))

df_main <- read_feather(here::here("output","zostavax","dataset_zostavax_main.arrow"))
df_processed_main <- processing(df_main, as.Date("2013-09-01"), as.Date("2014-02-01"), "zostavax","main")

df_2010 <- read_feather(here::here("output","zostavax","dataset_zostavax_2010.arrow"))
df_processed_2010 <- processing(df_2010, as.Date("2010-09-01"), as.Date("2011-02-01"), "zostavax","2010")

df_2016 <- read_feather(here::here("output","zostavax","dataset_zostavax_2016.arrow"))
df_processed_2016 <- processing(df_2016, as.Date("2016-09-01"), as.Date("2017-02-01"), "zostavax","2016")
