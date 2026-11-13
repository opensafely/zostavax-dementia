library(tidyverse)

source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "exclusions_flow_chart.R"))


exclusion_flow_chart(
  "zostavax",
  "main"
)

exclusion_flow_chart(
  "zostavax",
  "2010"
)

exclusion_flow_chart(
  "zostavax",
  "2016"
)