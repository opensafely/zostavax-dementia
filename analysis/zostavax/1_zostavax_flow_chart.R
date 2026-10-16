library(tidyverse)

source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "exclusions_flow_chart.R"))

exclusions_zostavax_main <- exclusion_flow_chart(
  "zostavax",
  "main"
)

