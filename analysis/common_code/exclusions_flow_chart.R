library(tidyverse)
library(readr)
library(arrow)
library(fs)
library(here)

exclusion_flow_chart <- function(vaccine_name, analysis, exclude_cols){
  
    df <- read_feather(
      here::here("output","zostavax","processed",
                 paste0("dataset_processed_", vaccine_name, "_", analysis, ".arrow")
                       )
    
    )# Select your exclusion criteria columns
    criteria <- df[, exclude_cols]
    
    # Pairwise counts
    flow_chart <- crossprod(as.matrix(criteria)) %>%
      as.data.frame() %>%
      mutate(
       # replace NAs with zeroes
       across(everything(), ~ replace_na(.x, 0)),
       # rounding and redaction
       across(everything(),
         ~ sdc.rounding(.x, sdc.threshold)
       )
    
    write_csv(
      flow_chart,
      here::here(
        "output", vaccine_name, "processed",
        paste0("flow_chart_", vaccine_name, "_", analysis, ".csv")
      )
    )
    
    return(flow_chart)
}

