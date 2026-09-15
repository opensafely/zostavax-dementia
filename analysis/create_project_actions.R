# Load libraries ----

library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)

# Specify analysis components ----

vax <- "zostavax"
threshold_date <- "2013-09-01"
index_date <- "2014-02-01"
min_dob <- "1920-09-01"
max_dob <- "1948-09-01"
populations <- c(
  "general",
  "reduced_excl",
  "aged78_81",
  "aged77_82",
  "women",
  "men",
  "cogimp_yes",
  "cogimp_no",
  "thresholdm_no"
)
analysis_groups <- c()

# Specify defaults ----

defaults_list <- list(
  version = "5.0"
)

# Create generic action function -----

action <- function(
  name,
  run,
  dummy_data_file = NULL,
  arguments = NULL,
  needs = NULL,
  highly_sensitive = NULL,
  moderately_sensitive = NULL
) {
  outputs <- list(
    moderately_sensitive = moderately_sensitive,
    highly_sensitive = highly_sensitive
  )
  outputs[sapply(outputs, is.null)] <- NULL

  actions <- list(
    run = paste(c(run, arguments), collapse = " "),
    dummy_data_file = dummy_data_file,
    needs = needs,
    outputs = outputs
  )
  actions[sapply(actions, is.null)] <- NULL

  action_list <- list(name = actions)
  names(action_list) <- name

  action_list
}

# Create populations function ----

make_population <- function(vax, population) {
  splice(
    action(
      name = glue("rd_populations_{population}"),
      run = glue(
        "r:v2 analysis/{vax}/3_{vax}_rd_populations.R {population}"
      ),
      needs = list(glue("process_{vax}")),
      highly_sensitive = list(
        cohort = glue(
          "output/{vax}/populations/dataset_analysis_{vax}_main_{population}.arrow"
        )
      )
    )
  )
}

# Create generic comment function ----

comment <- function(...) {
  list_comments <- list(...)
  comments <- map(list_comments, ~ paste0("## ", ., " ##"))
  comments
}

# Create function to convert comment "actions" in a yaml string into proper comments ----

convert_comment_actions <- function(yaml.txt) {
  yaml.txt %>%
    str_replace_all("\\\n(\\s*)\\'\\'\\:(\\s*)\\'", "\n\\1") %>%
    #str_replace_all("\\\n(\\s*)\\'", "\n\\1") %>%
    str_replace_all("([^\\'])\\\n(\\s*)\\#\\#", "\\1\n\n\\2\\#\\#") %>%
    str_replace_all("\\#\\#\\'\\\n", "\n")
}

# Define and combine all actions into a list of actions ------------------------

actions_list <- splice(
  ## Post YAML disclaimer ------------------------------------------------------

  comment(
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
    "DO NOT EDIT project.yaml DIRECTLY",
    "This file is created by create_project_actions.R",
    "Edit and run create_project_actions.R to update the project.yaml",
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),

  comment("Generate dataset"),

  action(
    name = glue("generate_dataset_{vax}_main"),
    run = glue(
      "ehrql:v1 generate-dataset analysis/dataset_definition.py --output output/{vax}/dataset_{vax}_main.arrow --dummy-data-file analysis/dummy_data/dummy_dataset_{vax}.arrow -- --threshold_date {threshold_date} --index_date {index_date} --min_dob {min_dob} --max_dob {max_dob} --vaccine_name {vax}"
    ),
    highly_sensitive = list(
      data1 = glue("output/{vax}/dataset_{vax}_main.arrow")
    )
  ),

  comment("Process dataset"),

  action(
    name = glue("process_{vax}"),
    run = glue("r:v2 analysis/{vax}/0_{vax}_processing.R"),
    needs = list(glue("generate_dataset_{vax}_main")),
    highly_sensitive = list(
      data1 = glue("output/{vax}/processed/dataset_processed_{vax}_main.arrow")
    )
  ),

  comment("Flow chart"),

  action(
    name = glue("flow_chart_{vax}"),
    run = glue("r:v2 analysis/{vax}/1_{vax}_flow_chart.R"),
    needs = list(glue("process_{vax}")),
    moderately_sensitive = list(
      data1 = glue("output/{vax}/processed/flow_chart_{vax}_main.csv")
    )
  ),

  comment("Descriptives"),

  action(
    name = glue("descriptives_{vax}"),
    run = glue("r:v2 analysis/{vax}/2_{vax}_descriptives.R"),
    needs = list(glue("process_{vax}")),
    moderately_sensitive = list(
      txt = glue("output/{vax}/descriptives/main/*.txt"),
      csv = glue("output/{vax}/descriptives/main/*.csv"),
      png = glue("output/{vax}/descriptives/main/*.png")
    )
  ),

  comment("Define populations"),

  splice(
    unlist(
      lapply(
        populations,
        function(x) {
          make_population(
            vax = glue("{vax}"),
            population = x
          )
        }
      ),
      recursive = FALSE
    )
  ) #,

  # comment("Determine MSE optimal bandwidth"),

  # action(
  #   name = glue("rd_msebw_{vax}"),
  #   run = glue("r:v2 analysis/{vax}/4_{vax}_rd_msebw.R"),
  #   needs = list(glue("rd_populations_general")),
  #   moderately_sensitive = list(
  #     data1 = "output/{vax}/setup/rd_msebw.csv"
  #   )
  # ),

  # # comment("Run RD analyses"),
  #
  # splice(
  #   unlist(
  #     lapply(
  #       populations,
  #       function(x)
  #         population_action(
  #           action = "analysis",
  #           script_number = "5",
  #           vax = glue("{vax}"),
  #           population = x
  #         )
  #     ),
  #     recursive = FALSE
  #   )
  # )
)

# Combine actions into project list --------------------------------------------

project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

# Convert list to yaml, reformat, and output a .yaml file ----------------------

as.yaml(project_list, indent = 2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")

# Return number of actions -----------------------------------------------------

count_run_elements <- function(x) {
  if (!is.list(x)) {
    return(0)
  }

  # Check if any names of this list are "run"
  current_count <- sum(names(x) == "run", na.rm = TRUE)

  # Recursively check all elements in the list
  return(current_count + sum(sapply(x, count_run_elements)))
}

print(paste0(
  "YAML created with ",
  count_run_elements(actions_list),
  " actions."
))
