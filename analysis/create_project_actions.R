# Load libraries ----
library(tidyverse)
library(yaml)
library(here)
library(glue)
library(readr)
library(dplyr)

# Specify analysis components ----
vaccine_name <- "zostavax"
threshold_date <- "2013-09-01"
index_date <- "2014-02-01"
min_dob <- "1920-09-01"
max_dob <- "1948-09-01"

# Load analyses ----
analyses <- read_csv(glue("lib/{vaccine_name}.csv"))

# Extract population and analysis groups from analyses ----
populations <- unique(analyses$population)
analysis_groups <- unique(analyses$analysis_group)
population_analysis_group <- unique(paste0(
  analyses$population,
  "-",
  analyses$analysis_group
))

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

# Create function to count number of actions ----
count_run_elements <- function(x) {
  if (!is.list(x)) {
    return(0)
  }

  # Check if any names of this list are "run"
  current_count <- sum(names(x) == "run", na.rm = TRUE)

  # Recursively check all elements in the list
  return(current_count + sum(sapply(x, count_run_elements)))
}

# Create project-specific functions

## Create make_population function ----
make_population <- function(vax, population) {
  splice(
    action(
      name = glue("rd_populations_{population}"),
      run = glue(
        "r:v2 analysis/{vaccine_name}/3_{vaccine_name}_rd_populations.R {population}"
      ),
      needs = list(glue("process_{vaccine_name}")),
      highly_sensitive = list(
        cohort = glue(
          "output/{vaccine_name}/populations/dataset_analysis_{vaccine_name}_main_{population}.arrow"
        )
      )
    )
  )
}

## Create run_rd_analysis function ----
run_rd_analysis <- function(vax, population_analysis_group) {
  population <- strsplit(population_analysis_group, split = "-")[[1]][1]
  analysis_group <- strsplit(population_analysis_group, split = "-")[[1]][2]
  splice(
    action(
      name = glue(
        "rd_analysis_{analysis_group}"
      ),
      run = glue(
        "r:v2 analysis/{vaccine_name}/5_{vaccine_name}_rd_analysis.R {population} {analysis_group}"
      ),
      needs = list(
        glue("rd_populations_{population}"),
        glue("rd_msebw_{vaccine_name}")
      ),
      highly_sensitive = list(
        cohort = glue(
          "output/{vaccine_name}/results/rd_results_{analysis_group}.csv"
        )
      )
    )
  )
}


# Define and combine all actions into a list of actions ----

actions_list <- splice(
  ## Post YAML disclaimer ----

  comment(
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #",
    "DO NOT EDIT project.yaml DIRECTLY",
    "This file is created by create_project_actions.R",
    "Edit and run create_project_actions.R to update the project.yaml",
    "# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #"
  ),

  comment("Generate dataset"),

  action(
    name = glue("generate_dataset_{vaccine_name}_main"),
    run = glue(
      "ehrql:v1 generate-dataset analysis/dataset_definition.py --output output/{vaccine_name}/dataset_{vaccine_name}_main.arrow --dummy-data-file analysis/dummy_data/dummy_dataset_{vaccine_name}.arrow -- --threshold_date {threshold_date} --index_date {index_date} --min_dob {min_dob} --max_dob {max_dob} --vaccine_name {vaccine_name}"
    ),
    highly_sensitive = list(
      data1 = glue("output/{vaccine_name}/dataset_{vaccine_name}_main.arrow")
    )
  ),

  comment("Process dataset"),

  action(
    name = glue("process_{vaccine_name}"),
    run = glue("r:v2 analysis/{vaccine_name}/0_{vaccine_name}_processing.R"),
    needs = list(glue("generate_dataset_{vaccine_name}_main")),
    highly_sensitive = list(
      data1 = glue(
        "output/{vaccine_name}/processed/dataset_processed_{vaccine_name}_main.arrow"
      )
    )
  ),

  comment("Flow chart"),

  action(
    name = glue("flow_chart_{vaccine_name}"),
    run = glue("r:v2 analysis/{vaccine_name}/1_{vaccine_name}_flow_chart.R"),
    needs = list(glue("process_{vaccine_name}")),
    moderately_sensitive = list(
      data1 = glue(
        "output/{vaccine_name}/processed/flow_chart_{vaccine_name}_main.csv"
      )
    )
  ),

  comment("Descriptives"),

  action(
    name = glue("descriptives_{vaccine_name}"),
    run = glue("r:v2 analysis/{vaccine_name}/2_{vaccine_name}_descriptives.R"),
    needs = list(glue("process_{vaccine_name}")),
    moderately_sensitive = list(
      txt = glue("output/{vaccine_name}/descriptives/main/*.txt"),
      csv = glue("output/{vaccine_name}/descriptives/main/*.csv"),
      png = glue("output/{vaccine_name}/descriptives/main/*.png")
    )
  ),

  comment("Define populations"),

  splice(
    unlist(
      lapply(
        populations,
        function(x) {
          make_population(
            vax = glue("{vaccine_name}"),
            population = x
          )
        }
      ),
      recursive = FALSE
    )
  ),

  comment("Determine MSE optimal bandwidth"),

  action(
    name = glue("rd_msebw_{vaccine_name}"),
    run = glue("r:v2 analysis/{vaccine_name}/4_{vaccine_name}_rd_msebw.R"),
    needs = list("rd_populations_general"),
    moderately_sensitive = list(
      data1 = glue("output/{vaccine_name}/setup/rd_msebw.csv")
    )
  ),

  comment("Run RD analyses"),

  splice(
    unlist(
      lapply(
        population_analysis_group,
        function(x) {
          run_rd_analysis(
            vax = glue("{vaccine_name}"),
            population_analysis_group = x
          )
        }
      ),
      recursive = FALSE
    )
  ),

  comment("Make output"),

  action(
    name = glue("rd_output_{vaccine_name}"),
    run = glue("r:v2 analysis/{vaccine_name}/6_{vaccine_name}_rd_output.R"),
    needs = as.list(paste0("rd_analysis_", analysis_groups)),
    moderately_sensitive = list(
      data1 = glue("output/{vaccine_name}/output/rd_results.csv"),
      data2 = glue("output/{vaccine_name}/output/rd_results_rounded.csv")
    )
  )
)

# Combine actions into project list ----
project_list <- splice(
  defaults_list,
  list(actions = actions_list)
)

# Convert list to yaml, reformat, and output a .yaml file ----
as.yaml(project_list, indent = 2) %>%
  # convert comment actions to comments
  convert_comment_actions() %>%
  # add one blank line before level 1 and level 2 keys
  str_replace_all("\\\n(\\w)", "\n\n\\1") %>%
  str_replace_all("\\\n\\s\\s(\\w)", "\n\n  \\1") %>%
  writeLines("project.yaml")

# Return number of actions ----
print(paste0(
  "YAML created with ",
  count_run_elements(actions_list),
  " actions."
))
