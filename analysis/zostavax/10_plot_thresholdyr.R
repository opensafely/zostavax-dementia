# Load libraries ----
library("here")
library("glue")
library("ggplot2")
library("lubridate")
library("magrittr")
library("dplyr")

# Specify parameters ----
vaccine_name <- "zostavax"

# Create rd_plots directory ----
output_dir <- here::here("output", vaccine_name, "rd_plots")
fs::dir_create(output_dir)

# Source functions ----
source(here::here("analysis/specify_paths.R"))

# Load plots labels ----
labels <- read.csv("lib/labels.csv")

# Load RD estimates ----
df <- read.csv(glue::glue(
  release_dir,
  "{vaccine_name}/output/rd_results_rounded.csv"
))

# Define plot_annual_fup function ----
plot_thresholdyr <- function(vaccine_name = "zostavax", outcome = "dementia") {
  
  # Filter ----
  df <- df[df$population=="reduced_excl",]
  
  # Restrict to outcome ----
  outcome_var <- labels[labels$name==outcome,]$variable
  df <- df[df$outcome==outcome_var,]
  
  # Annotate ----
  df$primary <- ifelse(df$threshold_year==2013, "Primary", "Secondary")
  df$robust_est <- df$robust_coef * 100
  df$robust_coef_lci <- (df$robust_coef - qnorm(0.975) * df$robust_se) * 100
  df$robust_coef_uci <- (df$robust_coef + qnorm(0.975) * df$robust_se) * 100
  
  # Define plot ----
  plot <- ggplot(df, mapping = aes(
    x = threshold_year,
    y = robust_est,
    ymin = robust_coef_lci,
    ymax = robust_coef_uci,
    shape = primary
  )) +
    geom_hline(yintercept = 0, colour = "darkgrey") +
    geom_pointrange() +
    geom_point() +
    geom_text(
      aes(
        y = robust_coef_uci + 0.15,
        label = sprintf(
          "%.2f\n(%.2f, %.2f)",
          robust_est,
          robust_coef_lci,
          robust_coef_uci
        )
      ),
      size = 2.5
    ) +
    labs(
      y = glue("Effect of being eligible for {vaccine_name} on {outcome}"),
      x = "Threshold year"
    ) +
    scale_x_continuous(breaks = seq(2010, 2016, 3),
                       lim = c(2010,2016)) +
    guides(shape = "none") +
    theme_classic()
  
  # Save plot ----
  ggsave(
    filename = file.path(output_dir, glue::glue("thresholdyr_{outcome}.png")),
    plot = plot,
    height = 210,
    width = 210,
    units = "mm",
    dpi = 600,
    scale = 0.8
  )
  
  # Return plot ----
  return(plot)
  
}

# Make plots ----

plot_thresholdyr(outcome = "shingles")
plot_thresholdyr(outcome = "dementia")
plot_thresholdyr(outcome = "neuralgia")
plot_thresholdyr(outcome = "all-cause death")
