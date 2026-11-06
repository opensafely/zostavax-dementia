# Load libraries ----
library("here")
library("glue")
library("ggplot2")
library("lubridate")

# Specify parameters ----
vaccine_name <- "zostavax"
outcome <- "dementia"

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

# Filter ----
df <- df[df$analysis_group %in% c("annual_fup") & df$rd=="fuzzy",]

# Define plot_annual_fup function ----
plot_annual_fup <- function(vaccine_name = "zostavax", outcome = "dementia") {
  
  # Restrict to outcome ----
  outcome_var <- labels[labels$name==outcome,]$variable
  df <- df[df$outcome==outcome_var,]
  
  # Annotate ----
  df$primary <- ifelse(df$end_date_years==12, "Primary", "Secondary")
  df$robust_est <- df$robust_coef * 100
  df$robust_coef_lci <- (df$robust_coef - qnorm(0.975) * df$robust_se) * 100
  df$robust_coef_uci <- (df$robust_coef + qnorm(0.975) * df$robust_se) * 100
  
  # Define plot ----
  plot <- ggplot(df, mapping = aes(
    x = end_date_years,
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
      x = "Follow-up period (years)"
    ) +
    scale_x_continuous(breaks = seq(min(df$end_date_years), max(df$end_date_years), 1), 
                       lim = c(min(df$end_date_years), max(df$end_date_years))) +
    scale_y_continuous(lim = c(-3,2), breaks = seq(-100,100,1)) +
    guides(shape = "none") +
    theme_classic()
  
  # Save plot ----
  ggsave(
    filename = file.path(output_dir, glue::glue("annual_fup_{outcome}.png")),
    plot = plot,
    height = 210,
    width = 297,
    units = "mm",
    dpi = 600,
    scale = 0.8
  )
  
  # Return plot ----
  return(plot)

}

# Make plots ----

plot_annual_fup(outcome = "dementia")
plot_annual_fup(outcome = "shingles")
plot_annual_fup(outcome = "all-cause death")
plot_annual_fup(outcome = "neuralgia")