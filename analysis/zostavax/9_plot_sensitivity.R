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

plot_sensitivity <- function(rd = "sharp", vaccine_name = "zostavax", outcome = "dementia") {
    
  # Filter estimates ----
  df <- df[
    df$rd == rd &
      is.na(df$end_date_years) &
      df$outcome==labels[labels$name==outcome,]$variable &
      df$population!="reduced_excl",
  ]
  
  # Label analysis group ----
  df <- merge(df, labels[labels$type=="analysis_group",], 
              by.x = "analysis_group", by.y = "variable", all.x = TRUE)
  
  df <- dplyr::rename(df, "analysis_group_name" = "name")
  df$type <- NULL
  df$analysis_group_name <- ifelse(df$analysis_group=="alt_msebw", paste0(df$analysis_group_name,df$msebw_multiplier),df$analysis_group_name)

  # Label outcomes ----
  df <- merge(df, labels[labels$type=="outcome",c("name","variable")], 
              by.x = "outcome", by.y = "variable", all.x = TRUE)
  
  # Tidy estimates ----
  df$robust_est <- df$robust_coef * 100
  df$robust_coef_lci <- (df$robust_coef - qnorm(0.975) * df$robust_se) * 100
  df$robust_coef_uci <- (df$robust_coef + qnorm(0.975) * df$robust_se) * 100
  
  # Order sensitivity analyses ----
  df <- df %>%
    arrange(analysis_group_order) %>%
    mutate(
      analysis_group_name = factor(
        analysis_group_name,
        levels = analysis_group_name
      )
    )
  
  # Annotate ----
  title <- ifelse(rd=="sharp","being eligible for ","")
  
  # Define plot ----
  plot <- ggplot(
    df,
    aes(x = robust_est, y = forcats::fct_rev(analysis_group_name), xmin = robust_coef_lci, xmax = robust_coef_uci,)
  ) +
    geom_vline(
      aes(xintercept = 0),
      linetype = "dashed",
      colour = "darkred"
    ) +
    geom_point() +
    geom_pointrange() +
    geom_text(
      aes(
        label = sprintf(
          "%.2f (%.2f, %.2f)",
          robust_est,
          robust_coef_lci,
          robust_coef_uci
        )
      ),
      nudge_y = 0.3,
      size = 3
    ) +
    scale_x_continuous(breaks = seq(-100,100,1)) +
    theme_classic() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 11,
        face = "plain"
      ),
      strip.background = element_blank()
    ) +
    labs(
      y = "",
      x = glue("Effect of {title}{vaccine_name} on {outcome}")
    )
  
  # Save plot ----
  ggsave(
    filename = file.path(output_dir, glue::glue("sensitivity_{rd}_{outcome}.png")),
    plot = plot,
    height = 210,
    width = 1.5 * 210,
    units = "mm",
    dpi = 600,
    scale = 0.8
  )

  # Return plot ----
  return(plot)
  
}

# Make plots ----

plot_sensitivity(rd = "sharp", outcome = "shingles")
plot_sensitivity(rd = "sharp", outcome = "dementia")
plot_sensitivity(rd = "sharp", outcome = "neuralgia")
plot_sensitivity(rd = "sharp", outcome = "all-cause death")

plot_sensitivity(rd = "fuzzy", outcome = "shingles")
plot_sensitivity(rd = "fuzzy", outcome = "dementia")
plot_sensitivity(rd = "fuzzy", outcome = "neuralgia")
plot_sensitivity(rd = "fuzzy", outcome = "all-cause death")
