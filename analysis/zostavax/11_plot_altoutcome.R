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

plot_altoutcome <- function(rd = "sharp", vaccine_name = "zostavax", outcome_group = "subtype", outcome = "") {
    
  # Process outcome_group ----
  altoutcome <- outcome
  if (outcome_group=="subtype") {
    keep <- labels[labels$outcome_group==outcome_group | labels$name=="dementia",]$variable
  }
  if (outcome_group=="alternative") {
    keep <- labels[labels$outcome_group %in% c(outcome_group,"primary"),]$variable
    keep <- keep[grepl(outcome,keep)]
    altoutcome <- paste0("_",outcome)
  }
  if (outcome_group=="control") {
    keep <- labels[labels$outcome_group==outcome_group,]$variable
  }
  
  # Filter estimates ----
  df <- df[
    df$analysis_group %in% c("primary","alt_outcome") &
    df$rd == rd &
      is.na(df$end_date_years) &
      df$outcome %in% keep &
      df$population!="reduced_excl",
  ]
  
  # Label outcomes ----
  df <- merge(df, labels[labels$type=="outcome",c("name","variable")], 
              by.x = "outcome", by.y = "variable", all.x = TRUE)
  
  # Tidy estimates ----
  df$primary <- ifelse(df$analysis_group=="primary", "Primary", "Secondary")
  df$robust_est <- df$robust_coef * 100
  df$robust_coef_lci <- (df$robust_coef - qnorm(0.975) * df$robust_se) * 100
  df$robust_coef_uci <- (df$robust_coef + qnorm(0.975) * df$robust_se) * 100

  # Annotate ----
  title <- ifelse(rd=="sharp","being eligible for ","")
  
  # Define plot ----
  plot <- ggplot(
    df,
    aes(x = robust_est, y = forcats::fct_rev(name), xmin = robust_coef_lci, xmax = robust_coef_uci, shape = primary)
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
    guides(shape = "none") +
    labs(
      y = "",
      x = glue("Effect of {title}{vaccine_name}")
    )
  
  # Save plot ----
  ggsave(
    filename = file.path(output_dir, glue::glue("altoutcome_{rd}_{outcome_group}{altoutcome}.png")),
    plot = plot,
    height = 297*(length(keep)/17),
    width = 250,
    units = "mm",
    dpi = 600,
    scale = 0.8
  )

  # Return plot ----
  return(plot)
  
}

# Make plots ----

plot_altoutcome(rd = "sharp", outcome_group = "control")
plot_altoutcome(rd = "sharp", outcome_group = "subtype")
plot_altoutcome(rd = "sharp", outcome_group = "alternative", outcome = "shingles")
plot_altoutcome(rd = "sharp", outcome_group = "alternative", outcome = "dementia")
plot_altoutcome(rd = "sharp", outcome_group = "alternative", outcome = "neuralgia")
plot_altoutcome(rd = "sharp", outcome_group = "alternative", outcome = "death")

plot_altoutcome(rd = "fuzzy", outcome_group = "control")
plot_altoutcome(rd = "fuzzy", outcome_group = "subtype")
plot_altoutcome(rd = "fuzzy", outcome_group = "alternative", outcome = "shingles")
plot_altoutcome(rd = "fuzzy", outcome_group = "alternative", outcome = "dementia")
plot_altoutcome(rd = "fuzzy", outcome_group = "alternative", outcome = "neuralgia")
plot_altoutcome(rd = "fuzzy", outcome_group = "alternative", outcome = "death")