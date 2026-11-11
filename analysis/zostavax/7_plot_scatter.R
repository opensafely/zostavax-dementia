# Load libraries ----
library("here")
library("glue")
library("ggplot2")
library("lubridate")

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
df_est <- read.csv(glue::glue(
  release_dir,
  "{vaccine_name}/output/rd_results_rounded.csv"
))

# Load bandwidth ----
df_msebw <- read.csv(glue::glue(
  release_dir,
  "{vaccine_name}/setup/rd_msebw.csv"
))

# Define plot_rd function ----
plot_rd <- function(rd = "fuzzy", vaccine_name = "zostavax", outcome = "dementia", dob_threshold_date = as.Date("1933-09-01"), bandwidth = df_msebw$h_left, time_horizon = 7, ymin = 15, ymax = 30, ytxt = 23) {
  
  ## Load incidence ----
  outcome_title <- stringr::str_to_title(outcome)
  df_points <- read.csv(glue::glue(
    release_dir,
    "{vaccine_name}/descriptives/main/yearly_cumulative_incidence_by_dob_{outcome_title}.csv"
  ))
  
  ## Calculate relative month ----
  df_points$relative_month <- lubridate::time_length(
    lubridate::interval(dob_threshold_date, df_points$month_of_birth),
    unit = "months"
  )
  
  ## Calculate weight of month ----
  df_points$weight <- pmax(
    0,
    1 - abs(df_points$relative_month) / bandwidth
  )
  
  ## Filter points to plot ----
  df_points <- df_points[df_points$year == time_horizon & df_points$relative_month!= 0, ]
  
  ## Identify estimates for plot ----
  plot_est <- df_est[
    !is.na(df_est$end_date_years) &
      df_est$end_date_years == time_horizon &
      df_est$outcome == labels[labels$name == outcome, ]$variable &
      df_est$rd == rd,
  ]
  effect <- round(plot_est$robust_coef * 100, 1)
  lci <- round(
    (plot_est$robust_coef - qnorm(0.975) * plot_est$robust_se) * 100, 1
  )
  uci <- round(
    (plot_est$robust_coef + qnorm(0.975) * plot_est$robust_se) * 100, 1
  )
  p <- signif(plot_est$robust_p, 2)
  title <- ifelse(rd=="sharp","being eligible for ","")
    
  # Define plot ----
  plot <- ggplot(
    df_points,
    aes(x = relative_month, y = 100 * cmlinc)
  ) +
    geom_point(
      aes(fill = weight),
      shape = 21,
      size = 2,
      colour = "grey20"
    ) +
    geom_vline(
      aes(xintercept = 0),
      linetype = "dashed",
      colour = "darkred"
    ) +
    annotate(
      "text",
      x = 3,
      y = ytxt,
      label = glue::glue(
        "Effect of {title}{vaccine_name} on {outcome}\n{effect} (95% CI = {lci} to {uci}), P = {p}"
      ),
      hjust = 0,
      vjust = 1,
      size = 4,
      colour = "black"
    ) +
    # annotate(
    #   "segment",
    #   x = -16, xend = -24,
    #   y = ymax - 0.5, yend = ymax - 0.5,
    #   arrow = grid::arrow(length = grid::unit(0.2, "cm")),
    #   colour = "grey50"
    # ) +
    annotate(
      "text",
      x = -20, y = ymax,
      label = "Earlier date of birth\n(ineligible)"
    ) +
    # annotate(
    #   "segment",
    #   x = 16, xend = 24,
    #   y = ymax - 0.5, yend = ymax - 0.5,
    #   arrow = grid::arrow(length = grid::unit(0.2, "cm")),
    #   colour = "grey50"
    # ) +
    annotate(
      "text",
      x = 20, y = ymax,
      label = "Later date of birth\n(eligible)"
    ) +
    geom_smooth(
      data = subset(df_points, relative_month < 0),
      method = "lm",
      se = FALSE,
      colour = "black"
    ) +
    geom_smooth(
      data = subset(df_points, relative_month > 0),
      method = "lm",
      se = FALSE,
      colour = "black"
    ) +
    scale_x_continuous(
      breaks = seq(-ceiling(bandwidth), ceiling(bandwidth), 6),
      limits = c(-ceiling(bandwidth), ceiling(bandwidth))
    ) +
    scale_y_continuous(limits = c(ymin, ymax)) +
    labs(
      x = paste0(
        "Month of birth (relative to ",format(dob_threshold_date, "%e %B %Y"),")"
      ),
      y = glue::glue(
        "Probability of a new {outcome} diagnosis over a {time_horizon} year follow-up period (%)"
      )
    ) +
    scale_fill_gradient(
      low = "white",
      high = "grey50",
      limits = c(0, 1)
    ) +
    guides(fill = "none") +
    theme_classic() +
    theme(
      plot.title = element_text(
        hjust = 0.5,
        size = 11,
        face = "plain"
      )
    )
  
  # Save plot ----
  ggsave(
    filename = file.path(output_dir, glue::glue("scatter_{rd}_{outcome}.png")),
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

plot_rd(rd = "sharp", outcome = "dementia", ymin = 15, ymax = 30, ytxt = 24)
plot_rd(rd = "sharp", outcome = "shingles", ymin = 2, ymax = 10, ytxt = 5.5)
plot_rd(rd = "sharp", outcome = "all-cause death", ymin = 25, ymax = 50, ytxt = 40)
plot_rd(rd = "sharp", outcome = "neuralgia", ymin = 0, ymax = 3, ytxt = 1)

plot_rd(rd = "fuzzy", outcome = "dementia", ymin = 15, ymax = 30, ytxt = 24)
plot_rd(rd = "fuzzy", outcome = "shingles", ymin = 2, ymax = 10, ytxt = 5.5)
plot_rd(rd = "fuzzy", outcome = "all-cause death", ymin = 25, ymax = 50, ytxt = 40)
plot_rd(rd = "fuzzy", outcome = "neuralgia", ymin = 0, ymax = 3, ytxt = 1)