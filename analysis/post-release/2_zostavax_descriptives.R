# load packages ----
library("tidyverse")
library("arrow")
library("here")
library("glue")
library("survival")

# Specify analysis parameters ----

# analysis specific parameters
vaccine_name <- "zostavax"
analysis <- "main" # main, 2010, 2016

# ensure skim outputs do not wrap to new line
options(width = 200)

# define threshold dates
if (analysis == "main") {
  dob_threshold_date <- as.Date("1933-09-01")
  threshold_date <- as.Date("2013-09-01")
  end_date <- threshold_date + years(12) - days(1)
}

if (analysis == "2010") {
  dob_threshold_date <- as.Date("1933-09-01") + years(-3)
  threshold_date <- as.Date("2013-09-01") + years(-3)
  end_date <- threshold_date + years(12) - days(1)
}

if (analysis == "2016") {
  dob_threshold_date <- as.Date("1933-09-01") + years(3)
  threshold_date <- as.Date("2013-09-01") + years(3)
  end_date <- threshold_date + years(12) - days(1)
}

index_date <- threshold_date + months(5)

# run functions ----
source(here::here("analysis", "common_code", "utility.R"))
source(here::here("analysis", "common_code", "descriptives.R"))


# select location of outputted files to import
#input_dir <- here("output", vaccine_name, "descriptives", analysis)
input_dir <- here("release", vaccine_name, "descriptives", analysis)

# create output directory ----
output_dir <- here("release", vaccine_name, "descriptives", analysis)
fs::dir_create(output_dir)


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Cumulative incidence of vaccination since index date ----
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #



plot_cumulative_events <- function(
  group,
  time_horizon,
  event_name
){

  df_km <- read_csv(
    file = fs::path(input_dir, glue("cumulative_incidence_{event_name}_{time_horizon}.csv"))
  ) |>
  mutate(
    group = .data[[group]]
  ) |>
  group_by(group)


  # add estimate at time zero (where cml_inc=0 - this helps with plotting)
  df_time0 <-
    df_km |>
    mutate(
      lagtime = lag(time, 1, 0), # assumes the time-origin is zero
    ) %>%
    group_modify(
      ~ add_row(
        .x,
        time = 0, # assumes time origin is zero
        lagtime = 0,
        cmlinc = 0,
        cmlinc.low = 0,
        cmlinc.high = 0,
        .before = 0
      )
    )
  
  # plot cumulative incidence of events up to the time horizon
  plot_km <-
    ggplot(df_time0, aes(group = group, colour = group, fill = group)) +
    geom_step(aes(x = time, y = cmlinc), direction = "vh") +
    geom_step(
      aes(x = time, y = cmlinc),
      direction = "vh",
      linetype = "dashed",
      alpha = 0.5
    ) +
    geom_rect(
      aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high),
      alpha = 0.1,
      colour = "transparent"
    ) +
    scale_color_discrete() +
    scale_fill_discrete(guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Time",
      y = "Cumulative Incidence",
      colour = group,
      fill = NULL,
      title = NULL
    ) +
    theme_minimal() +
    theme(
      axis.line.x = element_line(colour = "black"),
      panel.grid.minor.x = element_blank(),
      legend.position = "inside",
      legend.position.inside = c(.05, .95),
      legend.justification = c(0, 1),
    )

  print(plot_km)

  
  # save plot
  ggsave(
    plot_km,
    filename = glue("cumulative_incidence_{event_name}_{time_horizon}.png"),
    path = output_dir,
    width = 15,
    height = 15,
    units = "cm"
  )
}

# Cumulative uptake of Zostavax by age in months up to one year after the threshold date
plot_cumulative_events("month_of_birth_fct", 52*7, "Zostavax by month")




# report outcome rates by month_of_birth, at yearly snapshot dates ----
# use the "X_first_date_after" column as the event_date_col, or the "X_first_date_ever" column with a filter on the inputted data to ensure this event did not occur prior to baseline
plot_yearly_cumulative_incidence_by_dob <- function(
  dob_threshold_date,
  event_name
) {

    # read data
  km_data <- read_csv(
    file = fs::path(
      input_dir,
      glue("yearly_cumulative_incidence_by_dob_{event_name}.csv")
    )
  )
  
  # plot rates at yearly snapshots
  km_plot <-
    ggplot(km_data) +
    geom_point(aes(x = month_of_birth, y = cmlinc)) +
    geom_vline(aes(xintercept = dob_threshold_date), linetype = "dashed") +
    geom_vline(aes(xintercept = as.Date("1942-09-01")), linetype = "dashed") +
    scale_x_date(
      name = "Date of birth",
      labels = ~ scales::label_date("%e %b %y")(.),
      breaks = dob_threshold_date + months(seq(-15, 15) * 12),
      guide = guide_axis(angle = 90)
    ) +
    facet_wrap(vars(year)) +
    scale_y_continuous(labels = scales::label_percent()) +
    labs(
      y = glue("Cumulative incidence"),
    ) +
    theme_bw()

  print(km_plot)

  # save plot
  ggsave(
    km_plot,
    filename = glue("yearly_cumulative_incidence_by_dob_{event_name}.png"),
    path = output_dir,
    width = 10,
    height = 20,
    units = "cm"
  )
}


# Cumulative incidence of outcomes by month of birth, up to 7 years after threshold

# zostavax - start follow up immeidately after threshold
plot_yearly_cumulative_incidence_by_dob(dob_threshold_date, "Zostavax")

# outcomes - start follow up from index date
plot_yearly_cumulative_incidence_by_dob(dob_threshold_date, "Shingles")
plot_yearly_cumulative_incidence_by_dob(dob_threshold_date, "Dementia")
plot_yearly_cumulative_incidence_by_dob(dob_threshold_date, "Neuralgia")
plot_yearly_cumulative_incidence_by_dob(dob_threshold_date, "All-cause death")
