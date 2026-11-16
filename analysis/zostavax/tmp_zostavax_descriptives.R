output_dir <- "~/OneDrive - University of Bristol/Papers/zostavax-dementia/output/zostavax/descriptives/main/"

# yearly_cumulative_incidence_by_dob ----

for (event_name in c("Dementia","Shingles","All-cause death","Neuralgia","Zostavax")) {
  
  # load data
  km_data <- read_csv(
    fs::path(
      output_dir,
      glue("yearly_cumulative_incidence_by_dob_{event_name}.csv")
    )
  )
  
  # plot rates at yearly snapshots
  km_plot <-
    ggplot(km_data) +
    geom_point(aes(x = month_of_birth, y = cmlinc)) +
    geom_vline(aes(xintercept = dob_threshold_date), linetype = "dashed") +
    scale_x_date(
      name = "Date of birth",
      labels = ~ scales::label_date("%e %b %y")(.),
      breaks = dob_threshold_date + months(seq(-15, 15) * 12),
      guide = guide_axis(angle = 90)
    ) +
    facet_grid(rows = vars(year)) +
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
    path = output_dir
  )

}

# cumulative_incidence ----

#cumulative_events(df_analysis, "month_of_birth_fct", precision=7, start_date = threshold_date, index_date = threshold_date, 52*7, "zostavax_date_1", "Zostavax by month")

event_name <- "Zostavax by month"
time_horizon <- 364
group <- "month_of_birth_fct"

# load data
df_km <- read_csv(
  fs::path(
    output_dir,
    glue("cumulative_incidence_{event_name}_{time_horizon}.csv")
  )
)

df_km$month_of_birth_fct <- factor(
  df_km$month_of_birth_fct,
  levels = c(
    "Oct '32", "Nov '32", "Dec '32",
    "Jan '33", "Feb '33", "Mar '33", "Apr '33",
    "May '33", "Jun '33", "Jul '33", "Aug '33",
    "Sep '33", "Oct '33", "Nov '33", "Dec '33",
    "Jan '34", "Feb '34", "Mar '34", "Apr '34",
    "May '34", "Jun '34", "Jul '34", "Aug '34",
    "Sep '34"
  )
)

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

plot_km <-
  ggplot(df_time0, aes(group = .data[[group]], colour = .data[[group]], fill = .data[[group]])) +
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
  geom_vline(
    aes(xintercept = as.numeric(as.Date("2014-02-01") - as.Date("2013-09-01"))),
    linetype = "solid",
    colour = "black"
  ) +
  scale_color_discrete() +
  scale_fill_discrete(guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
  coord_cartesian(xlim = c(0, NA)) +
  labs(
    x = "Time",
    y = "Cumulative Incidence",
    colour = "Month of Birth",
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
  width = 297,
  height = 210,
  units = "mm"
)

# Balance table

table_balance_wide <- read_csv(
  fs::path(
    output_dir,
    glue("table_balance.csv")
  )
)

N <- table_balance_wide$N_FALSE
  
table1 <- table_balance_wide[,c("variable_label","variable_level","n_FALSE","n_TRUE")]
table1 <- table1[!grepl("Age",table1$variable_label),]
write_csv(table1,fs::path(
  output_dir,
  glue("table1.csv")
))