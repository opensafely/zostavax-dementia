library("gtsummary")

## Create table1-style summary of characteristics, with SDC applied ----

table1_summary <- function(.data, group, label, threshold) {

  ## this function is highly dependent on the structure of tbl_summary object internals!
  ## be careful if this package is updated

  group_quo <- enquo(group)

  ## create a table of baseline characteristics between each treatment group
  tab_summary <-
    .data |>
    select(
      !!group_quo,
      any_of(names(label)),
    ) %>%
    gtsummary::tbl_summary(
      by = !!group_quo,
      label = label[names(label) %in% names(.)],
      statistic = list(
        # N ~ "{N}",
        all_categorical() ~ "{n} ({p}%)",
        all_continuous() ~ "{mean} ({sd}); ({p10}, {p25}, {median}, {p75}, {p90})"
      )
    )

  ## extract structured info from tbl_summary object to apply SDC to the counts
  raw_stats <-
    tab_summary$cards$tbl_summary |>
    mutate(
      variable = factor(variable, levels = names(label)),
      variable_label = factor(variable, levels = names(label),  labels = label),
    ) |>
    filter(!(context %in% c("missing", "attributes", "total_n"))) |>
    select(-warning, -error, -gts_column) |>
    pivot_wider(
      id_cols = c("group1", "group1_level", "variable", "variable_label", "variable_level", "context"),
      names_from = stat_name,
      values_from = stat
    ) |>
    mutate(
      across(
        c(n, N),
        ~ {
          map_int(., ~ {
            if (is.null(.)) NA else as.integer(.)
          })
        }
      ),
      across(
        c(p, median, p10, p25, p75, p90, mean, sd),
        ~ {
          map_dbl(., ~ {
            if (is.null(.)) NA else as.numeric(.)
          })
        }
      ),
      across(
        c(group1_level, variable_level),
        ~ {
          map_chr(., ~ {
            if (is.null(.)) NA else as.character(.)
          })
        }
      ),
    ) |>
    arrange(variable_label, group1, group1_level)

  raw_stats_redacted <-
    raw_stats |>
    mutate(
      n = sdc.rounding(n, threshold),
      N = sdc.rounding(N, threshold),
      p = n / N,
      mean = if_else(n == 0, NA_real_, mean),
      sd = if_else(n == 0, NA_real_, sd),
      p10 = if_else(n == 0, NA_real_, p10),
      p25 = if_else(n == 0, NA_real_, p25),
      median =  if_else(n == 0, NA_real_, median),
      p75 = if_else(n == 0, NA_real_, p75),
      p90 = if_else(n == 0, NA_real_, p90),
    ) 

  return(raw_stats_redacted)
}



## get Kaplan Meier estimate for a specific time horizon quickly without dipping into survival package ----
km_at_t <- function(event_time, event_indicator, time_horizon) {
 
  unique_event_times <- sort(unique(event_time[event_indicator == 1 & event_time <= time_horizon]))
  
  if (length(unique_event_times) == 0) {
    return(1.0)
  }
  
  # create product terms for KM 
  product_terms <- sapply(
    unique_event_times, 
    function(ti) {
      n.risk <- sum(event_time >= ti)   # n at risk at time ti
      n.events  <- sum(event_time == ti & event_indicator == 1)   # n experiencing event at ti
      return(1 - (n.events / n.risk)) # terms to be multiplied
    }
  )
  
  return(prod(product_terms))
}

## get Kaplan Meier estimates for each day up to time horizon ----
# heavily inspired by the KM reusable OpenSAFELY action https://github.com/opensafely-actions/kaplan-meier-function/blob/main/analysis/km.R
km <- function(.data, group_col, start_date, precision = 1, time_horizon, event_date_col, censor_date_col) {

  time_horizon <- ceiling_any(time_horizon, precision) # convert time_horizon in days to lower precision if needed
  start_date <- start_date - 1L # if events occur on same day as start date, that's ok, treat as event_time=1

  df_tte <-
    .data |>
    transmute(
      group = .data[[group_col]],
      event_date = .data[[event_date_col]],
      censor_date = .data[[censor_date_col]],
      event_time = as.integer(pmin(event_date, censor_date, start_date + time_horizon, na.rm=TRUE) - start_date),
    ) |>
   mutate(
    event_time = ceiling_any(event_time, precision),
    event_indicator = event_time < time_horizon,
   )

  times_count <- table(cut(df_tte$event_time, c(-Inf, 0, 1, Inf), right=FALSE, labels= c("<0", "0", ">0")), useNA="ifany")

  if(!identical(as.integer(times_count), c(0L, 0L, nrow(df_tte)))) {
    print(times_count)
    stop("all event times must be strictly positive")
  }

  ## Calculate KM estimates -----

  ## Run `survfit` across each level of exposure and subgroup ----
  ## do this independently rather than using stratification or covariates
  ## because it makes variable name handling easier

  # for each group level, pass data through `survival::survfit` to get KM table
  df_km <-
    df_tte |>
    group_by(group) |>
    nest() |>
    mutate(
      surv_obj_tidy = purrr::map(data, ~ {
        survival::survfit(
          survival::Surv(event_time, event_indicator) ~ 1,
          data = .x,
          conf.type="log-log"
        ) |>
        broom::tidy() |>
        tidyr::complete(
          time = seq_len(time_horizon), # fill in 1 row for each day of follow up
          fill = list(n.event = 0L, n.censor = 0L) # fill in zero events on those days
        ) |>
        tidyr::fill(n.risk, .direction = c("up"))
      }),
    ) |>
    select(-data) |>
    tidyr::unnest(surv_obj_tidy)

  ## Round the count values in the survival data ----
  # round event times such that no event time has fewer than `min_count` events
  # recalculate KM estimates based on these rounded event times


  df_km_rounded <- 
    df_km |>
    mutate(
      surv = cumprod(1 - n.event / n.risk),
      ## standard errors on complementary log-log scale
      surv.cll = log(-log(surv)), # this is equivalent to the log cumulative hazard
      summand = (1 / (n.risk - n.event)) - (1 / n.risk), # = n.event / ((n.risk - n.event) * n.risk) but re-written to prevent integer overflow
      surv.cll.se = if_else(surv==1, 0, sqrt((1 / log(surv)^2) * cumsum(summand))), # assume SE is zero until there are events -- makes plotting easier
      surv.low = exp(-exp(surv.cll + qnorm(0.975) * surv.cll.se)),
      surv.high = exp(-exp(surv.cll + qnorm(0.025) * surv.cll.se)),
      #cumulative incidence (= complement of survival)
      cmlinc = 1 - surv,
      cmlinc.low = 1 - surv.high,
      cmlinc.high = 1 - surv.low,
    ) |>
    group_by(group) |>
    transmute(
      group,
      time,

      # multiply cumulative incidence by (rounded) baseline population size, apply rounding, and rescale
      n_rounded = sdc.rounding(max(n.risk), sdc.threshold),
      cmlinc = sdc.rounding(cmlinc*n_rounded, sdc.threshold) / n_rounded, 
      cmlinc.low = sdc.rounding(cmlinc.low*n_rounded, sdc.threshold) / n_rounded, 
      cmlinc.high = sdc.rounding(cmlinc.high*n_rounded, sdc.threshold) / n_rounded, 
    )
  
  return(df_km_rounded)
}



# report any discontinuities in events occurring either side of the threhold_date, by reporting outcome rates by month_of_birth ----
# use the "X_before_threshold" columns as the event_col for this function
check_discontinuity_pre <- function(.data, dob_threshold_date, threshold_date, event_col, event_name) {
  
  dat_summary <-
    .data |>
    transmute(
      month_of_birth,
      event = .data[[event_col]],
     ) |>
    group_by(month_of_birth) |>
    summarise(
      n =n(),
      event_n = sum(event),
      event_rate = mean(event)
    ) |>
    ungroup() |> 
    # apply SDC
    mutate(
      n = sdc.rounding(n, sdc.threshold),
      event_n = sdc.rounding(event_n, sdc.threshold),
      event_rate = event_n / n
    ) 

  plot_summary <-
    ggplot(dat_summary) +
    geom_point(aes(x = month_of_birth, y = event_rate))+
    geom_vline(aes(xintercept = dob_threshold_date), linetype="dashed")+
    scale_x_date(
      
      # Three possible options for scale of horizontal axis 
      name = "Date of birth", labels = ~ scales::label_date("%d %b %y")(.), breaks = dob_threshold_date + months(seq(-10,10)*12),
      #name = glue("Month of birth (relative to {scales::label_date('%d %b %y')(dob_threshold_date)})"), labels = ~ interval(dob_threshold_date, .) %/% months(1), breaks = dob_threshold_date + months(seq(-10,10)*6),
      #name = glue("Age at {scales::label_date('%d %b %y')(threshold_date)}"), labels = ~ (interval(., threshold_date) %/% months(1))/12, breaks = dob_threshold_date + months(seq(-10,10)*6),
        
    )+
    scale_y_continuous(labels = scales::label_percent())+
    labs(
      y = glue("Proportion")
    )+
    theme_bw()
  
  
  print(plot_summary)

  # save to disk
  ggsave(plot_summary, filename=glue("discontinuity_pre_{event_name}.png"), path=output_dir)
}


# report the cumulative incidence of events by age either side of threshold date (or by month_of_birth) ----
# use the "X_first_date_after" column as the event_date_col, or the "X_first_date_ever" column with a filter on the inputted data to ensure this event did not occur prior to baseline
cumulative_events <- function(.data, group, precision, time_horizon, event_date_col, event_name){
  
  df_km <- km(.data, group, threshold_date, precision, time_horizon, event_date_col, "censor_date")

  write_csv(df_km, fs::path(output_dir, glue("cumulative_incidence_{event_name}_{time_horizon}.csv")))

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
    ggplot(df_time0, aes(group = group, colour = group, fill = group)) +
    geom_step(aes(x = time, y = cmlinc), direction = "vh") +
    geom_step(aes(x = time, y = cmlinc), direction = "vh", linetype = "dashed", alpha = 0.5) +
    geom_rect(aes(xmin = lagtime, xmax = time, ymin = cmlinc.low, ymax = cmlinc.high), alpha = 0.1, colour = "transparent") +
    scale_color_discrete() +
    scale_fill_discrete(guide = "none") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.01))) +
    coord_cartesian(xlim = c(0, NA)) +
    labs(
      x = "Time",
      y = "Cumulative Incidence",
      colour = NULL,
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
  
  ggsave(plot_km, filename=glue("cumulative_incidence_{event_name}_{time_horizon}.png"), path=output_dir, width = 15, height = 15, units = "cm")

}



# report any discontinuities in events occurring either side of the threhold_date, by reporting outcome rates by month_of_birth ----
# use the "X_first_date_after" column as the event_date_col, or the "X_first_date_ever" column with a filter on the inputted data to ensure this event did not occur prior to baseline
check_discontinuity_post <- function(.data, dob_threshold_date, threshold_date, time_horizon, event_date_col, event_name) {
  
  threshold_date <- threshold_date - 1L # so that events occurring on the threshold date are not excluded 

  dat_summary <-
    .data |>
    transmute(
      month_of_birth,
      day_diff_threshold,
      event_date = .data[[event_date_col]],
      censor_date = pmin(reg_end_date, date_of_death, na.rm=TRUE),
      event_time = as.integer(pmin(event_date, censor_date, threshold_date + time_horizon, na.rm=TRUE) - threshold_date),
      event_indicator = event_time < time_horizon
    ) |>
    group_by(month_of_birth) |>
    summarise(
      n = n(),
      cmlinc = 1 - km_at_t(event_time = event_time, event_indicator = event_indicator, time_horizon = time_horizon)
    ) |> 
    ungroup() |>
    # apply SDC
    mutate(
      n = sdc.rounding(n, sdc.threshold),
      cmlinc = sdc.rounding(cmlinc*n, sdc.threshold) / n, 
    )

  plot_summary <-
    ggplot(dat_summary) +
    geom_point(aes(x = month_of_birth, y = cmlinc))+
    geom_vline(aes(xintercept = dob_threshold_date), linetype="dashed")+
    scale_x_date(
      
      # Three possible options for scale of horizontal axis 

      name = "Date of birth", labels = ~ scales::label_date("%d %b %y")(.), breaks = dob_threshold_date + months(seq(-10,10)*12),
      #name = glue("Month of birth (relative to {scales::label_date('%d %b %y')(dob_threshold_date)})"), labels = ~ interval(dob_threshold_date, .) %/% months(1), breaks = dob_threshold_date + months(seq(-10,10)*6),
      #name = glue("Age at {scales::label_date('%d %b %y')(threshold_date)}"), labels = ~ (interval(., threshold_date) %/% (months(1))/12, breaks = dob_threshold_date + months(seq(-10,10)*6),
      
      ## For some reason using a secondary axis like this doesn't work! very frustrating
      #sec.axis = sec_axis(
      #  name = glue("Age at {scales::label_date('%d %b %y')(threshold_date)}"), 
      #  transform = ~ . , 
      #  labels = ~ (interval(., threshold_date) %/% (months(1)))/12,
      #  breaks = dob_threshold_date + months(seq(-10,10)*6)
      #)
    )+
    scale_y_continuous(labels = scales::label_percent())+
    labs(
      y = glue("Cumulative incidence of event within {time_horizon} days")
    )+
    theme_bw()
  
  
  print(plot_summary)

  ggsave(plot_summary, filename=glue("discontinuity_{event_name}_{time_horizon}.png"), path=output_dir)
}