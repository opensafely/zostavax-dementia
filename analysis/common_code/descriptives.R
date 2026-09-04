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



# get Kaplan Meier estimate for a specific time horizon quickly without dipping into survival package
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

# get Kaplan Meier estimates for each day up to time horizon
# heavily inspired by the KM reusable OpeNSAFELY action https://github.com/opensafely-actions/kaplan-meier-function/blob/main/analysis/km.R
km <- function(.data, group_col, index_date, precision = 1, time_horizon, event_date_col, censor_date_col) {

  time_horizon <- ceiling_any(time_horizon, precision) # convert time_horizon in days to lower precision if needed
  index_date <- index_date - 1L # if events occur on same day as index date, that's ok, treat as event_time=1

  df_tte <-
    .data |>
    transmute(
      group = .data[[group_col]],
      event_date = .data[[event_date_col]],
      censor_date = .data[[censor_date_col]],
      event_time = as.integer(pmin(event_date, censor_date, index_date + time_horizon, na.rm=TRUE) - index_date),
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
