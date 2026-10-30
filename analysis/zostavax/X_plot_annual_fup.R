# Load libraries ----
library("here")
library("glue")
library("ggplot2")

# Specify parameters ----
vaccine_name <- "zostavax"
outcome <- "dementia"

# Create rd_plots directory ----
output_dir <- here("output", vaccine_name, "rd_plots")
fs::dir_create(output_dir)

# Source functions ----
source(here::here("analysis", "common_code", "utility.R"))

# Load analyses ----
df <- read.csv(glue("output/{vaccine_name}/output/rd_results_rounded.csv"))

# Filter ----
df <- df[df$analysis_group %in% c("annual_fup") & df$outcome %in% c("dementia_first_date_ever") & df$rd=="fuzzy",]

# Annotate ----
df$primary <- ifelse(df$end_date_years==12, "Primary", "Secondary")
df$robust_coef_lci <- df$robust_coef - qnorm(0.975)*df$robust_se
df$robust_coef_uci <- df$robust_coef + qnorm(0.975)*df$robust_se

# Plot ----
ggplot(df, mapping = aes(
  x = end_date_years,
  y = robust_coef,
  ymin = robust_coef_lci,
  ymax = robust_coef_uci,
  shape = primary
)) +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_pointrange() +
  geom_point() +
  labs(
    y = glue("Effect of {vaccine_name} on {outcome} across different follow-up periods"),
    x = "Follow-up period (years)"
  ) +
  scale_colour_manual(values = bespoke_colours) +
  scale_x_continuous(breaks = seq(min(df$end_date_years), max(df$end_date_years), 1), 
                     lim = c(min(df$end_date_years), max(df$end_date_years))) +
  guides(shape = "none") +
  theme_bespoke()