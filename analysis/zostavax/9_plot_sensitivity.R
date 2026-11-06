# Load libraries ----
library("here")
library("glue")
library("ggplot2")
library("lubridate")

# Specify parameters ----
vaccine_name <- "zostavax"
outcome <- "neuralgia"

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

# Filter estimates ----

df <- df[
  df$rd == "fuzzy" &
    is.na(df$end_date_years),
]

# Label analysis group ----

df <- merge(df, labels[labels$type=="analysis_group",], 
            by.x = "analysis_group", by.y = "variable", all.x = TRUE)

df <- dplyr::rename(df, "analysis_group_name" = "group", "analysis_group_subname" = "name")
df$type <- NULL
df$analysis_group_subname <- ifelse(df$analysis_group=="alt_msebw", paste0("MSE bandwith x",df$msebw_multiplier),df$analysis_group_subname)
df$analysis_group_subname <- ifelse(df$analysis_group=="alt_kernel", stringr::str_to_title(df$kernel), df$analysis_group_subname)
df$analysis_group_subname <- ifelse(df$analysis_group=="alt_polynomial", df$polynomial, df$analysis_group_subname)

# Label outcomes ----

df <- merge(df, labels[labels$type=="outcome",c("name","variable")], 
            by.x = "outcome", by.y = "variable", all.x = TRUE)

# Tidy estimates ----
df$robust_est <- df$robust_coef * 100
df$robust_coef_lci <- (df$robust_coef - qnorm(0.975) * df$robust_se) * 100
df$robust_coef_uci <- (df$robust_coef + qnorm(0.975) * df$robust_se) * 100

# Define plot ----

df_plot <- df[df$outcome==labels[labels$name==outcome,]$variable,]

plot <- ggplot(
  df_plot,
  aes(x = robust_est, y = analysis_group_subname, xmin = robust_coef_lci, xmax = robust_coef_uci,)
) +
  geom_vline(
    aes(xintercept = 0),
    linetype = "dashed",
    colour = "darkred"
  ) +
  geom_point() +
  geom_pointrange() +
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
    x = glue("Effect of being eligible for {vaccine_name} on {outcome}")
  ) +
  facet_wrap(analysis_group_name~., ncol=1, scales="free_y")

# Save plot ----
ggsave(
  filename = file.path(output_dir, glue::glue("sensitivity_{outcome}.png")),
  plot = plot,
  height = 210,
  width = 1.5 * 210,
  units = "mm",
  dpi = 600,
  scale = 0.8
)