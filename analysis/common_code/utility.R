sdc.rounding <- function(x, threshold){
  case_when(
    x <= 7 ~ 0,
    x > 7 ~ round(x / threshold) * threshold
  )
}

roundmid_any <- function(x, to = 1) {
  # like ceiling_any, but centers on (integer) midpoint of the rounding points
  ceiling(x / to) * to - (floor(to / 2) * (x != 0))
}


floor_any <- function(x, to = 1) {
  x - x %% to
}


ceiling_any <- function(x, to = 1) {
  # round to nearest 100 millionth to avoid floating point errors
  ceiling(plyr::round_any(x / to, 1 / 100000000)) * to
}


# rounding precision
sdc.threshold <- 5
# ggplot styling
library(ggplot2)
theme_bespoke <- function(base_size = 11, base_family = "Arial") {
  
  theme_classic(base_size = base_size, 
                base_family = base_family) +
    
    theme(
      # Background
      plot.background = element_rect(
        fill = "white", colour = NA
      ),
      panel.background = element_rect(
        fill = "white", colour = NA
      ),
      
      # Axes
      axis.line = element_line(
        colour = "black", linewidth = 0.5
      ),
      axis.ticks = element_line(
        colour = "black", linewidth = 0.4
      ),
      axis.text = element_text(
        colour = "black", size = base_size
      ),
      axis.title = element_text(
        colour = "black", size = base_size
      ),
      
      # Titles
      plot.title = element_text(
        face = "bold", size = base_size + 2,
        colour = "black", hjust = 0
      ),
      plot.subtitle = element_text(
        size = base_size, colour = "black"
      ),
      
      # Legend
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold", size = base_size
      ),
      legend.text = element_text(
        size = base_size - 1, colour = "black"
      ),
      legend.key = element_blank(),
      
      # Grid
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Margins
      plot.margin = margin(10, 12, 10, 10)
    )
}

