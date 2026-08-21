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