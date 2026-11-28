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

first_date_ever <- function(data, condition) {
  
  cols <- c(
    paste0(condition, "_gp_first_date_ever"),
    paste0(condition, "_hosp_first_date_ever"),
    paste0(condition, "_ons_date")
  )

  do.call(pmin, c(data[cols], list(na.rm = TRUE)))
}

first_date_after <- function(data, condition) {
  
  cols <- c(
    paste0(condition, "_gp_first_date_after"),
    paste0(condition, "_hosp_first_date_after"),
    paste0(condition, "_ons_date")
  )

  do.call(pmin, c(data[cols], list(na.rm = TRUE)))
}

last_date_before <- function(data, condition) {
  
  cols <- c(
    paste0(condition, "_gp_last_date_before"),
    paste0(condition, "_hosp_last_date_before")
  )

  do.call(pmin, c(data[cols], list(na.rm = TRUE)))
}