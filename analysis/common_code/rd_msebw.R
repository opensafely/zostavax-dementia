# Load libraries ----
library(rdrobust)

# Define function ----
rd_msebw <- function(data, dependent, running, end) {
  ## Validate function inputs ----
  rd_input_checks(
    fn = "rd_msebw",
    data = data,
    dependent = dependent,
    running = running
  )

  ## Apply end date to dependent variable and make it binary ----
  data <- data |>
    dplyr::mutate(
      dependent_binary = !is.na(.data[[dependent]]) &
        .data[[dependent]] <= pmin(pat_end_date, end)
    )

  ## Perform mse-optimal bw selection ----
  rd_mse <- rdbwselect(
    y = data[[dependent]],
    x = data[[running]],
    bwselect = "mserd"
  )

  ## Record mse-optimal bw selection ----
  msebw <- data.frame(
    h_left = rd_mse$bws[1],
    h_right = rd_mse$bws[2],
    b_left = rd_mse$bws[3],
    b_right = rd_mse$bws[4]
  )

  ## Return mse-optimal bw selection ----
  return(msebw)
}
