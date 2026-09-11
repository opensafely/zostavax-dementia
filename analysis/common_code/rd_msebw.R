# Load libraries ----
library(data.table)
library(rdrobust)

# Define function ----
rd_msebw <- function(data, dependent, running) {
  ## Validate function inputs ----
  rd_input_checks(
    fn = "rd_msebw",
    data = data,
    dependent = dependent,
    running = running
  )

  ## Perform mse-optimal bw selection ----
  rd_mse <- rdbwselect(
    data = data,
    y = data[[dependent]],
    x = data[[running]],
    bwselect = "mserd"
  )

  ## Record mse-optimal bw selection ----
  msebw <- data.table(
    h_left = rd_mse$bws[1],
    h_right = rd_mse$bws[2],
    b_left = rd_mse$bws[3],
    b_right = rd_mse$bws[4]
  )

  ## Return mse-optimal bw selection ----
  return(msebw)
}
