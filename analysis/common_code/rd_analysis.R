# Load libraries ----
library(rdrobust)

# Define function ----
rd_analysis <- function(
  data,
  design = c("sharp", "fuzzy"),
  bandwidth,
  threshold,
  kernel = "triangular",
  polynomial = 1,
  dependent,
  running,
  fuzzy_date = NULL,
  end
) {
  ## Validate function inputs ----
  rd_input_checks(
    fn = "rd_analysis",
    data = data,
    design = design,
    bandwidth = bandwidth,
    threshold = threshold,
    kernel = kernel,
    polynomial = polynomial,
    dependent = dependent,
    running = running,
    fuzzy_date = fuzzy_date,
    end = end
  )

  ## Make fuzzy ----

  data$fuzzy <- dplyr::if_else(
    dplyr::between(data[[fuzzy_date]], threshold, end),
    1L,
    0L,
    missing = 0L
  )

  ## Apply end date to dependent variable ----

  data <- data |>
    dplyr::mutate(
      dependent_ended = dplyr::if_else(
        .data[[dependent]] <= end,
        .data[[dependent]],
        as.Date(NA)
      )
    )

  # Make results dataframe ----
  results <- list()

  # Run sharp RD ----
  if ("sharp" %in% design) {
    rd_sharp <- rdrobust(
      y = data[["dependent_ended"]],
      x = data[[running]],
      p = polynomial,
      kernel = kernel,
      vce = "hc0",
      c = 0,
      h = bandwidth
    )
    results[["sharp"]] <- extract_rd_results(rd_sharp, "sharp")
  }

  ## Run fuzzy RD ----
  if ("fuzzy" %in% design) {
    rd_fuzzy <- rdrobust(
      y = data[["dependent_ended"]],
      x = data[[running]],
      fuzzy = data[["fuzzy"]],
      p = polynomial,
      kernel = kernel,
      vce = "hc0",
      c = 0,
      h = bandwidth
    )
    results[["fuzzy"]] <- extract_rd_results(rd_fuzzy, "fuzzy")
  }

  ## Return RD results ----

  rd_analysis_results <- bind_rows(results)
  return(rd_analysis_results)
}
