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
  # Validate function inputs ----
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

  # Make fuzzy ----

  data[,
    fuzzy := fifelse(between(get(fuzzy_date), threshold, end), 1, 0, na = 0)
  ]

  # Apply end date to dependent variable ----

  data[,
    dependent_ended := fifelse(
      get(dependent) <= end,
      get(dependent),
      as.Date(NA)
    )
  ]

  # Make results list ----

  result <- list()

  # Run sharp RD ----
  if ("sharp" %in% design) {
    rd_sharp <- rdrobust(
      data = data,
      y = dependent_ended,
      x = data[[running]],
      p = polynomial,
      kernel = kernel,
      vce = "hc0",
      c = 0,
      h = bandwidth
    )
    result$sharp <- rd_sharp
  }

  # Run fuzzy RD ----
  if ("fuzzy" %in% design) {
    rd_fuzzy <- rdrobust(
      data = data,
      y = dependent_ended,
      x = data[[running]],
      fuzzy = fuzzy,
      p = polynomial,
      kernel = kernel,
      vce = "hc0",
      c = 0,
      h = bandwidth
    )
    result$fuzzy <- rd_fuzzy
  }

  # Return ----

  return(result)
}
