# Load libraries ----
library(checkmate)

# Define function ----
rd_input_checks <- function(
  fn,
  data,
  design,
  bandwidth,
  threshold,
  kernel,
  polynomial,
  dependent,
  running,
  fuzzy_date,
  end
) {
  ## Check which function is being validated ----
  assert_choice(fn, c("rd_msebw", "rd_analysis"))

  ## Always checks ----
  ### Check data is a data.table ----
  assert_class(data, "data.table")

  ### Check data contains 1 or more rows ----
  assert_true(nrow(data) > 0)

  ### Check variable dependent is a date ----
  assert_multi_class(
    data[[dependent]],
    c("Date", "POSIXct", "POSIXlt")
  )

  ### Check variable running is numeric ----
  assert_multi_class(data[[running]], c("numeric"))

  ### Check variables dependent and running are supplied ----
  required_vars <- c(dependent, running)
  assert_subset(
    required_vars,
    choices = colnames(data),
    .var.name = "required_vars"
  )

  ## Checks for rd_analysis ----
  if (fn == "rd_analysis") {
    ### Check design is 'sharp' and/or 'fuzzy' ----
    assert_subset(design, c("sharp", "fuzzy"))

    ### Check bandwidth is a positive numeric value of length 1 or 2 ----
    assert_numeric(
      bandwidth,
      finite = TRUE,
      any.missing = FALSE,
      min.len = 1,
      max.len = 2
    )
    assert_true(all(bandwidth > 0))

    ### Check threshold and end are non-missing single date values ----
    for (x in c("threshold", "end")) {
      assert_date(
        get(x),
        len = 1,
        any.missing = FALSE,
        .var.name = x
      )
    }

    ### Check kernel is 'triangular', 'epanechnikov' or 'uniform' (including 'epanechnikov' for completeness) ----
    assert_choice(kernel, c("triangular", "epanechnikov", "uniform"))

    ### Check polynomial is a positive numeric value of length 1 ----
    assert_number(
      polynomial,
      finite = TRUE,
      na.ok = FALSE
    )
    assert_true(polynomial > 0)

    ## Checks for rd_analysis including fuzzy design ----
    if ("fuzzy" %in% design) {
      ### Check treatment status variable has been supplied if 'fuzzy' specified ----
      assert_string(fuzzy_date, null.ok = FALSE)

      ### Check if fuzzy_date is supplied ----
      assert_subset(
        fuzzy_date,
        choices = colnames(data),
        .var.name = "fuzzy_date"
      )

      ### Check variable fuzzy_date is a date ----
      assert_multi_class(
        data[[fuzzy_date]],
        c("Date", "POSIXct", "POSIXlt")
      )
    }
  }

  return("Inputs are valid")
}
