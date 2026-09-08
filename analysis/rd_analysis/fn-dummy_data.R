library(data.table)

dummy_data <- function(
  seed = 1,
  threshold,
  n,
  p_elig,
  p_vax,
  p_out_vax,
  p_out_unvax
) {
  # Set seed ----
  set.seed(seed)

  # Create patient level data ----
  data <- data.table(
    patid = paste0("P", sprintf("%06d", 1:n))
  )

  # Some patients are eligible for vaccination ----
  data[, elig_run := runif(n, -5 * 12, 5 * 12)]
  data[, elig := (elig_run >= 0 & elig_run <= 12)]

  # Some eligible participants receive vaccination ----
  data[,
    vax := fifelse(
      elig == TRUE,
      rbinom(n, size = 1, prob = p_vax),
      0
    )
  ]
  data[,
    vax_date := fifelse(
      vax == 1,
      threshold + sample(1:365, .N, replace = TRUE),
      as.Date(NA)
    )
  ]

  # Outcome occurs in some percentage of all patients ----
  data[,
    out := fifelse(
      vax == 1,
      rbinom(n, 1, p_out_vax),
      rbinom(n, 1, p_out_unvax)
    )
  ]
  data[,
    out_date := fifelse(
      out == 1,
      threshold + (6 * 30) + sample(1:(12 * 365), .N, replace = TRUE),
      as.Date(NA)
    )
  ]

  # Return dummy data ----
  return(data[])
}
