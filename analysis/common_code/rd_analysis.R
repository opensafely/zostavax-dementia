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

  # Make results data frame ----
  result <- data.frame(
    rd = character(),
    bw_type = character(),
    kernel = character(),
    vce_method = character(),
    n_obs_left = numeric(),
    n_obs_right = numeric(),
    order_est_left = numeric(),
    order_est_right = numeric(),
    order_bias_left = numeric(),
    order_bias_right = numeric(),
    bwest_h_left = numeric(),
    bwest_h_right = numeric(),
    bwest_b_left = numeric(),
    bwest_b_right = numeric(),
    uniq_obs_left = numeric(),
    uniq_obs_right = numeric(),
    conv_coef = numeric(),
    conv_se = numeric(),
    conv_z = numeric(),
    conv_p = numeric(),
    conv_lci = numeric(),
    conv_uci = numeric(),
    bc_coef = numeric(),
    bc_se = numeric(),
    bc_z = numeric(),
    bc_p = numeric(),
    bc_lci = numeric(),
    bc_uci = numeric(),
    robust_coef = numeric(),
    robust_se = numeric(),
    robust_z = numeric(),
    robust_p = numeric(),
    robust_lci = numeric(),
    robust_uci = numeric(),
    stage1_conv_coef = numeric(),
    stage1_conv_se = numeric(),
    stage1_conv_z = numeric(),
    stage1_conv_p = numeric(),
    stage1_conv_lci = numeric(),
    stage1_conv_uci = numeric(),
    stage1_bc_coef = numeric(),
    stage1_bc_se = numeric(),
    stage1_bc_z = numeric(),
    stage1_bc_p = numeric(),
    stage1_bc_lci = numeric(),
    stage1_bc_uci = numeric(),
    stage1_robust_coef = numeric(),
    stage1_robust_se = numeric(),
    stage1_robust_z = numeric(),
    stage1_robust_p = numeric(),
    stage1_robust_lci = numeric(),
    stage1_robust_uci = numeric()
  )

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

    result[nrow(result) + 1, ] <- c(
      rd = "sharp",
      bw_type = rd_sharp$bwselect,
      kernel = rd_sharp$kernel,
      vce_method = rd_sharp$vce,
      n_obs_left = rd_sharp$N[1],
      n_obs_right = rd_sharp$N[2],
      order_est_left = rd_sharp$p,
      order_est_right = rd_sharp$p,
      order_bias_left = rd_sharp$q,
      order_bias_right = rd_sharp$q,
      bwest_h_left = rd_sharp$bws[1],
      bwest_b_left = rd_sharp$bws[2],
      bwest_h_right = rd_sharp$bws[3],
      bwest_b_right = rd_sharp$bws[4],
      uniq_obs_left = rd_sharp$M[1],
      uniq_obs_right = rd_sharp$M[2],
      conv_coef = rd_sharp$coef[1],
      conv_se = rd_sharp$se[1],
      conv_z = rd_sharp$z[1],
      conv_p = rd_sharp$pv[1],
      conv_lci = rd_sharp$ci[1],
      conv_uci = rd_sharp$ci[4],
      bc_coef = rd_sharp$coef[2],
      bc_se = rd_sharp$se[2],
      bc_z = rd_sharp$z[1],
      bc_p = rd_sharp$pv[2],
      bc_lci = rd_sharp$ci[2],
      bc_uci = rd_sharp$ci[5],
      robust_coef = rd_sharp$coef[3],
      robust_se = rd_sharp$se[3],
      robust_z = rd_sharp$z[1],
      robust_p = rd_sharp$pv[3],
      robust_lci = rd_sharp$ci[3],
      robust_uci = rd_sharp$ci[6],
      stage1_conv_coef = NA,
      stage1_conv_se = NA,
      stage1_conv_z = NA,
      stage1_conv_p = NA,
      stage1_conv_lci = NA,
      stage1_conv_uci = NA,
      stage1_bc_coef = NA,
      stage1_bc_se = NA,
      stage1_bc_z = NA,
      stage1_bc_p = NA,
      stage1_bc_lci = NA,
      stage1_bc_uci = NA,
      stage1_robust_coef = NA,
      stage1_robust_se = NA,
      stage1_robust_z = NA,
      stage1_robust_p = NA,
      stage1_robust_lci = NA,
      stage1_robust_uci = NA
    )
  }

  ## Run fuzzy RD ----
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

    result[nrow(result) + 1, ] <- c(
      rd = "fuzzy",
      bw_type = rd_fuzzy$bwselect,
      kernel = rd_fuzzy$kernel,
      vce_method = rd_fuzzy$vce,
      n_obs_left = rd_fuzzy$N[1],
      n_obs_right = rd_fuzzy$N[2],
      order_est_left = rd_fuzzy$p,
      order_est_right = rd_fuzzy$p,
      order_bias_left = rd_fuzzy$q,
      order_bias_right = rd_fuzzy$q,
      bwest_h_left = rd_fuzzy$bws[1],
      bwest_b_left = rd_fuzzy$bws[2],
      bwest_h_right = rd_fuzzy$bws[3],
      bwest_b_right = rd_fuzzy$bws[4],
      uniq_obs_left = rd_fuzzy$M[1],
      uniq_obs_right = rd_fuzzy$M[2],
      conv_coef = rd_fuzzy$coef[1],
      conv_se = rd_fuzzy$se[1],
      conv_z = rd_fuzzy$z[1],
      conv_p = rd_fuzzy$pv[1],
      conv_lci = rd_fuzzy$ci[1],
      conv_uci = rd_fuzzy$ci[4],
      bc_coef = rd_fuzzy$coef[2],
      bc_se = rd_fuzzy$se[2],
      bc_z = rd_fuzzy$z[1],
      bc_p = rd_fuzzy$pv[2],
      bc_lci = rd_fuzzy$ci[2],
      bc_uci = rd_fuzzy$ci[5],
      robust_coef = rd_fuzzy$coef[3],
      robust_se = rd_fuzzy$se[3],
      robust_z = rd_fuzzy$z[1],
      robust_p = rd_fuzzy$pv[3],
      robust_lci = rd_fuzzy$ci[3],
      robust_uci = rd_fuzzy$ci[6],
      stage1_conv_coef = rd_fuzzy$tau_T[1],
      stage1_conv_se = rd_fuzzy$se_T[1],
      stage1_conv_z = rd_fuzzy$z_T[1],
      stage1_conv_p = rd_fuzzy$pv_T,
      stage1_conv_lci = rd_fuzzy$ci_T[1],
      stage1_conv_uci = rd_fuzzy$ci_T[4],
      stage1_bc_coef = rd_fuzzy$tau_T[2],
      stage1_bc_se = rd_fuzzy$se_T[2],
      stage1_bc_z = rd_fuzzy$z_T[2],
      stage1_bc_p = rd_fuzzy$pv_T[1],
      stage1_bc_lci = rd_fuzzy$ci_T[2],
      stage1_bc_uci = rd_fuzzy$ci_T[5],
      stage1_robust_coef = rd_fuzzy$tau_T[3],
      stage1_robust_se = rd_fuzzy$se_T[3],
      stage1_robust_z = rd_fuzzy$z_T[3],
      stage1_robust_p = rd_fuzzy$pv_T[2],
      stage1_robust_lci = rd_fuzzy$ci_T[3],
      stage1_robust_uci = rd_fuzzy$ci_T[6]
    )
  }

  ## Return RD results ----

  return(result)
}
