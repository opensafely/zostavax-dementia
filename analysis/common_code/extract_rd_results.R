# Load libraries ----
library(checkmate)

# Define function ----
extract_rd_results <- function(rd, design) {
  ## Check inputs ----
  assert_choice(design, c("sharp", "fuzzy"))
  assert_list(rd, min.len = 1)
  ## Extract result ----
  result <- data.frame(
    rd = design,
    bw_type = rd$bwselect,
    rd_kernel = rd$kernel,
    vce_method = rd$vce,
    n_obs_left = rd$N[1],
    n_obs_right = rd$N[2],
    order_est_left = rd$p,
    order_est_right = rd$p,
    order_bias_left = rd$q,
    order_bias_right = rd$q,
    bwest_h_left = rd$bws[1],
    bwest_b_left = rd$bws[2],
    bwest_h_right = rd$bws[3],
    bwest_b_right = rd$bws[4],
    uniq_obs_left = rd$M[1],
    uniq_obs_right = rd$M[2],
    conv_coef = rd$coef[1],
    conv_se = rd$se[1],
    conv_z = rd$z[1],
    conv_p = rd$pv[1],
    conv_lci = rd$ci[1],
    conv_uci = rd$ci[4],
    bc_coef = rd$coef[2],
    bc_se = rd$se[2],
    bc_z = rd$z[1],
    bc_p = rd$pv[2],
    bc_lci = rd$ci[2],
    bc_uci = rd$ci[5],
    robust_coef = rd$coef[3],
    robust_se = rd$se[3],
    robust_z = rd$z[1],
    robust_p = rd$pv[3],
    robust_lci = rd$ci[3],
    robust_uci = rd$ci[6],
    stage1_conv_coef = if (design == "fuzzy") rd$tau_T[1] else NA_real_,
    stage1_conv_se = if (design == "fuzzy") rd$se_T[1] else NA_real_,
    stage1_conv_z = if (design == "fuzzy") rd$z_T[1] else NA_real_,
    stage1_conv_p = if (design == "fuzzy") rd$pv_T[1] else NA_real_,
    stage1_conv_lci = if (design == "fuzzy") rd$ci_T[1] else NA_real_,
    stage1_conv_uci = if (design == "fuzzy") rd$ci_T[4] else NA_real_,
    stage1_bc_coef = if (design == "fuzzy") rd$tau_T[2] else NA_real_,
    stage1_bc_se = if (design == "fuzzy") rd$se_T[2] else NA_real_,
    stage1_bc_z = if (design == "fuzzy") rd$z_T[2] else NA_real_,
    stage1_bc_p = if (design == "fuzzy") rd$pv_T[2] else NA_real_,
    stage1_bc_lci = if (design == "fuzzy") rd$ci_T[2] else NA_real_,
    stage1_bc_uci = if (design == "fuzzy") rd$ci_T[5] else NA_real_,
    stage1_robust_coef = if (design == "fuzzy") rd$tau_T[3] else NA_real_,
    stage1_robust_se = if (design == "fuzzy") rd$se_T[3] else NA_real_,
    stage1_robust_z = if (design == "fuzzy") rd$z_T[3] else NA_real_,
    stage1_robust_p = if (design == "fuzzy") rd$pv_T[3] else NA_real_,
    stage1_robust_lci = if (design == "fuzzy") rd$ci_T[3] else NA_real_,
    stage1_robust_uci = if (design == "fuzzy") rd$ci_T[6] else NA_real_
  )

  return(result)
}
