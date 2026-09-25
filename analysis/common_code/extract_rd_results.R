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
    bc_z = rd$z[2],
    bc_p = rd$pv[2],
    bc_lci = rd$ci[2],
    bc_uci = rd$ci[5],
    robust_coef = rd$coef[3],
    robust_se = rd$se[3],
    robust_z = rd$z[3],
    robust_p = rd$pv[3],
    robust_lci = rd$ci[3],
    robust_uci = rd$ci[6]
  )

  ## Add Stage 1 results ----
  stage1_methods <- c("conv", "bc", "robust")
  if (design == "fuzzy") {
    for (i in seq_along(stage1_methods)) {
      method <- stage1_methods[i]
      result[[paste0("stage1_", method, "_coef")]] <- rd$tau_T[i]
      result[[paste0("stage1_", method, "_se")]] <- rd$se_T[i]
      result[[paste0("stage1_", method, "_z")]] <- rd$z_T[i]
      result[[paste0("stage1_", method, "_p")]] <- rd$pv_T[i]
      result[[paste0("stage1_", method, "_lci")]] <- rd$ci_T[i]
      result[[paste0("stage1_", method, "_uci")]] <- rd$ci_T[i + 3] # CI indices are arranged as: # conv: 1, 4 # bc: 2, 5 # robust: 3, 6
    }
  } else {
    for (method in stage1_methods) {
      result[[paste0("stage1_", method, "_coef")]] <- NA_real_
      result[[paste0("stage1_", method, "_se")]] <- NA_real_
      result[[paste0("stage1_", method, "_z")]] <- NA_real_
      result[[paste0("stage1_", method, "_p")]] <- NA_real_
      result[[paste0("stage1_", method, "_lci")]] <- NA_real_
      result[[paste0("stage1_", method, "_uci")]] <- NA_real_
    }
  }

  ## Return extracted result ----
  return(result)
}
