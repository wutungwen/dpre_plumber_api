library(plumber)

cos_sim_dpre_df <- readRDS("parameters/cos_sim_dpre_df.obj")

eta_dpre_df <- readRDS("parameters/eta_dpre_df.obj")

pr("plumber/plumber.R") |>
  pr_run()