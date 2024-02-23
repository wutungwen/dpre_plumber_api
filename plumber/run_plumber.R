library(plumber)

cos_sim_dpre_df <- readRDS("parameters/cos_sim_dpre_df.obj")

eta_dpre_df <- readRDS("parameters/eta_dpre_df.obj")

plumb("plumber/plumber.R")$
  run(host = "0.0.0.0", port = 8000)