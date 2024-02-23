library(plumber)

#* @apiTitle DPRE API
#* @apiDescription DPREモデルの事後分布からQOIを抽出するAPI 

#* コサイン類似度を抽出する
#* @param senator 対象の上院議員
#* @param top_num 抽出データ数
#* @get /cos_sim
function(senator, top_num) {
  cos_sim_dpre_df |>
    dplyr::filter(.data$senator_1 == .env$senator) |>
    dplyr::select(senator_2, mean, q5, q95) |>
    dplyr::arrange(-mean) |>
    head(as.integer(top_num))
}

#* ETAの事後分布を抽出する
#* @param senator 対象の上院議員
#* @get /eta
function(senator) {
  eta_dpre_df |>
    dplyr::filter(.data$senator == .env$senator) |>
    dplyr::select(senator, latent_group_id, mean, q5, q95)
}
