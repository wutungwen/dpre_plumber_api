# 詳細はREADMEのQiitaの記事を見てください

data("s109", package = "pscl")

rc_df <- s109$votes |> 
  as.data.frame() |>
  tibble::rownames_to_column(var = "senator") |>
  tibble::tibble() |>
  tidyr::pivot_longer(!senator, names_to = "bill", values_to = "result_cd") |>
  dplyr::filter(result_cd %in% c(1,2,3,4,5,6)) |>
  dplyr::mutate(
    result = dplyr::case_when(
      result_cd %in% c(1,2,3) ~ 1,
      TRUE ~ 0
    )
  )

senator_master <- rc_df |>
  dplyr::select(senator) |>
  dplyr::distinct() |>
  dplyr::arrange(senator) |>
  dplyr::mutate(
    senator_id = dplyr::row_number()
  ) |>
  dplyr::mutate(
    party_id = dplyr::case_when(
      stringr::str_detect(senator, "\\(R") == TRUE ~ 1,
      TRUE ~ 2
    )
  )

bill_master <- rc_df |>
  dplyr::select(bill) |>
  dplyr::distinct() |>
  dplyr::arrange(bill) |>
  dplyr::mutate(
    bill_id = dplyr::row_number()
  )

rc_df_with_id <- rc_df |>
  dplyr::left_join(senator_master, by = "senator") |>
  dplyr::left_join(bill_master, by = "bill") 

set.seed(12345)
train_id <- sample(seq_len(nrow(rc_df_with_id)), nrow(rc_df_with_id) * 0.8)

rc_df_with_id_test_all <- rc_df_with_id[-train_id,]

rc_df_with_id_test_all_zero_id <- which(rc_df_with_id_test_all$result == 0)

rc_df_with_id_test_all_one_id <- sample(
  which(rc_df_with_id_test_all$result == 1),
  length(which(rc_df_with_id_test_all$result == 0))
)

rc_df_with_id_test_balanced <- rc_df_with_id_test_all[c(rc_df_with_id_test_all_zero_id, rc_df_with_id_test_all_one_id),]


data_dpre_list <- list(
  N = nrow(rc_df_with_id[train_id,]),
  P = 5,
  senator_type = nrow(senator_master),
  bill_type = nrow(bill_master),
  K = 10,
  
  senator = rc_df_with_id$senator_id[train_id],
  bill =  rc_df_with_id$bill_id[train_id],
  result = rc_df_with_id$result[train_id],
  
  val_N = nrow(rc_df_with_id[-train_id,]),
  val_senator = rc_df_with_id$senator_id[-train_id],
  val_bill =  rc_df_with_id$bill_id[-train_id],
  val_result = rc_df_with_id$result[-train_id]
)


m_senate_dpre_init <- cmdstanr::cmdstan_model("estimation/senate_dpre.stan",
                                              cpp_options = list(
                                                stan_threads = TRUE
                                              )
)

m_senate_dpre_estimate <- m_senate_dpre_init$variational(
  seed = 12345,
  threads = 6,
  iter = 50000,
  init = 20,
  data = data_dpre_list
)

m_senate_dpre_summary <- m_senate_dpre_estimate$summary()

cos_sim_dpre_df <- m_senate_dpre_summary |>
  dplyr::filter(stringr::str_detect(variable, "cos_sim")) |>
  dplyr::mutate(
    senator_id_list = purrr::map(
      variable, \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2:3] |> 
          as.integer()
      }
    )
  ) |>
  tidyr::unnest_wider(col = senator_id_list, names_sep = "_") |>
  dplyr::left_join(senator_master, by = c("senator_id_list_1" = "senator_id")) |>
  dplyr::left_join(senator_master, by = c("senator_id_list_2" = "senator_id")) |>
  dplyr::rename(
    party_id_1 = party_id.x,
    party_id_2 = party_id.y,
    senator_1 = senator.x,
    senator_2 = senator.y
  ) |>
  dplyr::mutate(
    party_1 = dplyr::case_when(
      party_id_1 == 1 ~ "Republican",
      TRUE ~ "Democrat"
    ),
    party_2 = dplyr::case_when(
      party_id_2 == 1 ~ "Republican",
      TRUE ~ "Democrat"
    )
  )

eta_dpre_df <- m_senate_dpre_summary |>
  dplyr::filter(stringr::str_detect(variable, "eta")) |>
  dplyr::mutate(
    senator_id_list = purrr::map(
      variable, \(x){
        stringr::str_split(x, "\\[|\\]|,")[[1]][2:3] |> 
          as.integer()
      }
    )
  ) |>
  tidyr::unnest_wider(col = senator_id_list, names_sep = "_") |>
  dplyr::rename(
    senator_id = senator_id_list_1,
    latent_group_id = senator_id_list_2
  ) |>
  dplyr::left_join(senator_master, by = "senator_id")

saveRDS(cos_sim_dpre_df, "parameters/cos_sim_dpre_df.obj")

saveRDS(eta_dpre_df, "parameters/eta_dpre_df.obj")
