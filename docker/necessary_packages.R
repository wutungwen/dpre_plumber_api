purrr::map(list.files("plumber"), 
               \(x){
                 stringr::str_c("plumber/", x) |>
                   readLines()
               }
               ) |>
  stringr::str_c(collapse = " ") |>
  stringr::str_extract_all("(\\w+)::") |>
  unlist() |>
  unique() |>
  stringr::str_remove_all("::")