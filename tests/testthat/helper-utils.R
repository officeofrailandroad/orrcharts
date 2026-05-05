

dummy_chart_name <- function(name = "chart") {
  rand_id_length <- 6
  rand_id <- stringr::str_flatten(sample(LETTERS, rand_id_length, replace = TRUE))

  glue::glue("{rand_id}_{name}.png")
}
