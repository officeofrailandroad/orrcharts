

dummy_chart_name <- function(name = "chart") {
  rand_id_length <- 6
  rand_id <- stringr::str_flatten(sample(LETTERS, rand_id_length, replace = TRUE))

  glue::glue("{rand_id}_{name}.png")
}


#' Expect chart created
#'
#' Check that a png files is created by given the function and params.
#' `chart_params` is a named list where the names match the parameters of the function.
expect_chart_created <- function(
  chart_function,
  chart_params = list(),
  temp_chart_dir = tempdir()
  ) {

  chart_filename <- dummy_chart_name()
  chart_params$filename <- chart_filename
  chart_params$path <- temp_chart_dir

  try({
    do.call(
      chart_function,
      args = chart_params
    )
  })

  testthat::expect_true(
    file.exists(file.path(temp_chart_dir, chart_filename))
  )
}
