

test_that(
  "dummy data creates side_by_side_bar with default arguments", {
    test_data <- dplyr::tibble(
      year = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      value = rnorm(10, 20, 5),
      change = rnorm(10, 0, 0.1)
    )

    chart_filename <- dummy_chart_name()

    try({
      side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)
