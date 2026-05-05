



test_that(
  "dummy data creates quarterly_bar with default arguments", {
    bar_test_data <- dplyr::tibble(
      fq = c(20211,20212,20213,20214,20221,20222,20223,20224,20231,20232,20233,20234,20241,20242,20243,20244,20251,20252,20253,20254),
      value = rnorm(20, 20, 5)
    )

    chart_filename <- dummy_chart_name()

    try({
      quarterly_bar(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)
