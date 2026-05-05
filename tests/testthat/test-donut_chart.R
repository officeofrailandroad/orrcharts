

test_that(
  "dummy data creates donut_chart with default arguments", {
    test_donut_data <- dplyr::tibble(
      category = c("External to the lift system", "Misuse and vandalism", "Wear and tear"),
      value = c(13, 27, 60)
    )

    chart_filename <- dummy_chart_name()

    try({
      bar_chart(
        data = test_donut_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)
