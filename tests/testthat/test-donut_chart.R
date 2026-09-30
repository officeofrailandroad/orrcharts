

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


test_that(
  "donut chart with default args has not changed",
  {
    test_donut_data <- dplyr::tibble(
      category = c("External to the lift system", "Misuse and vandalism", "Wear and tear"),
      value = c(13, 27, 60)
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      bar_chart(
        data = test_donut_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "donut_default.png"
    )
  }
)


test_that(
  "donut chart with custom pie args has not changed",
  {
    test_donut_data <- dplyr::tibble(
      category = c("External to the lift system", "Misuse and vandalism", "Wear and tear"),
      value = c(13, 27, 60)
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      donut_chart(
        data = test_donut_data %>% dplyr::arrange(value),
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        data_labeller = scales::label_percent(scale = 1),
        labels_gap_size = 3,
        outer_chart_limit = 8,
        as_pie_chart = TRUE,
        centre_label = "100%"
      )
      ,
      "pie_custom.png"
    )
  }
)



