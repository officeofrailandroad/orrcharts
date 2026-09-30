
test_that(
  "dummy data creates line_chart with default arguments", {
    bar_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = rnorm(6, 40, 5),
      `Category B` = rnorm(6, 20, 5)
    )

    chart_filename <- dummy_chart_name()

    try({
      line_chart(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)

test_that(
  "line_chart with default args has not changed",
  {
    line_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = 1:6,
      `Category B` = 6:1
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      line_chart(
        data = line_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "line_default.png"
    )
  }
)

test_that(
  "line_chart with custom args has not changed",
  {
    line_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = 1:6,
      `Category B` = 6:1
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      line_chart(
        data = line_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        point_shapes = c(17,18),
        series_colours = c("#702472","#8787C9"),
        y_axis_breaks = seq(from = 0, to = 10, by = 2),
        y_axis_labeller = scales::label_number(accuracy = 1),
        data_labeller = scales::label_number(suffix = "m"),
        show_series_labels = FALSE,
        x_axis_labels = c("2020/21","2022/23","2025/26"),
        series_breaks = "2022/23"
      ),
      "line_custom.png"
    )
  }
)



