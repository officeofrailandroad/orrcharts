


test_that(
  "dummy data creates bar_chart with default arguments", {
    bar_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = rnorm(6, 20, 5),
      `Category B` = rnorm(6, 20, 5)
    )

    chart_filename <- dummy_chart_name()

    try({
      bar_chart(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)

test_that(
  "bar chart with default args has not changed",
  {
    bar_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = 1:6,
      `Category B` = 6:1
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      bar_chart(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "bar_default.png"
    )
  }
)

test_that(
  "bar chart with default args has not changed",
  {
    bar_test_data <- dplyr::tibble(
      year = c("2020/21","2021/22","2022/23","2023/24","2024/25","2025/26"),
      `Category A` = 1:6,
      `Category B` = 6:1
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      bar_chart(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        y_axis_breaks = seq(from = 0, to = 8, by = 2),
        data_labeller = scales::label_number(accuracy = 1),
        show_legend = FALSE,
        bar_colours = c("#8787C9","#253268")
      ),
      "bar_custom.png"
    )
  }
)


