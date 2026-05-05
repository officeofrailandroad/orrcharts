


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
