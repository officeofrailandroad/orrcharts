

test_that(
  "dummy data creates horizontal_bar with default arguments", {
    bar_test_data <- dplyr::tibble(
      foc = c("Freightliner Group","GB Railfreight","DB Cargo UK","Direct Rail Services","Colas Freight","Devon and Cornwall Railways"),
      val = c(39.2, 32.3, 20.3, 4.7, 2.5, 0.9)
    )

    chart_filename <- dummy_chart_name()

    try({
      horizontal_bar(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)

test_that(
  "horizontal_bar chart with default args has not changed",
  {
    bar_test_data <- dplyr::tibble(
      foc = c("Freightliner Group","GB Railfreight","DB Cargo UK","Direct Rail Services","Colas Freight","Devon and Cornwall Railways"),
      val = c(39.2, 32.3, 20.3, 4.7, 2.5, 0.9)
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      horizontal_bar(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "horizontal_bar_default.png"
    )
  }
)

test_that(
  "horizontal_bar chart with custom args has not changed",
  {
    bar_test_data <- dplyr::tibble(
      foc = c("Freightliner Group","GB Railfreight","DB Cargo UK","Direct Rail Services","Colas Freight","Devon and Cornwall Railways"),
      val = c(39.2, 32.3, 20.3, 4.7, 2.5, 0.9)
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      horizontal_bar(
        data = bar_test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        bar_colours = "#8787C9",
        y_axis_label_wrap_nchar = 100,
        show_legend = TRUE,
        data_labeller = scales::label_number(suffix = "m")
      ),
      "horizontal_bar_custom.png"
    )
  }
)


