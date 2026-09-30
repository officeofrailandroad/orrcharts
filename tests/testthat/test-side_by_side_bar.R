

test_that(
  "dummy data creates side_by_side_bar with default arguments", {
    test_data <- dplyr::tibble(
      year = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      value = rnorm(10, 20, 5),
      change = rnorm(10, 0, 0.1)
    )

    expect_chart_created(
      chart_function = side_by_side_bar,
      chart_params = list(
        data = test_data
      )
    )
  }
)

test_that(
  "dummy data creates side_by_side_bar with non-default args arguments", {
    test_data <- dplyr::tibble(
      year = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      value = rnorm(10, 20, 5),
      change = rnorm(10, 0, 0.1)
    )

    expect_chart_created(
      chart_function = side_by_side_bar,
      chart_params = list(
        data = test_data,
        order_by_bar = "right"
      )
    )

    expect_chart_created(
      chart_function = side_by_side_bar,
      chart_params = list(
        data = test_data,
        order_descending = FALSE
      )
    )

    expect_chart_created(
      chart_function = side_by_side_bar,
      chart_params = list(
        data = test_data,
        left_bar_labeller = scales::label_comma()
      )
    )

    expect_chart_created(
      chart_function = side_by_side_bar,
      chart_params = list(
        data = test_data,
        right_bar_labeller = scales::label_percent()
      )
    )

  }
)

test_that(
  "side_by_side_bar chart with default args has not changed",
  {
    test_data <- dplyr::tibble(
      year = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      value = 25:16,
      change = -4:5
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "side_by_side_bar_default.png"
    )
  }
)

test_that(
  "side_by_side_bar chart with custom args has not changed",
  {
    test_data <- dplyr::tibble(
      year = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      value = 25:16,
      change = -4:5
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        left_bar_title = "Value metric\nfrom A to B",
        right_bar_title = "Change metric\nfrom A to B",
        order_by_bar = "left",
        left_bar_colour = "#026060",
        right_bar_colour_positive = "#D8730F",
        right_bar_colour_negative = "#D8730F",
        left_bar_labeller = scales::label_number(),
        right_bar_labeller = scales::label_percent(scale = 1),
        panel_proportional_widths = c(0.3, 0.3)
      ),
      "side_by_side_bar_custom.png"
    )
  }
)

