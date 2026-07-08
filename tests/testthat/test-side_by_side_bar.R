

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
