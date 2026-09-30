


test_that(
  "dummy data creates grouped_side_by_side_bar with default arguments", {
    test_data <- dplyr::tibble(
      cat = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      grp = c("A","A","A","A","B","B","B","B","C","C"),
      value = rnorm(10, 20, 5),
      change = rnorm(10, 0, 0.1)
    )

    chart_filename <- dummy_chart_name()

    try({
      grouped_side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      )
    })

    expect_true(file.exists(file.path(TEMP_CHART_DIR, chart_filename)))
  }
)


test_that(
  "grouped_side_by_side_bar chart with default args has not changed",
  {
    test_data <- dplyr::tibble(
      cat = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      grp = c("A","A","A","A","B","B","B","B","C","C"),
      value = 1:10,
      change = 5:-4
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      grouped_side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR
      ),
      "grouped_bar_default.png"
    )
  }
)


test_that(
  "grouped_side_by_side_bar chart with custom args has not changed",
  {
    test_data <- dplyr::tibble(
      cat = c("Avanti","GWR","Lumo","ScotRail","Southeastern","Grand Central","Greater Anglia","GTR","Northern","c2c"),
      grp = c("A","A","A","A","B","B","B","B","C","C"),
      value = 1:10,
      change = 5:-4
    )

    chart_filename <- dummy_chart_name()

    expect_snapshot_file(
      grouped_side_by_side_bar(
        data = test_data,
        filename = chart_filename,
        path = TEMP_CHART_DIR,
        left_bar_labeller = scales::label_number(accuracy = 1),
        left_bar_colour = "#026060",
        left_bar_title = "Custom title\n over two lines",
        right_bar_colour = "#EB668F",
        right_bar_title = "Custom change\n over two lines"
      ),
      "grouped_bar_custom.png"
    )
  }
)

