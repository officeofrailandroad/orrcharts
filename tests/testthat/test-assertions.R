test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

# assert_chart_params
test_that(
  "assert_chart_params errors on bad inputs", {
    # data
    expect_error(assert_chart_params(data = "dataframe"))
    expect_error(assert_chart_params(data = 1:10))
    expect_error(assert_chart_params(data = data.frame(col = 1:10)))
    expect_error(assert_chart_params(data = tibble(col1 = character(0), col2 = numeric(0))))
    # filename
    expect_error(assert_chart_params(filename = 1))
    expect_error(assert_chart_params(filename = c("file1", "file2")))
    expect_error(assert_chart_params(filename = character()))
    # path
    expect_error(assert_chart_params(path = 2))
    expect_error(assert_chart_params(path = c(".", "path")))
    expect_error(assert_chart_params(path = character()))
    # chart height and width
    expect_error(assert_chart_params(chart_height = "a"))
    expect_error(assert_chart_params(chart_width = "a"))
    expect_error(assert_chart_params(chart_height = 1:5))
    expect_error(assert_chart_params(chart_width = 1:5))
    expect_error(assert_chart_params(chart_height = NULL))
    expect_error(assert_chart_params(chart_width = NULL))
    # show_legend
    expect_error(assert_chart_params(show_legend = 2))
    expect_error(assert_chart_params(show_legend = NULL))
    expect_error(assert_chart_params(show_legend = c(TRUE, TRUE)))
    # labellers
    expect_error(assert_chart_params(data_labeller = NULL))
    expect_error(assert_chart_params(data_labeller = "a"))
    expect_error(assert_chart_params(data_labeller = base::c))
    expect_error(assert_chart_params(y_axis_labeller = NULL))
    expect_error(assert_chart_params(y_axis_labeller = "a"))
    expect_error(assert_chart_params(y_axis_labeller = base::c))
    # y_axis_breaks
    expect_error(assert_chart_params(y_axis_breaks = NULL))
    expect_error(assert_chart_params(y_axis_breaks = c("a","b","c")))
  }
)

test_that(
  "assert_chart_params does not error when it shouldn't", {
    expect_true(assert_chart_params(filename = "chart.png"))
    expect_true(assert_chart_params(path = NULL))
    expect_true(assert_chart_params(path = "."))
    expect_true(assert_chart_params(chart_width = 6.7))
    expect_true(assert_chart_params(chart_height = 3.567))
    expect_true(assert_chart_params(show_legend = TRUE))
    expect_true(assert_chart_params(show_legend = FALSE))
    expect_true(assert_chart_params(data_labeller = label_orr_comma()))
    expect_true(assert_chart_params(y_axis_labeller = scales::label_percent()))
    expect_true(assert_chart_params(y_axis_breaks = seq(from = 0, to = 100, by = 20)))
  }
)
