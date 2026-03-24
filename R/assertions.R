

# Run common argument assertions in the chart functions
assert_chart_params <- function(...) {
  # Gather arguments passed in as a list
  args <- list(...)

  # Run checks on the arguments.
  # These should be standard across many charts.
  # By placing in if statements not all args need by present

  # data
  if("data" %in% names(args)) {
    assertthat::assert_that(
      base::inherits(args$data, "data.frame"),
      base::ncol(args$data) >= 2,
      base::nrow(args$data) > 0,
      msg = "Argument data needs to be a data frame with at least 2 columns and 1 row"
    )
  }

  # filename
  if("filename" %in% names(args)) {
    assertthat::assert_that(
      assertthat::is.string(args$filename),
      msg = "filename arguement should be a string of length 1"
    )
  }

  # path
  if("path" %in% names(args)) {
    assertthat::assert_that(
      is.null(args$path) | assertthat::is.string(args$path),
      msg = "path can be a string of length 1 or NULL"
    )
  }

  # chart_width
  if("chart_width" %in% names(args)) {
    assertthat::assert_that(
      assertthat::is.number(args$chart_width),
      msg = "chart_width and chart_height need to be single numbers"
    )
  }

  # chart_height
  if("chart_height" %in% names(args)) {
    assertthat::assert_that(
      assertthat::is.number(args$chart_height),
      msg = "chart_width and chart_height need to be single numbers"
    )
  }

  # show_legend
  if("show_legend" %in% names(args)) {
    assertthat::assert_that(
      assertthat::is.flag(args$show_legend),
      msg = "show_legend argument can be TRUE or FALSE"
    )
  }

  # y axis labeller
  if("y_axis_labeller" %in% names(args)) {
    assertthat::assert_that(
      rlang::is_closure(args$y_axis_labeller),
      msg = "y_axis_labeller expects a function"
    )
  }

  # y axis breaks
  if("y_axis_breaks" %in% names(args)) {
    assertthat::assert_that(
      methods::is(args$y_axis_breaks, "waiver") |
        (inherits(args$y_axis_breaks, "numeric") & length(args$y_axis_breaks) > 1),
      msg = "y_axis_breaks expect a numeric vector of length at least 2, or a ggplot waiver"
    )
  }

  # data labeller
  if("data_labeller" %in% names(args)) {
    assertthat::assert_that(
      rlang::is_closure(args$data_labeller),
      msg = "data_labeller expects a function"
    )
  }

  invisible(TRUE)
}
