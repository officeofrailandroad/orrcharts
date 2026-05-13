

#' Line Chart
#'
#' A simple line chart to plot time series data.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param data A data frame with at least 2 columns. The first column sets the
#'   x-axis, typically a date or time period, which need to be ordered oldest to
#'   newest. Each column after that contains the values to be plotted in each
#'   line. The column names will form the series labels shown on the chart.
#' @param x_axis_labels An array of labels to display on the x-axis. These need
#'   to match the data supplied in the first column. X-axis ticks will show for
#'   all points but this parameter control which labels are displayed. By
#'   default all labels are shown.
#' @param series_colours Colours for the lines. Defaults to ORR colours.
#' @param point_shapes The shapes to show on data point per series. See
#'   [ggplot2::scale_shape()].
#' @param show_series_labels If `TRUE` chart will display the series labels,
#'   taken from column names, on the chart. Label colours will match series
#'   colours.
#' @param data_labeller Function which controls how the data labels are
#'   displayed.
#' @param series_label_coordinates Set series label location manually. Expects a
#'   data frame with 3 columns containing the series name (from the data column
#'   headers), x and y coordinates in the format supplied in data. If NULL, the
#'   default, locations are guessed using ggrepel.
#' @param chart_seed Set random seed for [ggrepel::geom_text_repel()]. `ggrepel`
#'   uses random numbers in the positioning algorithm. By setting a seed for the
#'   random number generator the labels will be in the same place each time the
#'   plot is created with the same data.
#' @returns A PNG file of a simple ORR style line chart. This function uses an
#'   algorithm from `ggrepel` to place the data and series labels. The algorithm
#'   moves the labels in small increments at random trying to minimise the
#'   amount of overlaps. If you are not happy with the final positions, change
#'   the `chart_seed` argument - this might not always return different results.
#' @export
line_chart <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    series_colours = orr_colours(),
    point_shapes = c(15, 16, 17, 18, 0, 1, 2, 5),
    x_axis_labels = NULL,
    y_axis_breaks = ggplot2::waiver(),
    y_axis_labeller = scales::label_comma(),
    show_series_labels = TRUE,
    series_label_coordinates = NULL,
    data_labeller = label_orr_comma(),
    chart_seed = 101
) {
  assert_chart_params(
    data, filename, path, chart_width, chart_height, series_colours, point_shapes,
    x_axis_labels, y_axis_breaks, y_axis_labeller, show_series_labels, data_labeller,
    chart_seed
  )
  # Fix first column name
  colnames(data)[1] <- "date"

  # Use a row number for continuous scale on x axis. This allows minor tick marks to be shown
  data <- data %>%
    dplyr::mutate(
      x_id = dplyr::row_number()
    )

  # Setup labels for the x axis - only show labels specified
  x_dates <- data$date
  n_dates <- length(x_dates)
  x_breaks <- 1:n_dates
  x_labels <- x_dates
  # If x axis labels are specified, calculate where they are
  if (length(x_axis_labels) > 0) {
    x_breaks <- which(x_dates %in% x_axis_labels) # locations of desired x axis labels
    x_labels <- x_axis_labels
  }

  # Set y-axis limits
  y_limits <- NULL
  if(! methods::is(y_axis_breaks, "waiver")) {
    # If axis breaks are specified, set the max of y axis to the largest break.
    y_limits <- c(0, max(y_axis_breaks))
  }

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  # Remove names from colours
  base::names(series_colours) <- NULL

  # Find a date in the middle of the plot. This will position the series labels
  middle_date <- x_dates[base::floor(length(x_dates) / 2)]

  # Get the last point in each series for data labels
  last_points <- data %>%
    dplyr::slice_tail(n = 1) %>%
    tidyr::pivot_longer(- dplyr::all_of(c("x_id", "date"))) %>%
    dplyr::mutate(
      # Create the data label to be displayed
      label = data_labeller(.data$value)
    )

  # Pivot data into ggplot format
  series_label_wrap_width <- 20
  plot_data <- data %>%
    tidyr::pivot_longer(- dplyr::all_of(c("x_id", "date"))) %>%
    dplyr::mutate(
      # Add series name labels at middle_date points
      series_label = base::ifelse(
        .data$date == middle_date & show_series_labels,
        stringr::str_wrap(.data$name, series_label_wrap_width),
        ""
      )
    )

  # Create series labels geom_text
  # Use ggrepel unless coordinates are specified
  # Otherwise use supplied coordinates
  # https://ggplot2-book.org/programming.html#multiple-components

  geom_repel_series_labels <- ggrepel::geom_text_repel(
    ggplot2::aes(label = .data$series_label),
    family = font_fam,
    size = font_size,
    fontface = "bold",
    # Set label anchor point to centre of label
    hjust = "center",
    vjust = "middle",
    # Space around series labels - set to dodge lines
    box.padding = 1,
    point.padding = 1,
    # Only move up and down to dodge lines
    direction = "y",
    # Don't show segment lines
    min.segment.length = 100,
    # Increased the default allowed overlaps for when lines are really close together.
    max.overlaps = 20,
    # Set line height for wrapped labels
    lineheight = 0.25,
    seed = chart_seed
  )


  # Use coords
  geom_set_series_labels <- NULL
  if(!is.null(series_label_coordinates)) {
    series_label_loc <- series_label_coordinates[, 1:3]
    names(series_label_loc) <- c("name", "date", "y")
    series_label_loc <- series_label_loc %>%
      dplyr::mutate(
        series = stringr::str_wrap(.data$name, series_label_wrap_width)
      )
    # Find x axis ID for specified coordinates
    series_label_loc$x_id <- match(series_label_loc$date, x_dates)
    geom_set_series_labels <- ggplot2::geom_text(
      data = series_label_loc,
      ggplot2::aes(
        x = .data$x_id,
        y = .data$y,
        label = .data$series,
        colour = .data$name,
        group = .data$name
      ),
      size = font_size,
      fontface = "bold",
      # Set label anchor point to centre of label
      hjust = "center",
      vjust = "middle",
      lineheight = 0.25
    )
  }

  geom_series_labels <- if(!is.null(series_label_coordinates)) {
    geom_set_series_labels
  } else {
    geom_repel_series_labels
  }

  plot <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = .data$x_id,
      y = .data$value,
      colour = .data$name,
      group = .data$name
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(ggplot2::aes(shape = .data$name)) +
    # Series labels
    # Use ggrepel to try to prevent them overlapping the lines and each other
    # ggrepel::geom_text_repel(
    #   ggplot2::aes(label = .data$series_label),
    #   family = font_fam,
    #   size = font_size,
    #   fontface = "bold",
    #   # Set label anchor point to centre of label
    #   hjust = "center",
    #   vjust = "middle",
    #   # Space around series labels - set to dodge lines
    #   box.padding = 1,
    #   point.padding = 1,
    #   # Only move up and down to dodge lines
    #   direction = "y",
    #   # Don't show segment lines
    #   min.segment.length = 100,
    #   # Increased the default allowed overlaps for when lines are really close together.
    #   max.overlaps = 20,
    #   # Set line height for wrapped labels
    #   lineheight = 0.25,
    #   seed = chart_seed
    # ) +
    geom_series_labels +
    # Last point data labels
    ggrepel::geom_text_repel(
      data = last_points,
      ggplot2::aes(label = .data$label),
      family = font_fam,
      size = font_size,
      fontface = "bold",
      hjust = -0.2, # nudge data label to the right of last point
      vjust = "middle",
      # Only move up and down to dodge lines
      direction = "y",
      # Don't show segment lines
      min.segment.length = 100,
      box.padding = 0.1,
      force_pull = 5,
      seed = chart_seed
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_x_continuous(
      name = NULL,
      breaks = x_breaks,
      labels = x_labels,
      minor_breaks = 1:n_dates,
      guide = ggplot2::guide_axis(minor.ticks = TRUE),
      expand = ggplot2::expansion(mult = c(0,0.1)) # expand end of x-axis to fit data labels.
    ) +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = y_axis_breaks,
      limits = y_limits,
      labels = y_axis_labeller, # show axis labels with % symbol if needed
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_color_manual(values = series_colours) +
    ggplot2::scale_shape_manual(values = point_shapes) +
    # Don't show legends
    ggplot2::guides(colour = "none", shape = "none") +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = (font_size * ggplot2::.pt)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
      panel.grid.major.y = ggplot2::element_line(color = "grey90"), # set y axis lines to light grey,
      # Set x axis tick lengths
      axis.ticks.length.x = grid::unit(.3, "cm"),
      axis.minor.ticks.x.bottom = ggplot2::element_line(),
      axis.minor.ticks.length = ggplot2::rel(0.5)
    )

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    path = path,
    width = chart_width,
    height = chart_height,
    units = "in",
    device = "png",
    dpi = 300
  )
}
