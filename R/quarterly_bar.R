
#' Quarterly Bar Chart
#'
#' A bar chart time series with the label above the last data point.
#' @inheritParams ggplot2::ggsave
#' @param data A data frame with 2 columns. The first column should be the
#'   financial_quarter_key and the second column the value to be plotted. The
#'   data should start at Q1 of a year.
#' @param bar_colour A hex code or colour name for the fill colour of the bars.
#'   Defaults to ORR dark blue.
#' @param y_axis_percent If `TRUE` a '%' symbol is added to the y-axis labels.
#' @param v_nudge_data_label Shift the last bar data label up or down to dodge
#'   the neighbouring bars. Negative values move upwards, positive values move
#'   downwards.
#' @param h_nudge_x_axis_labels Shift the x-axis labels right or left to be in
#'   the centre of the years. Typically between -1 and 1. Negative value shifts
#'   right, positive shifts left.
#' @return A ggplot2 bar chart for ORR quarterly bar time series.
quarterly_bar <- function(
    data,
    filename,
    path = NULL,
    bar_colour = "#253268",
    y_axis_percent = TRUE,
    v_nudge_data_label = 0,
    h_nudge_x_axis_labels = 0
) {
  # Argument assertions
  assertthat::assert_that(
    assertthat::is.flag(y_axis_percent),
    msg = "Argument y_axis_percent expects TRUE or FALSE"
  )
  assertthat::assert_that(
    assertthat::is.number(v_nudge_data_label),
    assertthat::is.number(h_nudge_x_axis_labels),
    msg = "Arguments v_nudge_data_label and h_nudge_x_axis_labels need to be single numbers"
  )
  assertthat::assert_that(
    assertthat::is.string(bar_colour),
    msg = "Argument bar_colour needs to be a colour string"
  )
  assertthat::assert_that(
    base::inherits(data, "data.frame"),
    base::ncol(data) >= 2,
    msg = "Argument data needs to be a data frame with at least 2 columns"
  )

  # set data column names
  data <- data[,1:2]
  colnames(data) <- c("financial_quarter_key", "value")

  plot_data <- data %>%
    dplyr::arrange(.data$financial_quarter_key) %>%
    dplyr::mutate(
      bar_number = dplyr::row_number(), # order to plot bars
      year_label = year_label_from_quarter_key(.data$financial_quarter_key)
    )

  # Find the key for the last quarter in data set
  last_quarter = max(data$financial_quarter_key, na.rm = TRUE)

  # Set suffix to value labels
  value_label_suffix <- base::ifelse(y_axis_percent, "%", "")

  # data for the data label above the last bar
  label_data <- plot_data %>%
    dplyr::filter(.data$financial_quarter_key == last_quarter) %>%
    dplyr::mutate(
      label = base::paste(
        base::format(base::round(.data$value, digits = 1), nsmall = 1),
        value_label_suffix,
        sep = ""
      )
    )

  # y axis labels
  ## At some point better to pass the labeller as a function argument
  y_labeller <- base::ifelse(
    y_axis_percent,
    scales::label_percent(scale = 1), # add % to value
    scales::label_comma() # comma seperated
  )

  # x axis plot labels
  plot_x_labels <- plot_data %>%
    dplyr::pull(.data$year_label) %>%
    base::unique() %>%
    base::c("")

  # calculate the max number of quarters
  plot_years <- base::length(plot_x_labels) - 1
  max_q <- plot_years * 4

  # Set font family and size
  font_fam <- "Arial"
  font_size <- 37

  showtext::showtext_auto()

  qbar_plot <- plot_data %>%
    ggplot2::ggplot() +
    ggplot2::coord_cartesian(clip = "off") +
    # bars
    ggplot2::geom_bar(ggplot2::aes(.data$bar_number, .data$value), stat = "identity", fill = bar_colour) +
    # data label
    ggplot2::geom_text(
      data = label_data,
      ggplot2::aes(.data$bar_number, .data$value, label = .data$label),
      hjust = "center",
      vjust = -1 + v_nudge_data_label, # hover above bar
      family = font_fam,
      fontface = "bold",
      size = font_size,
      size.unit = "pt"
    ) +
    ggplot2::theme_classic(base_family = font_fam) +
    ggplot2::scale_y_continuous(
      breaks = base::seq(from = 0, to = 100, by = 20), # set y-axis breaks to every 20%
      labels = y_labeller, # show axis labels with % symbol if needed
      expand = ggplot2::expansion(mult = c(0,0.05)), # add 5% to top of y axis to make sure 100% shows
      oob = scales::squish,
      name = NULL # remove axis title
    ) +
    ggplot2::scale_x_continuous(
      limits = c(0.5, max_q + 0.5), # show 24 bars
      breaks = base::seq(from = 0.5, to = max_q + 0.5, by = 4), # set axis ticks to every 4 bars and show between bars
      expand = ggplot2::expansion(mult = c(0,0.001)), # extend end of xaxis slightly so last axis tick shows
      labels = plot_x_labels, # use year labels
      name = NULL # remove axis title
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = font_size),
      axis.ticks.length.x = grid::unit(.5, "cm"), # set length of x axis ticks
      title = ggplot2::element_blank(), # remove whitespace at top of plot for title
      plot.margin = ggplot2::margin(t = 5, r = 10, b = 0, l = 0.5), # set margins around plot
      panel.grid.major.y = ggplot2::element_line(color = "grey90"), # set y axis lines to light grey
      # set styles for x axis labels
      axis.text.x  =  ggplot2::element_text(
        colour = "black",
        hjust = 0.2 * plot_years - 1.8 + h_nudge_x_axis_labels, # left right adjustment - calculation tries to adjust for different number of years
        vjust = 6 # up down adjustment
      ),
      # set styles for y axis labels
      axis.text.y  =  ggplot2::element_text(colour = "black")
    )

  # Save plot in standard size
  ggplot2::ggsave(
    filename = filename,
    path = path,
    plot = qbar_plot,
    width = 6.692913,
    height = 3.566929,
    units = "in",
    device = "png",
    dpi = 300
  )
}
