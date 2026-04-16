

#' Bar Chart
#'
#' A simple or stacked bar chart in ORR style.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param bar_colours An array of colour hex-codes. Defaults to ORR colours.
#' @param data_labeller A function which controls how the data labels over the bars are displayed.
#' @param show_legend Should the legend be shown on the chart. Defaults to `TRUE`.
#' @export
bar_chart <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    bar_colours = orr_colours(),
    y_axis_breaks = ggplot2::waiver(),
    y_axis_labeller = scales::label_comma(),
    data_labeller = label_orr_comma(),
    show_legend = TRUE
) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height, bar_colours, y_axis_breaks,
    y_axis_labeller, data_labeller, show_legend
  )

  # Set name of first column of data
  base::colnames(data)[1] <- "category"

  # Remove names from list of colours - it interferes with ggplot
  base::names(bar_colours) <- NULL

  # Calculate if black or white text will have best contrast with bar colours
  bar_hcl <- farver::decode_colour(bar_colours, "rgb", "hcl")
  text_colours <- base::ifelse(bar_hcl[, "l"] > 50, "black", "white")

  # Set y-axis limits
  y_limits <- NULL
  if(! methods::is(y_axis_breaks, "waiver")) {
    # If axis breaks are specified, set the max of y axis to the largest break.
    y_limits <- c(0, max(y_axis_breaks))
  }

  # Should legend be shown
  fill_legend = "none"
  if(show_legend) fill_legend <- "legend"

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  plot_data <- data %>%
    tidyr::pivot_longer(- dplyr::all_of("category")) %>%
    dplyr::mutate(
      data_label = data_labeller(.data$value),
      # Wrap long category names to display better in the legend
      name = stringr::str_wrap(.data$name, width = 20)
    )

  bplt <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$category, y = .data$value, fill = .data$name)) +
    ggplot2::geom_col() +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$data_label, colour = .data$name),
      # Put labels in the middle of the bars
      position = ggplot2::position_stack(vjust = 0.5),
      family = font_fam,
      size = font_size
    ) +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = y_axis_breaks,
      limits = y_limits,
      labels = y_axis_labeller, # show axis labels with % symbol if needed
      expand = ggplot2::expansion(mult = 0)
    ) +
    ggplot2::scale_x_discrete(
      name = NULL
    ) +
    ggplot2::scale_fill_manual(values = bar_colours) +
    ggplot2::scale_colour_manual(values = text_colours) +
    ggplot2::guides(colour = "none", fill = fill_legend) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = (font_size * ggplot2::.pt)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1)),
      panel.grid.major.y = ggplot2::element_line(color = "grey90"),
      axis.ticks.x = ggplot2::element_blank(), # No x-axis ticks
      legend.direction = "horizontal", # Layout legend categories horizontally
      legend.position = "inside",
      legend.position.inside = c(0.5,0.95), # Legend centered and near top
      legend.text = ggplot2::element_text(lineheight = 0.25, size = ggplot2::rel(1)),
      legend.title = ggplot2::element_blank(), # No legend title
      # Remove legend background
      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    filename = filename,
    plot = bplt,
    path = path,
    width = chart_width,
    height = chart_height,
    units = "in",
    device = "png",
    dpi = 300
  )
}
