

#' Horizontal Bar Chart
#'
#' A simple horizontal bar chart to display a single set of values
#' @inheritParams bar_chart
#' @param outside_bar_threshold Controls when a data label is placed inside or
#'   outside the bar. Increase the value if data labels look cramped in small
#'   bars.
#' @param outside_bar_distance Controls how far outside the bar labels are
#'   shown. Increase to move further from the bar.
#' @param legend_position Set location of the legend in the plot. Default is
#'   bottom right. c(0,0) is bottom left.
#' @export
horizontal_bar <- function(
    data,
    filename,
    path,
    chart_width = 6.7,
    chart_height = 3.567,
    data_labeller = scales::label_number(),
    bar_colours = orr_colours(),
    outside_bar_threshold = 0.1,
    outside_bar_distance = 0.05,
    show_legend = FALSE,
    legend_position = c(0.8,0.2)

) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height,
    data_labeller
  )
  assertthat::assert_that(
    assertthat::is.scalar(outside_bar_threshold),
    assertthat::is.scalar(outside_bar_distance),
    outside_bar_threshold >= 0,
    outside_bar_distance >= 0,
    msg = "outside_bar_threshold and outside_bar_distance must be single non-negative numbers"
  )

  fixed_data <- data
  data_cols <- ncol(fixed_data)
  names(fixed_data) <- c(
    "name",
    # Wrap column names to keep legend compact
    stringr::str_wrap(names(fixed_data[2:data_cols]), 20)
  )
  col_labels <- names(fixed_data)[2:data_cols]

  plot_data <- fixed_data %>%
    tidyr::pivot_longer(- dplyr::all_of("name"), names_to = "col") %>%
    dplyr::mutate(
      name_label = stringr::str_wrap(.data$name, 20),
      # fix order of dodged bars to match order of columns in data supplied
      col = factor(col, levels = rev(col_labels)),
      value_label = data_labeller(.data$value),
      value_max = max(.data$value),
      value_min = min(0, .data$value),
      value_range = .data$value_max - .data$value_min, # width of plot
      label_outside = (abs(.data$value) / .data$value_range < outside_bar_threshold),
      # Set data label position
      label_y = ifelse(
        .data$label_outside,
        .data$value + outside_bar_distance * .data$value_range,
        .data$value / 2
      )
    )

  # Should legend be shown
  fill_legend = "none"
  if(show_legend) fill_legend <- "legend"

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  names(bar_colours) <- NULL

  bplt <- plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        stats::reorder(.data$name_label, .data$value),
        fill = .data$col,
        group = .data$col # important for dodged labels
      )
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        y = .data$value
      ),
      position = "dodge",
      colour = "white" # white space between dodged bars
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data$value_label,
        y = .data$label_y,
        colour = .data$label_outside
      ),
      # position data labels inside each bar
      position = ggplot2::position_dodge(width = 0.9),
      family = font_fam,
      size = font_size
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_classic() +
    ggplot2::scale_y_continuous(
      name = NULL,
      expand = c(0, 0)
    ) +
    ggplot2::scale_x_discrete(name = NULL) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = bar_colours,
      breaks = col_labels # fixes order of legend to match order of bars
    ) +
    ggplot2::scale_colour_manual(values = c("white","black")) +
    ggplot2::guides(colour = "none", fill = fill_legend) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = (font_size * ggplot2::.pt)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), lineheight = 0.25),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      legend.position = "inside",
      legend.direction = "vertical", # Layout legend categories vertical
      legend.position.inside = legend_position,
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


