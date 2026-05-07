

#' Horizontal Bar Chart
#'
#' A simple horizontal bar chart to display a single set of values
#' @inheritParams bar_chart
#' @param outside_bar_threshold Controls when a data label is placed inside or
#'   outside the bar. Increase the value if data labels look cramped in small
#'   bars.
#' @param outside_bar_distance Controls how far outside the bar labels are
#'   shown. Increase to move further from the bar.
#' @export
horizontal_bar <- function(
    data,
    filename,
    path,
    chart_width = 6.7,
    chart_height = 3.567,
    data_labeller = scales::label_number(),
    outside_bar_threshold = 0.1,
    outside_bar_distance = 0.05
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

  fixed_data <- data[,1:2]
  names(fixed_data) <- c("name", "value")

  plot_data <- fixed_data %>%
    dplyr::mutate(
      name_label = stringr::str_wrap(.data$name, 20),
      value_label = data_labeller(.data$value),
      value_max = max(.data$value),
      value_min = min(0, .data$value),
      value_range = .data$value_max - .data$value_min,
      label_outside = (abs(.data$value) / .data$value_range < outside_bar_threshold),
      label_y = ifelse(
        .data$label_outside,
        .data$value + outside_bar_distance * .data$value_range,
        .data$value / 2
        )
    )

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  bar_colour <- "#253268"

  bplt <- plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(stats::reorder(.data$name_label, .data$value))
    ) +
    ggplot2::geom_col(
      ggplot2::aes(
        y = .data$value
      ),
      fill = bar_colour
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        label = .data$value_label,
        y = .data$label_y,
        colour = .data$label_outside
      ),
      hjust = "centre",
      vjust = "middle",
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
    ggplot2::scale_colour_manual(values = c("white","black")) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = (font_size * ggplot2::.pt)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), lineheight = 0.25),
      axis.text.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::guides(colour = "none")

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


