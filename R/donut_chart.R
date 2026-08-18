

#' Donut Chart
#'
#' A simple donut chart.
#' @inheritParams bar_chart
#' @param colours The colours for the donut sections. Defaults to ORR colours.
#' @param labels_gap_size Gap between the outside of the donut and the centre of
#'   the text labels.Labels are positions using ggrepel to avoid overlaps. This
#'   parameter sets the starting distance before the repel algorithm is run.
#' @param as_pie_chart If true will remove hole in donut and show as pie chart.
#' @param min_label_segment_length Minimum length of line between the text label
#'   and the edge of the segment. Hidden if smaller than this.
#' @param outer_chart_limit The upper limit on x-axis which sets that outer size
#'   of the chart.
#' @param centre_label Text displayed at the centre of the chart
#' @export
donut_chart <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    colours = orr_colours(),
    data_labeller = scales::label_number(scale = 1, accuracy = 1),
    labels_gap_size = 2,
    min_label_segment_length = 0.4,
    outer_chart_limit = 7,
    as_pie_chart = FALSE,
    centre_label = ""
    ) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height, colours, data_labeller
  )
  assertthat::assert_that(
    assertthat::is.scalar(labels_gap_size),
    labels_gap_size >= 0
  )
  assertthat::assert_that(
    assertthat::is.flag(as_pie_chart)
  )
  assertthat::assert_that(
    assertthat::is.scalar(min_label_segment_length),
    min_label_segment_length >= 0
  )
  assertthat::assert_that(
    assertthat::is.scalar(outer_chart_limit),
    outer_chart_limit >= 0
  )
  assertthat::assert_that(
    assertthat::is.string(centre_label)
  )

  # Fix the size and names of the data
  fixed_data <- data[,1:2]
  names(fixed_data) <- c("category","value")

  # Remove names from list of colours - it interferes with ggplot
  base::names(colours) <- NULL

  cat_levels_order <- unique(fixed_data$category)

  plot_data <- fixed_data %>%
    dplyr::mutate(
      frac = .data$value / sum(.data$value),
      ymax = cumsum(.data$frac),
      ymin = c(0, utils::head(.data$ymax, n = -1)),
      frac_label = data_labeller(.data$value),
      label = paste(stringr::str_wrap(.data$category, 12), .data$frac_label, sep = "\n"),
      category = factor(.data$category, levels = cat_levels_order)
    )

  # Set font family and size
  font_fam <- .text_font_family
  showtext::showtext_auto()
  font_size <- .text_font_size

  donut_hole_size <- ifelse(as_pie_chart, 0.01, 2)
  donut_ring_width <- 4


  dplt <- plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        fill = .data$category,
        ymax = .data$ymax,
        ymin = .data$ymin,
        xmax = donut_ring_width,
        xmin = donut_hole_size
      )
    ) +
    ggplot2::geom_rect(colour = "white", linewidth = 0.3) +
    ggrepel::geom_text_repel(
      ggplot2::aes(
        y = (.data$ymax + .data$ymin) / 2,
        x = donut_ring_width,
        label = .data$label,
        colour = .data$category
      ),
      hjust = "center",
      vjust = "middle",
      family = font_fam,
      fontface = "bold",
      size = font_size,
      lineheight = 0.25,
      nudge_x = labels_gap_size,
      point.padding = 0.1,
      min.segment.length = min_label_segment_length
    ) +
    ggplot2::annotate(
      "text",
      x = 0, y = 0,
      hjust = "center", vjust = "middle",
      size = font_size,
      family = font_fam,
      fontface = "bold",
      lineheight = .text_line_height,
      label = centre_label
    ) +
    ggplot2::coord_polar(theta = "y", clip = "off") +
    ggplot2::xlim(c(0, outer_chart_limit)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      margins = ggplot2::margin_auto(0)
    ) +
    ggplot2::guides(
      fill = "none",
      colour = "none"
    )

  ggplot2::ggsave(
    filename = filename,
    plot = dplt,
    path = path,
    width = chart_width,
    height = chart_height,
    units = .plot_device_units,
    device = "png",
    dpi = .plot_png_dpi
  )

}
