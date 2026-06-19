

#' Donut Chart
#'
#' A simple donut chart.
#' @inheritParams bar_chart
#' @param colours The colours for the donut sections. Defaults to ORR colours.
#' @param labels_gap_size Gap between the outside of the donut and the centre of
#'   the text labels.
#' @param as_pie_chart If true will remove hole in donut and show as pie chart.
#' @export
donut_chart <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    colours = orr_colours(),
    data_labeller = scales::label_number(scale = 1, accuracy = 1),
    labels_gap_size = 1,
    as_pie_chart = FALSE
    ) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height, colours, data_labeller
  )
  assertthat::assert_that(
    assertthat::is.scalar(labels_gap_size)
  )
  assertthat::assert_that(
    assertthat::is.flag(as_pie_chart)
  )

  # Fix the size and names of the data
  fixed_data <- data[,1:2]
  names(fixed_data) <- c("category","value")

  # Remove names from list of colours - it interferes with ggplot
  base::names(colours) <- NULL

  plot_data <- fixed_data %>%
    dplyr::mutate(
      frac = .data$value / sum(.data$value),
      ymax = cumsum(.data$frac),
      ymin = c(0, utils::head(.data$ymax, n = -1)),
      frac_label = data_labeller(.data$value),
      label = paste(stringr::str_wrap(.data$category, 12), .data$frac_label, sep = "\n")
    )

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  donut_hole_size <- ifelse(as_pie_chart, 0, 2)
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
    ggplot2::geom_rect(colour = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        y = (.data$ymax + .data$ymin) / 2,
        x = (donut_hole_size + donut_ring_width) + labels_gap_size,
        label = .data$label,
        colour = .data$category
      ),
      hjust = "center",
      vjust = "middle",
      family = font_fam,
      fontface = "bold",
      size = font_size,
      lineheight = 0.25
    ) +
    ggplot2::coord_polar(theta = "y", clip = "off") +
    ggplot2::xlim(c(0, 7)) +
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
    units = "in",
    device = "png",
    dpi = 300
  )

}
