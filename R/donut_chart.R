

#' Donut Chart
#'
#' A simple donut chart.
#' @inheritParams bar_chart
#' @param colours The colours for the donut sections. Defaults to ORR colours.
#' @export
donut_chart <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    colours = orr_colours(),
    data_labeller = scales::label_number(scale = 1, accuracy = 1)
    ) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height, colours, data_labeller
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

  dplt <- plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(
        fill = .data$category,
        ymax = .data$ymax,
        ymin = .data$ymin,
        xmax = 4,
        xmin = 2
      )
    ) +
    ggplot2::geom_rect(colour = "white") +
    ggplot2::geom_text(
      ggplot2::aes(
        y = (.data$ymax + .data$ymin) / 2,
        x = 6,
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
    ggplot2::xlim(c(0, 6)) +
    ggplot2::theme_minimal() +
    ggplot2::scale_fill_manual(values = colours) +
    ggplot2::scale_colour_manual(values = colours) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      margins = ggplot2::margin(0)
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
