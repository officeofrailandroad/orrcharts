
#' Ggplot2 To ORR
#'
#' Take a custom ggplot and apply basic ORR theme and styling, then save as a
#' PNG file. Try to use the chart functions where ever possible but this is
#' sometimes helpful for a bespoke chart.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param plot A ggplot2 plot object.
#' @export
ggplot_to_orr <- function(
  plot,
  filename,
  path = NULL,
  chart_width = 6.7,
  chart_height = 3.567
) {

  # Set font family and size
  font_fam <- .text_font_family
  showtext::showtext_auto()
  font_size <- .text_font_size

  plt <- plot +
    ggplot2::theme(
      text = ggplot2::element_text(
        family = font_fam,
        size = (font_size * ggplot2::.pt)
      ),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), colour = "black"),
      legend.text = ggplot2::element_text(
        lineheight = .text_line_height,
        size = ggplot2::rel(1)
      )
    )

  # Save plot
  ggplot2::ggsave(
    filename = filename,
    plot = plt,
    path = path,
    width = chart_width,
    height = chart_height,
    units = .plot_device_units,
    device = "png",
    dpi = .plot_png_dpi
  )
}
