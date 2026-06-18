

#' Bar Chart
#'
#' A simple or stacked bar chart in ORR style.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param bar_colours An array of colour hex-codes. Defaults to ORR colours.
#' @param x_axis_labels Array of labels to display on the x-axis
#' @param data_labeller A function which controls how the data labels over the bars are displayed.
#' @param show_legend Should the legend be shown on the chart. Defaults to `TRUE`.
#' @param hide_y_axis If `TRUE` will remove y axis line, title and grid lines. Defaults to `FALSE`.
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
    x_axis_labels = ggplot2::waiver(),
    data_labeller = label_orr_comma(),
    show_legend = TRUE,
    hide_y_axis = FALSE
) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height, bar_colours, y_axis_breaks,
    y_axis_labeller, data_labeller, show_legend
  )
  assertthat::assert_that(assertthat::is.flag(hide_y_axis))

  # Set name of first column of data
  base::colnames(data)[1] <- "category"
  value_type_names <- colnames(data)[2:ncol(data)]
  category_names <- data$category

  # Remove names from list of colours - it interferes with ggplot
  base::names(bar_colours) <- NULL

  # Check there are enough colours
  assertthat::assert_that(
    length(bar_colours) >= length(value_type_names),
    msg = "Not enough bar colours supplied for the number of columns"
  )

  # Calculate if black or white text will have best contrast with bar colours
  bar_hcl <- farver::decode_colour(bar_colours, "rgb", "hcl")
  text_colours <- base::ifelse(bar_hcl[, "l"] > 50, "black", "white")
  names(text_colours)[1:length(value_type_names)] <- value_type_names

  # Set y-axis limits
  y_limits <- NULL
  if(! methods::is(y_axis_breaks, "waiver")) {
    # If axis breaks are specified, set the max of y axis to the largest break.
    y_limits <- c(0, max(y_axis_breaks))
  }

  # Should legend be shown
  fill_legend = "none"
  if(show_legend) fill_legend <- "legend"

  # Should y axis and lines be hidden
  y_axis_theme <- ggplot2::theme()
  if (hide_y_axis) {
    y_axis_theme <- ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor.y = ggplot2::element_blank()
    )
  }

  # Set font family and size
  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 13

  # Thresholds for data labels outside bars
  outside_bar_threshold <- 0.1
  outside_bar_distance <- 0.05

  plot_data <- data %>%
    tidyr::pivot_longer(- dplyr::all_of("category")) %>%
    dplyr::mutate(
      # Fix category and stacked values order
      category = factor(.data$category, levels = category_names, ordered = TRUE),
      name = factor(.data$name, levels = value_type_names, ordered = TRUE),
      data_label = data_labeller(.data$value),
      # Hide zero value data labels
      data_label = ifelse(.data$value == 0, NA_character_, .data$data_label),
      # Wrap long category names to display better in the legend
      # name_label = stringr::str_wrap(.data$name, width = 20),
    ) %>%
    dplyr::group_by(.data$category) %>%
    dplyr::arrange(.data$name) %>%
    dplyr::mutate(
      # Total for all categories that will be stacked
      category_value_sum = sum(.data$value, na.rm = TRUE),
      value_max_point = cumsum(.data$value),
      value_min_point = .data$value_max_point - .data$value,
      value_mid_point = .data$value_min_point + (.data$value_max_point - .data$value_min_point) / 2
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Figure out bar range for dodging data labels in small bars
      cat_value_max = max(.data$category_value_sum, 0),
      cat_value_min = min(.data$category_value_sum, 0),
      value_range = .data$cat_value_max - .data$cat_value_min,
      label_outside = (abs(.data$value) / .data$value_range < outside_bar_threshold),
      # Set data label position
      label_y = ifelse(
        .data$label_outside,
        .data$value_max_point + outside_bar_distance * .data$value_range,
        .data$value_mid_point
      ),
      label_colour_group = ifelse(
        .data$label_outside,
        "_outside",
        as.character(.data$name)
      )
    )

  # Add outside labels to text colour mapping
  text_colours <- c(`_outside` = "black", text_colours)

  bplt <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(x = .data$category, y = .data$value)) +
    ggplot2::geom_col(
      ggplot2::aes(
        y = .data$value,
        fill = .data$name
      ),
      colour = "white",
      linewidth = ggplot2::rel(0.3),
      position = ggplot2::position_stack(reverse = TRUE)
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data$label_y,
        label = .data$data_label,
        colour = .data$label_colour_group
      ),
      # Put labels in the middle of the bars
      # position = ggplot2::position_stack(reverse = TRUE),
      family = font_fam,
      size = font_size,
      na.rm = TRUE
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
      name = NULL,
      labels = x_axis_labels
    ) +
    ggplot2::scale_fill_manual(values = bar_colours, labels = function(x) stringr::str_wrap(x, width = 20)) +
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
    ) +
    y_axis_theme

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
