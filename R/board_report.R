
label_year_from_key <- function(fpk) {
  paste0(
    stringr::str_sub(fpk, 3, 4),
    "-",
    stringr::str_sub(fpk, 7, 8)
  )
}

label_period_from_key <- function(fpk) {
  paste0(
    label_year_from_key(fpk),
    " P",
    stringr::str_sub(fpk, -2, -1)
  )
}


#' Board Report GB Line Chart
#'
#' Create a line chart for the board report with GB values in black and regions in grey.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param gb_target_value The value of the end of year target for the GB metric.
#' @param last_point_labeller A function which determines how data labels are displayed.
#' @export
board_report_gb_line_chart <- function(
  data,
  filename,
  path = NULL,
  gb_target_value,
  chart_width = 2.6575,
  chart_height = 1.6457,
  y_axis_breaks = ggplot2::waiver(),
  y_axis_labeller = scales::label_comma(),
  last_point_labeller = scales::label_comma()
) {

  fixed_data <- data[,1:3]
  names(fixed_data) <- c("financial_period_key", "region", "value")

  # List all the period keys
  all_periods <- fixed_data %>%
    dplyr::pull(.data$financial_period_key) %>%
    unique()

  # Only label periods at the end of the year.
  break_periods <- all_periods[
    which(stringr::str_sub(all_periods, -2, -1) == "13")
  ]

  # Create the labels to display on the plot
  break_labels <- fpk_year_label(break_periods)

  # Set which line to highlight
  highlight <- "Great Britain"

  # Sort region levels so GB is last
  # This will put the highlighted line on top of the others
  region_levels <- unique(fixed_data$region)
  region_levels <- c(region_levels[!region_levels == highlight], highlight)

  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 8

  # Colours for the lines
  highlight_colour <- "black"
  supress_colour <- "grey80"

  # Create data to plot
  plot_data <- fixed_data %>%
    dplyr::filter(!is.na(.data$value)) %>%
    dplyr::mutate(
      is_last_point = (
        .data$financial_period_key == max(.data$financial_period_key) &
          .data$region == highlight),
      point_label = last_point_labeller(.data$value),
      plot_data_label = ifelse(
        .data$is_last_point,
        .data$point_label,
        NA
      ),
      region = factor(region, levels = region_levels)
    )

  # Plot titles
  plt_title <- "**Great Britain (NR network only)** <span style='color:grey;'>(regions in grey)</span>"

  target_label <- last_point_labeller(gb_target_value)
  last_point <- plot_data %>%
    dplyr::filter(.data$is_last_point)
  latest_period_label <- label_period_from_key(last_point$financial_period_key[1])
  latest_period_value <- last_point_labeller(last_point$value[1])
  plt_subtitle <- glue::glue("{latest_period_label}: {latest_period_value}, EoY target: {target_label}")

  gb_plot <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes(
      x = .data$financial_period_key,
      y = .data$value,
    )
  ) +
    ggplot2::geom_line(
      ggplot2::aes(
        colour = (.data$region == highlight),
        linewidth = (.data$region == highlight),
        group = .data$region
      )
    ) +
    ggplot2::geom_point(
      ggplot2::aes(size = .data$is_last_point),
      colour = highlight_colour,
      na.rm = TRUE
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$plot_data_label),
      na.rm = TRUE,
      size = 0.8 * font_size,
      hjust = -0.2
    ) +
    ggplot2::coord_cartesian(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::scale_y_continuous(
      name = NULL,
      labels = y_axis_labeller
    ) +
    ggplot2::scale_x_discrete(
      name = NULL,
      breaks = break_periods,
      labels = break_labels,
      expand = expansion(mult = c(0,0))
    ) +
    ggplot2::scale_color_manual(
      values = c(`TRUE` = highlight_colour, `FALSE` = supress_colour)
    ) +
    ggplot2::scale_size_manual(
      values = c(`TRUE` = 1, `FALSE` = NA)
    ) +
    ggplot2::scale_linewidth_manual(
      values = c(`TRUE` = 1, `FALSE` = 0.5)
    ) +
    ggplot2::guides(colour = "none", linewidth = "none", size = "none") +
    ggplot2::theme(
      text = ggplot2::element_text(size = (font_size * ggplot2::.pt), family = font_fam),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), colour = "black"),
      axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(r = 0.05, unit = "cm")),
      axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = 0.05, unit = "cm")),
      margins = ggplot2::margin(t = 0.1, r = 0.7, b = 0, l = 0, unit = "cm"),
      axis.line = ggplot2::element_blank(),
      panel.grid = ggplot2::element_line(linewidth = 0.2),
      axis.ticks.y = ggplot2::element_blank(),
      plot.title = ggtext::element_markdown(size = ggplot2::rel(1), margin = ggplot2::margin(b = 0.05, unit = "cm")),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1), margin = ggplot2::margin())
    ) +
    ggplot2::labs(
      title = plt_title,
      subtitle = plt_subtitle
    )

  ggplot2::ggsave(
    filename = filename,
    plot = gb_plot,
    path = path,
    units = "in",
    height = chart_height,
    width = chart_width,
    dpi = 300,
    device = "png"
  )
}


#' Board Report Region Bars
#'
#' Create a board report plot with bars and points for targets.
#' @inheritParams ggplot2::ggsave
#' @inheritParams quarterly_bar
#' @param region_targets A dataframe with two columns containing the region name and target value.
#' @param period_number The current period number, shown in the subtitle.
#' @param metric_name The name of the metric plotted, shown in the chart title.
#' @param bar_colour,point_colour Set the colours of bars and points in the plot.
#' @export
board_report_region_bars <- function(
    data,
    filename,
    path = NULL,
    region_targets,
    period_number,
    metric_name,
    chart_width = 2.6575,
    chart_height = 1.6457,
    bar_colour = "#253268",
    point_colour = "#D8730F",
    data_labeller = label_orr_comma()
    ) {

  fixed_data <- data[,1:2]
  names(fixed_data) <- c("region", "value")

  region_targs <- region_targets[,c(1:2)]
  names(region_targs) <- c("region", "target")

  font_fam <- "Arial"
  showtext::showtext_auto()
  font_size <- 8

  # Calculate if black or white text will have best contrast with bar colour
  bar_hcl <- farver::decode_colour(bar_colour, "rgb", "hcl")
  text_colour <- base::ifelse(bar_hcl[, "l"] > 50, "black", "white")

  plt_subtitle <- glue::glue(
    "<span style='color:{bar_colour};'>**{metric_name}**</span> (at period {period_number}) vs <span style='color:{point_colour};'>**Target**</span>"
  )

  plt_data <- fixed_data %>%
    dplyr::left_join(region_targs, by = "region")

  reg_bars_plot <- plt_data %>%
    dplyr::mutate(
      value_label = data_labeller(.data$value),
      target_label = data_labeller(.data$target)
    ) %>%
    ggplot2::ggplot(aes(x = .data$region)) +
    ggplot2::geom_col(
      ggplot2::aes(y = .data$value),
      fill = bar_colour
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$target),
      colour = point_colour,
      na.rm = TRUE,
      size = 2
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = 0,
        label = .data$value_label
      ),
      hjust = 0,
      nudge_y = 2,
      colour = text_colour,
      size = font_size
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        .data$region,
        .data$target,
        label = .data$target_label
      ),
      hjust = 0,
      nudge_y = 6,
      colour = point_colour,
      size = font_size,
      na.rm = TRUE
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_x_discrete(
      name = NULL,
      limits = rev
    ) +
    ggplot2::scale_y_continuous(
      name = NULL,
      label = scales::label_percent(scale = 1, accuracy = 1),
      limits = c(0, 100),
      position = "right",
      expand = ggplot2::expansion(mult = c(0, 0))
    ) +
    ggplot2::labs(
      subtitle = plt_subtitle
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(size = (font_size * ggplot2::.pt), family = font_fam, colour = "black"),
      axis.text = ggplot2::element_text(size = ggplot2::rel(1), colour = "black"),
      axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(0, 0, 0.05, 0, unit = "cm")),
      axis.line.x.top = ggplot2::element_line(colour = "black", lineend = "round"),
      axis.line.y.left = ggplot2::element_line(colour = "black"),
      axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(0, 0.1, 0, 0, unit = "cm")),
      axis.ticks.length.x.top = ggplot2::unit(0.1, units = "cm"),
      axis.ticks.x.top = ggplot2::element_line(color = "black"),
      panel.grid = ggplot2::element_blank(),
      margins = ggplot2::margin(0, 0.6, 0, 0, unit = "cm"),
      plot.title.position = "plot",
      plot.subtitle = ggtext::element_markdown(size = ggplot2::rel(1), margin = ggplot2::margin(0.1, 0, 0.1, 0.05, unit = "cm"))
    )

  ggplot2::ggsave(
    filename = filename,
    plot = reg_bars_plot,
    path = path,
    units = "in",
    height = chart_height,
    width = chart_width,
    dpi = 300,
    device = "png"
  )
}

