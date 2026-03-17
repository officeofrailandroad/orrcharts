
#' Side-By-Side Bar Chart
#'
#' Side-by-side bar chart to compare changes over time in a metric between TOCs.
#' @inheritParams ggplot2::ggsave
#' @param chart_width,chart_height Width and height in inches of the PNG output.
#' @param left_bar_title,right_bar_title The text which appears above each set
#'   of bars.
#' @param left_bar_colour A string hexcode for the colour of the bars in the
#'   left-hand side bar chart. Default is for ORR dark blue.
#' @param right_bar_colour_positive,right_bar_colour_negative String hexcodes
#'   for the colours of the right-hand side bar chart. `_positive` colours
#'   positive valued bars and `_negative` colours those below zero. Default is
#'   for green/red but could also use ORR yellow for both.
#' @param right_bar_order_descending `TRUE` or `FALSE`. If `TRUE` both bar
#'   charts are arranged in descending order of the right-hand bar chart. If
#'   `FALSE` then ascending.
#' @param left_bar_labeller,right_bar_labeller Functions which turn data numeric
#'   values into character labels.
side_by_side_bar <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 7.3,
    left_bar_title = "METRIC, TIME PERIOD",
    right_bar_title = "Change from TIME PERIOD",
    left_bar_colour = "#253268",
    right_bar_colour_positive = "#28994b",
    right_bar_colour_negative = "#B1173B",
    right_bar_order_descending = TRUE,
    left_bar_labeller = label_orr_comma,
    right_bar_labeller = label_orr_percent
) {


  data <- data[, 1:3]
  colnames(data) <- c("toc", "value", "change")

  plot_data <- data %>%
    # sort the values by the change column. Either descending or ascending based
    # on argument
    {
      if(right_bar_order_descending)
        dplyr::arrange(., dplyr::desc(change))
      else dplyr::arrange(., change)
      } %>%
    # create a variable to sort the bars later
    dplyr::mutate(bar_order = dplyr::row_number()) %>%
    # collapse value and change into single column of numbers for ggplot
    tidyr::pivot_longer(value:change, names_to = "col") %>%
    dplyr::mutate(
      col = base::factor(col, levels = c("value", "change")),
      # Create data labels
      value_label = dplyr::case_when(
        col == "value" ~ left_bar_labeller(value),
        col == "change" ~ right_bar_labeller(value),
        TRUE ~ ""
      ),
      # Colours for the bars
      value_fill = dplyr::case_when(
        col == "value" ~ "value",
        col == "change" & value >= 0 ~ "change_pos",
        col == "change" & value < 0 ~ "change_neg",
        TRUE ~ "value"
      )) %>%
    dplyr::group_by(col) %>%
    dplyr::mutate(
      # Figure out the width of the chart in axis units
      col_min_value = base::min(value, na.rm = TRUE),
      col_max_value = base::max(value, na.rm = TRUE),
      col_width = (col_max_value - col_min_value)) %>%
    dplyr::ungroup()

  # Set font family and size
  font_fam <- "Arial"
  font_size <- 37

  showtext::showtext_auto()

  # Set the titles for each chart
  col_titles <- c(
    "value" = stringr::str_wrap(left_bar_title, width = 35),
    "change" = stringr::str_wrap(right_bar_title, width = 35)
  )

  # Set the bar colours
  bar_colour_types <- c(
    value = left_bar_colour,
    change_pos = right_bar_colour_positive,
    change_neg = right_bar_colour_negative
    )

  # Number of TOCs/bars
  n_tocs = dplyr::n_distinct(plot_data$toc, na.rm = TRUE)


  plt <- plot_data %>%
    ggplot2::ggplot(ggplot2::aes(reorder(toc, -bar_order), value)) +
    ggplot2::geom_col(ggplot2::aes(fill = value_fill)) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = col_min_value - 0.1*col_width, # Data labels half width of chart to left
        label = value_label
        ),
      family = font_fam,
      size = 0.90*font_size, # Data labels look bigger so small adjustment here
      hjust = "right",
      size.unit = "pt") +
    ggplot2::geom_hline(yintercept = 0, colour = "black", size = 0.2) + # vertical axis lines
    ggplot2::geom_vline(xintercept = 0.4, colour = "black", size = 0.2) + # bottom horizontal line
    ggplot2::geom_vline(xintercept = n_tocs + 0.6, colour = "black", size = 0.2) + # top horizontal line
    ggplot2::facet_wrap(
      col ~ .,
      scales = "free_x",
      labeller = ggplot2::labeller(.default = label_value, col = col_titles) # Sets chart titles
      ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.3, 0))) +
    ggplot2::scale_x_discrete(expand = ggplot2::expansion(mult = c(0,0))) +
    ggplot2::scale_fill_manual(values = bar_colour_types) +
    ggplot2::guides(fill = "none") + # No fill legend for colours
    ggplot2::theme_classic(base_family = font_fam) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = font_size),
      axis.title = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      strip.background = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        hjust = 0, # Align left
        vjust = 0,
        face = "bold",
        lineheight = 0.3,
        margin = ggplot2::margin(t = 0, l = 5, b = 5, r = 0) # nudge text to line up with data labels
        ),
      panel.spacing.x = ggplot2::unit(0, "lines"), # remove padding between charts so horizontal lines go full width
      plot.margin = ggplot2::margin(0,10,0,0)
    )

  # Save plot in standard size
  ggplot2::ggsave(
    filename = filename,
    path = path,
    plot = plt,
    width = chart_width,
    height = chart_height,
    units = "in",
    device = "png",
    dpi = 300
  )
}
