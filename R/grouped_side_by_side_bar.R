
#' Grouped Side-By-Side Bar Chart
#'
#' Two columns of horizontal bars, grouped with titles.
#' @inheritParams ggplot2::ggsave
#' @inheritParams side_by_side_bar
#' @param data A dataframe with 4 columns. The first column should hold the bar categories, which set the y-axis labels (not in bold). The second column hold the groups of the bar categories, shown on the y-axis labels in bold. Column 3 holds the values for the left had set of bars. Column 4 holds the values for the right hand set of bars.The vertical order of the bars within the group matches the order they are supplied in the dataframe.
#' @param right_bar_colour String hexcodes for the right hand set of bars.
#' @param text_outside_bar_threshold Proportion of largest bar at which data labels are shown outside the bar. Default is 0.2.
#' @param rightside_margin Expand the margin on the right-hand side of the plot if a data label is cut off.
#' @export
grouped_side_by_side_bar <- function(
    data,
    filename,
    path = NULL,
    chart_width = 6.7,
    chart_height = 3.567,
    left_bar_title = "METRIC, TIME PERIOD",
    right_bar_title = "Change from TIME PERIOD",
    left_bar_colour = "#253268",
    right_bar_colour = "#D8730F",
    left_bar_labeller = label_orr_comma(),
    right_bar_labeller = label_orr_percent(),
    text_outside_bar_threshold = 0.2,
    rightside_margin = NA
    ) {
  # Check input parameters
  assert_chart_params(
    data, filename, path, chart_width, chart_height,
    left_bar_labeller, right_bar_labeller
  )
  assertthat::assert_that(
    assertthat::is.string(left_bar_title),
    assertthat::is.string(right_bar_title),
    msg = "left_bar_title and right_bar_title need to be strings of length 1."
  )
  assertthat::assert_that(
    assertthat::is.string(left_bar_colour),
    assertthat::is.string(right_bar_colour),
    msg = "left_bar_colour, right_bar_colour need to be colour strings of length 1"
  )
  assertthat::assert_that(
    assertthat::is.number(text_outside_bar_threshold),
    msg = "text_outside_bar_threshold need to be numeric of length 1"
  )
  assertthat::assert_that(
    assertthat::is.number(rightside_margin) | is.na(rightside_margin),
    msg = "rightside_margin need to be numeric of length 1 or NA"
  )


  # Set the colours of the bars and data labels
  bar_colours <- c(value = left_bar_colour, change = right_bar_colour)
  # Set data label colour to give best contrast with bar colour
  bar_hcl <- farver::decode_colour(bar_colours, "rgb", "hcl")
  text_colours <- base::ifelse(bar_hcl[, "l"] > 50, "black", "white")

  # Fix the number and names of the columns
  fixed_data <- data[, 1:4]
  base::names(fixed_data) <- c("type", "group", "value", "change")


  sort_data <- fixed_data %>%
    # Use dense rank (alphabetical) to sort the vertical bar groups
    # If you change this remember to also change the plot_groups_data sort below
    dplyr::mutate(group_id = dplyr::dense_rank(.data$group)) %>%
    dplyr::group_by(.data$group) %>%
    dplyr::mutate(
      # Keep the bars in the order provided
      group_order = dplyr::row_number()
    ) %>%
    dplyr::ungroup()

  # Create the group labels
  plot_groups <- sort_data %>%
    dplyr::mutate(
      group_label = base::paste0(.data$group, ":")
    ) %>%
    dplyr::pull(.data$group_label) %>%
    base::unique()

  # Make empty data rows with the group labels to insert into the data
  plot_groups_data <- tibble::tibble(
    type = plot_groups
  ) %>%
    dplyr::mutate(
      value = NA,
      change = NA,
      group_order = 0,
      group_id = dplyr::dense_rank(.data$type)
    )

  # Put the group labels in the plot data as empty rows
  plot_data <- sort_data %>%
    dplyr::select(-.data$group) %>%
    dplyr::bind_rows(plot_groups_data) %>%
    dplyr::group_by(.data$group_id) %>%
    dplyr::mutate(
      group_n = dplyr::n()
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # Set the order the bars and groups are plotted
      plot_order = .data$group_id + .data$group_order / .data$group_n,
      # Set the type label font face. Group labels should be bold
      label_face = base::ifelse(.data$type %in% plot_groups, "bold", "plain")
    ) %>%
    dplyr::arrange(.data$plot_order)

  # Get the type label font face.
  # Reverse order needed because chart is reverse reordered below to read top to bottom
  y_lab_face <- plot_data %>%
    dplyr::pull(.data$label_face) %>%
    base::rev()

  # Pivotted plot data
  piv_plot_data <- plot_data %>%
    tidyr::pivot_longer(tidyselect::all_of(c("value","change"))) %>%
    dplyr::group_by(.data$name) %>%
    dplyr::mutate(
      # Find values to help determine if text label should be inside or outside the bar
      max_value = base::max(.data$value, na.rm = TRUE),
      min_value = base::min(c(.data$value, 0), na.rm = TRUE),
      val_range = .data$max_value - .data$min_value,
      value_prop_of_max = base::abs(.data$value / .data$val_range)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      # If bar is less than 20% of the biggest bar, put label outside
      text_label_position_group = base::ifelse(.data$value_prop_of_max < text_outside_bar_threshold, "outside", .data$name),
      # Calculate text label position
      text_label_y = base::ifelse(.data$text_label_position_group == "outside", .data$value + base::sign(.data$value) * (0.03 * .data$val_range), .data$value - base::sign(.data$value) * (0.03 * .data$val_range)),
      # text_label_y = 0,
      # Create the data labels text
      text_label = base::ifelse(.data$name == "value", left_bar_labeller(.data$value), right_bar_labeller(.data$value)),
      # Set order of the facets
      name = base::factor(.data$name, levels = c("value","change"))
      )

  # Set font family and size
  font_fam <- "Arial"
  font_size <- 37
  showtext::showtext_auto()

  # Set titles above bar columns
  col_titles <- c(
    value = stringr::str_wrap(left_bar_title, 31),
    change = stringr::str_wrap(right_bar_title, 31)
  )



  gsbs_plot <- piv_plot_data %>%
    ggplot2::ggplot(
      ggplot2::aes(x = stats::reorder(.data$type, -.data$plot_order))) +
    ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
    ggplot2::geom_col(ggplot2::aes(y = .data$value, fill = .data$name), na.rm = TRUE) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data$text_label_y,
        label = ifelse(
          (.data$value >= 0 & .data$text_label_position_group != "outside") |
            (.data$value < 0 & .data$text_label_position_group == "outside"),
          .data$text_label, NA),
        colour = .data$text_label_position_group
        ),
      na.rm = TRUE,
      hjust = 1,
      family = font_fam,
      size = font_size / ggplot2::.pt
    ) +
    ggplot2::geom_text(
      ggplot2::aes(
        y = .data$text_label_y,
        label = ifelse(
          (.data$value >= 0 & .data$text_label_position_group != "outside") |
            (.data$value < 0 & .data$text_label_position_group == "outside"),
          NA, .data$text_label),
        colour = .data$text_label_position_group
      ),
      na.rm = TRUE,
      hjust = 0,
      family = font_fam,
      size = font_size / ggplot2::.pt
    ) +
    ggplot2::facet_wrap(
      ~ .data$name,
      scales = "free_x",
      labeller = ggplot2::labeller(name = col_titles)
    ) +
    ggplot2::coord_flip(clip = "off") +
    ggplot2::theme_minimal() +
    ggplot2::scale_x_discrete(
      name = NULL
    ) +
    ggplot2::scale_y_continuous(
      name = NULL,
      breaks = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = bar_colours
    ) +
    ggplot2::scale_colour_manual(
      # If data label is outside the bar make sure it is black text colour
      values = c(text_colours, "outside" = "black")
    ) +
    ggplot2::theme(
      text = ggplot2::element_text(family = font_fam, size = grid::unit(font_size, "pt")),
      # Using ggtext element_markdown seems to be safer for setting variable label face
      axis.text = ggtext::element_markdown(face = y_lab_face, colour = "black"),
      axis.ticks = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank(),
      strip.text.x = ggplot2::element_text(
        hjust = 0, # Align left
        vjust = 0,
        face = "bold",
        lineheight = 0.3,
        margin = ggplot2::margin(t = 0, l = 7, b = 5, r = 0)
      ),
      margins = ggplot2::margin_part(r = rightside_margin)
    ) +
    ggplot2::guides(
      fill = "none",
      colour = "none"
    )

  # Save plot in standard size
  ggplot2::ggsave(
    filename = filename,
    path = path,
    plot = gsbs_plot,
    width = chart_width,
    height = chart_height,
    units = "in",
    device = "png",
    dpi = 300
  )
}


