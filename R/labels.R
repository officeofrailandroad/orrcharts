
#' Label ORR Percent
#'
#' Turn number into an string in ORR percent format.
#' @param x A numeric vector.
#' @returns A character vector. Values are shown with a '%' symbol to zero
#'   decimal places unless between 1% and -1% and then shown to 1 decimal place.
#' @seealso [label_orr_comma()], [label_orr_percentage_point()]
label_orr_percent <- function(x) {
  perc_0dp <- scales::label_percent(scale = 1, accuracy = 1)
  perc_1dp <- scales::label_percent(scale = 1, accuracy = 0.1)
  base::ifelse(
    x < 1 & x > -1,
    perc_1dp(x),
    perc_0dp(x)
  )
}


#' Label ORR Comma
#'
#' Turn large numbers into string in ORR format.
#' @param x A numeric vector.
#' @returns A character vector. Numbers are shown with comma every 3 digits and
#'   rounded to 3 significant figures.
#' @seealso [label_orr_percent()], [label_orr_percentage_point()]
label_orr_comma <- function(x) {
  parse_fn <- scales::label_comma(accuracy = 1, scale = 1)
  parse_fn(base::signif(x, 3))
}

#' Label ORR Percentage Point Change
#'
#' Turn number into string in ORR percentage point change format.
#' @param x A numeric vector.
#' @returns A character vector.Numbers are shown to 1 decimal place with a 'pp'
#'   suffix and +/- prefix.
#' @seealso [label_orr_percent()], [label_orr_comma()]
label_orr_percentage_point <- function(x) {
  pp_fn <- scales::label_number(
    accuracy = 0.1,
    scale = 1,
    suffix = "pp",
    style_positive = "plus"
  )
  pp_fn(x)
}
