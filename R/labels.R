
#' Label ORR Percent
#'
#' Turn number into an string in ORR percent format.
#' @returns A labeller function. Values are shown with a '%' symbol to zero
#'   decimal places unless between 1% and -1% and then shown to 1 decimal place.
#' @seealso [label_orr_comma()], [label_orr_percentage_point()]
#' @export
label_orr_percent <- function() {
  function(x) {
    if (length(x) == 0) {
      return(character())
    }
    base::ifelse(
      x < 0.95 & x > -0.95,
      scales::number(x, accuracy = 0.1, suffix = "%"),
      scales::number(x, accuracy = 1, suffix = "%")
    )
  }
}


#' Label ORR Comma
#'
#' Turn large numbers into string in ORR format.
#' @param decimal_places An integer indicating the number of decimal places to be used.
#' @returns A character vector. Numbers are shown with comma every 3 digits and
#'   rounded to 3 significant figures.
#' @seealso [label_orr_percent()], [label_orr_percentage_point()]
#' @export
label_orr_comma <- function(decimal_places = 1) {
  scales::label_comma(accuracy = 1 / (10 ^ decimal_places), scale = 1)
}

#' Label ORR Percentage Point Change
#'
#' Turn number into string in ORR percentage point change format.
#' @param decimal_places An integer indicating the number of decimal places to be used.
#' @returns A character vector.Numbers are shown to 1 decimal place with a 'pp'
#'   suffix and +/- prefix.
#' @seealso [label_orr_percent()], [label_orr_comma()]
#' @export
label_orr_percentage_point <- function(decimal_places = 1) {
  scales::label_number(
    accuracy = 1 / (10 ^ decimal_places),
    scale = 1,
    suffix = "pp",
    style_positive = "plus"
  )
}
