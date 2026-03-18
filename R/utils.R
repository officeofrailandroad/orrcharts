

year_label_from_short_quarter_key <- function(key) {
  plus_year <- key + 10
  paste(
    substr(as.character(key), 1, 4),
    substr(as.character(plus_year), 3, 4),
    sep = "-"
  )
}

year_label_from_long_quarter_key <- function(key) {
  paste(
    substr(as.character(key), 1, 4),
    substr(as.character(key), 7, 8),
    sep = "-")
}

year_label_from_quarter_key <- function(key) {
  stopifnot(any(nchar(as.character(key)) %in% c(5, 9)))
  ifelse(
    nchar(key) == 5,
    year_label_from_short_quarter_key(key),
    year_label_from_long_quarter_key(key)
  )
}


load_arial_font <- function() {
  sysfonts::font_add(
    "Arial",
    regular = system.file("arial.ttf", package = "orrcharts"),
    bold = system.file("arialbd.ttf", package = "orrcharts"),
  )
}



