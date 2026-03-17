

year_label_from_quarter_key <- function(key) {
  paste(
    substr(as.character(key), 1, 4),
    substr(as.character(key), 7, 8),
    sep = "-")
}


load_arial_font <- function() {
  sysfonts::font_add(
    "Arial",
    regular = system.file("arial.ttf", package = "orrcharts"),
    bold = system.file("arialbd.ttf", package = "orrcharts"),
  )
}



