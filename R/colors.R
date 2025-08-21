col_to_rgbstring <- function(col) {
  rgb(t(grDevices::col2rgb(col)), maxColorValue = 255)
}
