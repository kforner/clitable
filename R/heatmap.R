# add a background color to a table cell

heat_column <- function(mat, col, ...) {
  mat[, col] <- heatmap_nums(as.numeric(mat[, col]), ...)
  mat
}

heatmap_nums <- function(x, ramp = grDevices::colorRamp(c("green", "red")), ...) 
{
  if (length(x) <= 1 || sum(!is.na(x)) <= 1) {
    return(ansi_string(as.character(x)))
  }

  scaled <- scale_numeric(x, ...)
  rgb_by_row <- ramp(scaled)
  .cell_bg <- function(i) {
    cell <- ansi_string(as.character(x[[i]]))
    if (is.na(rgb_by_row[i, 1])) return(cell)
    cell_bg(cell, t(rgb_by_row[i, , drop = FALSE]))
  }
  ansi_string(sapply(seq_along(x), .cell_bg))
}

cell_bg <- function(cell, color) {
  ansi_string(crayon::make_style(color, bg = TRUE)(cell))
}


#' scales a numeric vector
#' 
#' @param x       the numeric vector to scale
#' @param xmin    the minimum value used for the scaling. all all x < xmin are set to 0
#' @param xmax    the maximum value used for the scaling. all x > xmax set to 1

#' @return a numeric vector of the same length as x, with all values between 0 and 1, except for NAs which are unchanged
#' @export
#' @examples 
#' x <- c(0.1, 100, -2.5, 20, 78.2, NA)
#' scaled <- scale_numeric(x)
#' all(is.na(scaled) | (scaled >= 0 & scaled <= 1))
scale_numeric <- function(x, xmin = min(x, na.rm = TRUE), xmax = max(x, na.rm = TRUE)) {
  if (!length(x)) return(x)
  if (all(is.na(x))) return(x)
  if (xmin == xmax) return(x - xmin + 0.5)
  
  scaled <- (x - xmin) / (xmax - xmin)
  scaled[scaled < 0] <- 0
  scaled[scaled > 1] <- 1

  scaled
}

