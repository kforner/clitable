cli_table <- function(mat, header = TRUE, border_style = "single", ...) {
  mat <- as.matrix(mat)

  cws <- column_widths(mat, header = header)

  headers <- colnames(mat)

  mat2 <- mat
  for (col in seq_len(ncol(mat2))) {
    mat2[, col] <- extend_strings(mat2[, col], cws[[col]])
    headers[[col]] <- extend_strings(headers[[col]], cws[[col]])
  }
  if (header) colnames(mat2) <- headers
  

  chars <- cli_box_styles()[border_style, ]
  sep <- chars$vertical

  tbl <- sapply(seq_len(nrow(mat)), \(i) cli_row(mat2[i, ], sep = sep))
  if (header) {
    header_line <- cli_row(headers, sep = sep)
    width <- ansi_nchar(header_line)
    tbl <- c(
      box_line(chars, width),
      header_line,
      box_line(chars, width, top = FALSE), 
      tbl
    )
  }
  tbl <- ansi_string(tbl)

  tbl
}

cli_row <- function(row, sep = cli_box_styles()[["single", "vertical"]]) {
  ansi_string(paste0(paste0(sep, row, collapse = ""), sep))
}

box_line <- function(symbols, width, top = TRUE) {
  left <- symbols$top_left
  right <- symbols$top_right
  if (!top) {
    left <- symbols$bottom_left
    right <- symbols$bottom_right
  }
  ansi_string(paste0(
    left, 
    strrep(symbols$horizontal, width -  ansi_nchar(left) - ansi_nchar(right)),
    right
  ))
}



extend_strings <- function(xs, width) {
  nb_to_fill <- width - ansi_nchar(xs)
  paste0(cli_make_space(as.integer(nb_to_fill > 0)), xs, cli_make_space(nb_to_fill - 1))
}

column_widths <- function(mat, header = TRUE) {
  headers <- NULL
  if (header) headers <- colnames(mat)
  .colwidth <- function(col) {  max(ansi_nchar(c(mat[, col], headers[[col]])))   }
  
  sapply(seq_len(ncol(mat)), .colwidth)
}

add_margin_to_row_cells <- function(row, margin = 1) {
  spacer <- cli_make_space(margin)
  paste0(spacer, row, spacer)
}

add_margin_to_matrix <- function(mat, margin = 1, header = TRUE) {
  mat <- as.matrix(mat)
  for (i in seq_len(nrow(mat))) {
    mat[i, ] <- add_margin_to_row_cells(mat[i, ], margin)
  }
  if (header) {
    colnames(mat) <- add_margin_to_row_cells(colnames(mat), margin)
  }
  mat
}



  # spacer <- paste0(rep(" ", margin), collapse = "")

# N.B: shamelessly borrowed from cli since not exported
cli_box_styles <- function () 
{
    styles <- list(single = list(top_left = "┌", top_right = "┐", 
        bottom_right = "┘", bottom_left = "└", vertical = "│", 
        horizontal = "─"), double = list(top_left = "╔", 
        top_right = "╗", bottom_right = "╝", bottom_left = "╚", 
        vertical = "║", horizontal = "═"), round = list(top_left = "╭", 
        top_right = "╮", bottom_right = "╯", bottom_left = "╰", 
        vertical = "│", horizontal = "─"), `single-double` = list(top_left = "╓", 
        top_right = "╖", bottom_right = "╜", bottom_left = "╙", 
        vertical = "║", horizontal = "─"), `double-single` = list(top_left = "╒", 
        top_right = "╕", bottom_right = "╛", bottom_left = "╘", 
        vertical = "│", horizontal = "═"), classic = list(top_left = "+", 
        top_right = "+", bottom_right = "+", bottom_left = "+", 
        vertical = "|", horizontal = "-"), none = list(top_left = " ", 
        top_right = " ", bottom_right = " ", bottom_left = " ", 
        vertical = " ", horizontal = " "))
    if (!is_utf8_output()) {
        for (n in setdiff(names(styles), c("classic", "none"))) {
            styles[[n]] <- styles[["classic"]]
        }
    }
    do.call(rbind, styles)
}

# N.B: shamelessly borrowed from cli since not exported
cli_make_space <- function (num, filling = " ") 
{
    num <- pmax(0, num)
    res <- strrep(filling, num)
    Encoding(res) <- Encoding(filling)
    res
}
