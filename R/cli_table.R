#' generates a text table
#' 
#' @param mat                 the table content to print, can be a data.frame or a matrix
#' @param header              whether to use the row names as  table headers
#' @param header_style        the (crayon) style to use to print the headers (cf [crayon::style()]
#' @param border_style        the style to use for the table borders, one of `r names(BOX_STYLES)`
#' @param heatmap_columns     the columns that should be displayed as heatmaps, as a vector of column indices, names 
#'  or logicals
#' @param heatmap_colorspace  the colorspace to use for the heatmaps, to be passed to [grDevices::colorRamp()]
#' @param hilite_rows         the rows to highlight, as a vector of column indices, names or logicals
#' @param hilite_style        the (crayon) style to use to highlight the rows (cf [crayon::style()]
#' @param NA_style            the (crayon) style to use to highlight the NA values (cf [crayon::style()]
#' @inheritDotParams scale_numeric
#' 
#' @return the lines of the text table as an ansi_string vector
#' @export
#' @examples
#'   df <- head(datasets::penguins, 20)
#'   ct <- cli_table(df, header_style = "bold",
#'     NA_style = "strikethrough",
#'     heatmap_columns = list("flipper_len"), xmin = 180, xmax = 200,
#'     hilite_rows = !is.na(df$sex) & df$sex == "female" & df$bill_dep >= 19, 
#'     hilite_style = "bgGreen"
#'   )
#'   cat(ct, sep = "\n")
cli_table <- function(mat, header = TRUE, header_style = NULL, 
  border_style = "single",  
  heatmap_columns = NULL, heatmap_colorspace = c('green', 'red'), 
  hilite_rows = NULL, hilite_style = 'bgRed',
  NA_style = NULL,
  ...) 
{

  if (length(heatmap_columns)) {
    ramp <- grDevices::colorRamp(heatmap_colorspace)
    for (col in heatmap_columns) {
      mat <- heat_column(mat, col, ramp = ramp, ...)
    }
  }

  mat <- to_character_matrix(mat, NA_style)

  cws <- column_widths(mat, header = header)
  headers <- colnames(mat)

  mat2 <- mat
  for (col in seq_len(ncol(mat2))) {
    mat2[, col] <- extend_strings(mat2[, col], cws[[col]])
    headers[[col]] <- extend_strings(headers[[col]], cws[[col]])
  }
  if (header) colnames(mat2) <- headers
  
  chars <- BOX_STYLES[[border_style]]
  V <- chars$V

  ### table body
  tbl <- sapply(seq_len(nrow(mat)), \(i) cli_row(mat2[i, ], sep = V))

  if (length(hilite_rows)) {
    if (any(is.na(hilite_rows))) stop("NA not supported in hilite_rows")
    tbl[hilite_rows] <- crayon::style(tbl[hilite_rows], hilite_style)
  }

  ### table header
  if (header) {
    tbl <- c(
      crayon::style(cli_row(headers, sep = V), header_style),
      box_line(chars, cws, pos = "MID"), 
      tbl
    )
  }

  ### assemble top + table + bottom
  tbl <- c(box_line(chars, cws), tbl, box_line(chars, cws, pos = "BOTTOM")) 

  ansi_string(tbl)
}
# takes care of NAs
to_character_matrix <- function(df, NA_style = NULL) {
  mat <- as.matrix(df)
  mat[is.na(mat)] <- crayon::style("NA", NA_style)
  mat
}

cli_row <- function(row, sep = BOX_STYLES$single$V) {
  ansi_string(paste0(paste0(sep, row, collapse = ""), sep))
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



# N.B: shamelessly borrowed from cli since not exported
cli_make_space <- function (num, filling = " ") 
{
    num <- pmax(0, num)
    res <- strrep(filling, num)
    Encoding(res) <- Encoding(filling)
    res
}
