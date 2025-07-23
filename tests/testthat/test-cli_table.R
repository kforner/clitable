



.cli_table <- 
test_that("cli_table", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  df <- head(iris)

  tbl <- cli_table(df)

  expect_snapshot(print_text_table_with_base(df))
})

.extend_strings <- 
test_that("extend_strings", {
  xs <- names(iris)

  xs2 <- extend_strings(xs, max(cli::ansi_nchar(xs)))

  expect_equal(unique(cli::ansi_nchar(xs2)), max(cli::ansi_nchar(xs)))

  expect_identical(xs2, c("Sepal.Length", " Sepal.Width", "Petal.Length", " Petal.Width", " Species    "))
})

.column_widths <- 
test_that("column_widths", {
  mat <- as.matrix(head(iris))

  ### header = TRUE
  ws <- column_widths(mat)
  expect_equal(ws, nchar(colnames(mat)))

  expect_equal(column_widths(mat[c(), ]), ws)

  ### header = FALSE
  ws2 <- column_widths(mat, header = FALSE)
  expect_equal(unique(ws2), 3)
})


.add_margin_to_matrix <- 
test_that("add_margin_to_matrix", {
  mat <- as.matrix(head(iris))
  ### header = TRUE
  m0 <- add_margin_to_matrix(mat, 0)
  expect_identical(m0, mat)

  ### margin=1 header=TRUE
  m1 <- add_margin_to_matrix(mat, 1)
  
  expect_equal(column_widths(m1), column_widths(mat) + 2*1)
  # trim everything
  ref <- trimws(m1)
  colnames(ref) <- trimws(colnames(ref))
  expect_identical(ref, mat)

  ### margin=2 header=FALSE
  m2 <- add_margin_to_matrix(mat, 2, header = TRUE)

  expect_equal(column_widths(m2), column_widths(mat) + 2*2)
  expect_identical(m2, add_margin_to_matrix(m1, 1))
})

.cli_row <- 
test_that("cli_row", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  row <- names(iris)

  x <- cli_row(row)

  cat(x, "\n")
  x2 <- cli_row(add_margin_to_row_cells(row, 2))

  df <- head(iris)
  for (i in seq_len(nrow(df))) {
    cat(cli_row(add_margin_to_row_cells(df[i, ], 1)), "\n")
  }

})



# test_that("print_text_table_with_huxtable", {
#   # we enable crayon and color output for this test
#   local_reproducible_output(crayon = TRUE)
#   df <- head(iris)

#   withr::local_options(list(cli.num_colors = 256))

#   expect_snapshot(print_text_table_with_huxtable(df))
#   expect_snapshot(print_text_table_with_huxtable(df, styler = NULL))

#   expect_snapshot(print_text_table_with_huxtable(df, title = "Title"))

#   expect_snapshot(print_text_table_with_huxtable(df, footnote = "Footnote"))

#   expect_snapshot(print_text_table_with_huxtable(df, heatmap_columns = c(1, 3)))

#   expect_snapshot(print_text_table_with_huxtable(df, hilite_rows = c(1, 3)))
  
# })


