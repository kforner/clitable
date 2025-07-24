
.cli_table <- 
test_that("cli_table", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  df <- head(iris)

  tbl <- cli_table(df)

  expect_snapshot(cat(tbl, sep = "\n"))
})



.box_line <- 
test_that("box_line", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  ### single
  line <- box_line(cli_box_styles()["single", ], 80)

  expect_s3_class(line, "ansi_string")
  expect_equal(ansi_nchar(line), 80)
  
  bline <- box_line(cli_box_styles()["single", ], 80, top = FALSE)
  expect_equal(ansi_nchar(bline), 80)


  ### double
  line <- box_line(cli_box_styles()["double", ], 99)
  expect_equal(ansi_nchar(line), 99)

  ### classic
  line <- box_line(cli_box_styles()["classic", ], 37)
  expect_equal(ansi_nchar(line), 37)

  ### none
  line <- box_line(cli_box_styles()["none", ], 60)
  expect_equal(ansi_nchar(line), 60)
  expect_identical(as.character(line), strrep(" ", 60))
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
  expect_equal(ws2, c(3L, 3L, 3L, 3L, 6L))
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
  expect_snapshot(cat(x, "\n"))

  x2 <- cli_row(add_margin_to_row_cells(row, 2))
  expect_snapshot(cat(x2, "\n"))

})



