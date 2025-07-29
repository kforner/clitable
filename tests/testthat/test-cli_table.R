.box_line <- 
test_that("box_line", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  ### single
  ws <- ansi_nchar(names(iris))
  line <- box_line(BOX_STYLES$single, ws)

  expect_s3_class(line, "ansi_string")
  expect_identical(as.character(line), "┌────────────┬───────────┬────────────┬───────────┬───────┐")
  expect_equal(ansi_nchar(line), 80)

  bline <- box_line(BOX_STYLES$single, ws, top = FALSE)
  expect_identical(as.character(bline), "└────────────┴───────────┴────────────┴───────────┴───────┘")
  
  ### single-double
  style <- "single-double"
  line <- box_line(BOX_STYLES[[style]], ws)
  expect_identical(as.character(line), "╓────────────╥───────────╥────────────╥───────────╥───────╖")
  bline <- box_line(BOX_STYLES[[style]], ws, top = FALSE)
  expect_identical(as.character(bline), "╙────────────╨───────────╨────────────╨───────────╨───────╜")

  ### double-single
  style <- "double-single"
  line <- box_line(BOX_STYLES[[style]], ws)
  expect_identical(as.character(line), "╒════════════╤═══════════╤════════════╤═══════════╤═══════╕")
  bline <- box_line(BOX_STYLES[[style]], ws, top = FALSE)
  expect_identical(as.character(bline), "╘════════════╧═══════════╧════════════╧═══════════╧═══════╛")

  ### double
  line <- box_line(BOX_STYLES$double, ws)
  expect_identical(as.character(line), "╔════════════╦═══════════╦════════════╦═══════════╦═══════╗")
  bline <- box_line(BOX_STYLES$double, ws, top = FALSE)
  expect_identical(as.character(bline), "╚════════════╩═══════════╩════════════╩═══════════╩═══════╝")


  ### classic
  line <- box_line(BOX_STYLES$classic, ws)
  expect_identical(as.character(line), "+------------+-----------+------------+-----------+-------+")
  bline <- box_line(BOX_STYLES$classic, ws, top = FALSE)
  expect_identical(as.character(bline), "+------------+-----------+------------+-----------+-------+")

})




.cli_table <- 
test_that("cli_table", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  df <- head(iris)

  tbl <- cli_table(df)
browser()
  expect_snapshot(cat(tbl, sep = "\n"))
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



