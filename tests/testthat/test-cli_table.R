

.cli_table <- 
test_that("cli_table", {
  df <- head(datasets::iris)

  # boolean with NA
  bools <-  seq_len(nrow(df)) %% 2 == 0
  bools[3] <- NA
  expect_error(cli_table(df, hilite_rows = bools), "NA not supported in hilite_rows")


  local_reproducible_output(crayon = TRUE, unicode = TRUE)


  for (style in names(BOX_STYLES)) {
    expect_snapshot(cat(cli_table(df, border_style = style), sep = "\n"))
    expect_snapshot(cat(cli_table(df, border_style = style, header = FALSE), sep = "\n"))
  }

  ### heatmap_columns
  df <- head(datasets::mtcars, 10)

  ct <- cli_table(df, heatmap_columns = list(1, "hp", "carb"))
  expect_snapshot(cat(ct, sep = "\n"))

  ## with custom colors
  ct <- cli_table(df, heatmap_columns = list(1, 4, "carb"), heatmap_colorspace = c("blue", "yellow"))
  expect_snapshot(cat(ct, sep = "\n"))

  ## with custom range
  ct <- cli_table(df, heatmap_columns = 1, xmin = 18, xmax = 22)
  expect_snapshot(cat(ct, sep = "\n"))

  ### hilite rows
  # one by index
  ct <- cli_table(df, hilite_rows = 2)
  expect_snapshot(cat(ct, sep = "\n"))

  # multiple
  ct <- cli_table(df, hilite_rows = c(3, 1, 7))
  expect_snapshot(cat(ct, sep = "\n"))

  # using boolean
  ct <- cli_table(df, hilite_rows = (seq_len(nrow(df)) %% 2 == 0))
  expect_snapshot(cat(ct, sep = "\n"))



  # header
  ct <- cli_table(df, header = FALSE)
  expect_snapshot(cat(ct, sep = "\n"))

  # header_style
  ct <- cli_table(df)
  expect_false(crayon::has_style(ct[2]))

  ct <- cli_table(df, header_style = "bold")
  expect_true(crayon::has_style(ct[2]))
  expect_snapshot(cat(ct, sep = "\n"))

  ### matrix/df with NAs
  # default
  df <- head(datasets::penguins)
  ct <- cli_table(df)
  expect_identical(ct[1], "┌───────┬─────────┬────────┬────────┬───────────┬─────────┬──────┬────┐")
  expect_match(ct[7], "NA", fixed = TRUE)
  expect_snapshot(cat(ct, sep = "\n"))

  # custom
  ct <- cli_table(df, NA_style = "strikethrough")
  expect_match(ct[7], crayon::style("NA", "strikethrough"), fixed = TRUE)
  expect_snapshot(cat(ct, sep = "\n"))
})



.cli_table_and_cli <- 
test_that("cli_table_and_crayon", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  df <- head(datasets::mtcars)

  df[1, 1] <- with(getNamespace("crayon"), yellow$bgMagenta$bold(3))
  df[2, 2] <- with(getNamespace("crayon"), green(
    'I am a green line ' %+%
    blue$underline$bold('with a blue substring') %+%
    ' that becomes green again!'
  ))
  ct <- cli_table(df)

  expect_snapshot(cat(ct, sep = "\n"))
})


.to_character_matrix <- 
test_that("to_character_matrix", {

  ### df without NA: same as as.matrix
  df <- head(datasets::iris)
  expect_identical(to_character_matrix(df), as.matrix(df))
  
  ### df with NA 
  # no style
  df <- head(datasets::penguins)
  mat <- to_character_matrix(df)

  expect_false(identical(mat, as.matrix(df)))
  expect_identical(unique(mat[is.na(df)]), "NA")
  expect_identical(mat[!is.na(df)], as.matrix(df)[!is.na(df)])

  # with style
  local_reproducible_output(crayon = TRUE, unicode = TRUE)
  df <- head(datasets::penguins)
  mat <- to_character_matrix(df, "bold")
  
  expect_identical(mat[!is.na(df)], as.matrix(df)[!is.na(df)])
  expect_identical(unique(mat[is.na(df)]), crayon::style("NA", "bold"))
})


.extend_strings <- 
test_that("extend_strings", {
  xs <- names(datasets::iris)

  xs2 <- extend_strings(xs, max(cli::ansi_nchar(xs)))

  expect_equal(unique(cli::ansi_nchar(xs2)), max(cli::ansi_nchar(xs)))

  expect_identical(xs2, c("Sepal.Length", " Sepal.Width", "Petal.Length", " Petal.Width", " Species    "))
})

.column_widths <- 
test_that("column_widths", {
  mat <- as.matrix(head(datasets::iris))

  ### header = TRUE
  ws <- column_widths(mat)
  expect_equal(ws, nchar(colnames(mat)))

  expect_equal(column_widths(mat[c(), ]), ws)

  ### header = FALSE
  ws2 <- column_widths(mat, header = FALSE)
  expect_equal(ws2, c(3L, 3L, 3L, 3L, 6L))

  ### regression: NAs due to NAs in the matrix
  mat <- as.matrix(head(datasets::penguins))
  ws <- column_widths(mat)
  expect_equal(sum(is.na(ws)), 5)

  # should use to_character_matrix() instead
  mat <- to_character_matrix(head(datasets::penguins))
  ws <- column_widths(mat)
  expect_equal(sum(is.na(ws)), 0)
})


.add_margin_to_matrix <- 
test_that("add_margin_to_matrix", {
  mat <- as.matrix(head(datasets::iris))
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
  row <- names(datasets::iris)

  x <- cli_row(row)
  expect_snapshot(cat(x, "\n"))

  x2 <- cli_row(add_margin_to_row_cells(row, 2))
  expect_snapshot(cat(x2, "\n"))

})



