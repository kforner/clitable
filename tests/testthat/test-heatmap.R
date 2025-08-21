
.heat_column <- 
test_that("heat_column", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  mat <- head(mtcars, 10)

  mat <- heat_column(mat, "mpg")
  mat <- heat_column(mat, "carb", ramp = grDevices::colorRamp(c("blue", "red")))
  mat <- heat_column(mat, "hp", xmin = 100, xmax = 110)

  expect_snapshot(cat(cli_table(mat), sep = ""))
})


.heatmap_nums <- 
test_that("heatmap_nums", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  x <- c(0.1, 100, -2.5, 20, 78.2, NA)
  heated <- heatmap_nums(x)

  expect_s3_class(heated, "ansi_string")
  expect_length(heated, length(x))
  # check min
  expect_identical(heated[3], as.character(cell_bg(x[3], "green")))
  # check max
  expect_identical(heated[2], as.character(cell_bg(x[2], "red")))
  expect_snapshot(heated)
})



.scale_numeric <- 
test_that("scale_numeric_to_integer", {
  # standard
  x <- c(0.1, 100, -2.5, 20, 78.2, NA)
  scaled <- scale_numeric(x)
  expect_equal(scaled, (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))

  # empty
  expect_equal(scale_numeric(numeric()), numeric())

  # singletons
  expect_equal(scale_numeric(0), 0.5)
  expect_equal(scale_numeric(1), 0.5)

  # NAs
  expect_equal(scale_numeric(NA_real_), NA_real_)
  expect_equal(scale_numeric(c(10, NA, 1)), c(1, NA, 0))
  expect_equal(scale_numeric(c(NA, -0.3)), c(NA, 0.5))

  # custom xmin, xmax
  scaled <- scale_numeric(x, xmin = 1, xmax = 80)
  
  expect_equal(scaled[1], 0)
  expect_equal(scaled[2], 1)
  expect_equal(scaled[3], 0)

  ref <- scaled * (80 - 1) + 1
  expect_equal(x[-(1:3)], ref[-(1:3)])
})


.cell_bg <- 
test_that("cell_bg", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)


  x <- cell_bg("red", "red")

  expect_s3_class(x, "ansi_string")
  expect_snapshot(cat(x))

  # rgb vector
  expect_identical(cell_bg("red", col2rgb("red")), x)

  # rgb spec
  expect_identical(cell_bg("red", col_to_rgbstring("red")), x)

  ### heatmap
  colors <- grDevices::colorRampPalette(c("green", "red"))(8)
  x <- sapply(seq_along(colors), \(i) cell_bg(i, colors[[i]]))
  
  expect_length(x, length(colors))
  expect_snapshot(cat(x, sep = ""))
})



