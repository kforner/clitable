.col_to_rgbstring <- 
test_that("col_to_rgbstring", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  # string
  color <- "red"
  res <- col_to_rgbstring(color)
  expect_identical(col2rgb(res), col2rgb(color))

  # rgb
  color <- rgb(0.2, 0.4, 0)
  res <- col_to_rgbstring(color)
  expect_identical(col2rgb(res), col2rgb(color))

  # rgb string
  color <-"#7F7F00"
  res <- col_to_rgbstring(color)
  expect_identical(col2rgb(res), col2rgb(color))
})



