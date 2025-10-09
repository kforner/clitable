.demo <- 
test_that("demo", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  expect_no_error(demo())
})



