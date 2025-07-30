.box_line <- 
test_that("box_line", {
  local_reproducible_output(crayon = TRUE, unicode = TRUE)

  ws <- ansi_nchar(names(iris))

  ### single
  line <- box_line(BOX_STYLES[["single"]], ws)
  expect_s3_class(line, "ansi_string")

  for (style in names(BOX_STYLES)) {
    expect_snapshot(
      cat(
        box_line(BOX_STYLES[[style]], ws, pos = "TOP"), 
        box_line(BOX_STYLES[[style]], ws, pos = "MID"),
        box_line(BOX_STYLES[[style]], ws, pos = "BOTTOM"),
        sep = "\n"
      )
    )
  }
})



