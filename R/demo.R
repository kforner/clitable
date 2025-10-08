

demo <- function() {
  df <- head(iris)
  ####################
  cli::cli_h1("clitable border styles")

  for (style in names(BOX_STYLES)) {
    cli::cli_h2(paste("style:", style, "\n"))
    cat(cli_table(df, border_style = style), sep = "\n")
    cat(cli_table(df, border_style = style, header = FALSE), sep = "\n")
  }

  ####################
  cli::cli_h1("heatmap columns")

  df <- head(mtcars, 10)
  cli::cli_h2("default heatmap settings")
  ct <- cli_table(df, heatmap_columns = list(1, "hp", "carb"))
  cat(ct, sep = "\n")

  ## with custom colors
  cli::cli_h2("heatmap custom colors")
  ct <- cli_table(df, heatmap_columns = list(1, 4, "carb"), heatmap_colorspace = c("blue", "yellow"))
  cat(ct, sep = "\n")

  ## with custom range
  ct <- cli_table(df, heatmap_columns = 1, xmin = 18, xmax = 22)
  cli::cli_h2("heatmap custom range")
  cat(ct, sep = "\n")

  ####################
  cli::cli_h1("hilite rows")
  df <- head(mtcars, 10)
  ct <- cli_table(df, hilite_rows = c(3, 1, 7))
  cat(ct, sep = "\n")

  ########################
  cli::cli_h1("combined")
  df <- head(mtcars, 10)
  ct <- cli_table(df, heatmap_columns = list(1, 4, "carb"), hilite_rows = df$hp > 150, hilite_style = "yellow")
  cat(ct, sep = "\n")


  ####################
  cli::cli_h1("not all numeric")
  df <- head(penguins)
  df$sex <- with(getNamespace("crayon"), ifelse(df$sex == "female", blue$underline$bold(df$sex), df$sex) )

  ct <- cli_table(df)
  cat(ct, sep = "\n")

  ###############################
  cli::cli_h1("all bells and whistles")
  df <- head(penguins)
  df$species <- as.character(df$species)
  df[1, 1] <- crayon::style("ADELIE", "underline","bgYellow")
  ct <- cli_table(df, header_style = "bold",
    NA_style = "strikethrough",
    heatmap_columns = list("body_mass"), 
    hilite_rows = !is.na(df$sex) & df$sex == "female" , 
    hilite_style = "bgGreen"
  )
  cat(ct, sep = "\n")
}