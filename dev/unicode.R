

start <- 0x2500
end <- 0x257F

# Number of columns in the table
cols <- 8

# Generate the table
for (i in seq(start, end, by = cols)) {
  row <- sapply(i:(min(i + cols - 1, end)), function(code) {
    sprintf("U+%04X: %s%s", code, intToUtf8(code), intToUtf8(code))
  })
  cat(paste(row, collapse = "  "), "\n")
}


cat(strrep("\U2500", 10), "\n")



with(BOX_STYLES$single, {
  cat(TL, H, H, TM, H, H, TR, "\n", sep = "")
  cat(V, "A ", V,  "B ", V, "\n", sep = "")
  cat(BL, H, H, BM, H, H, BR, "\n", sep = "")
})

utf8ToInt("┏") |> int2hex()

int2hex <- function(x) sprintf("U+%04X", x)
