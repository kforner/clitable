BOX_STYLES <- list(
  single = list(
    TOP = list(L = "\U250C", M = "\U252C", R = "\U2510"),
    MID = list(L = "\U251C", M = "\U253C", R = "\U2524"),
    BOTTOM = list(L = "\U2514", M = "\U2534", R = "\U2518"),
    H = "\U2500",
    V = "\U2502"
  ), 
  double = list(
    TOP = list(L = "\U2554", M = "\U2566", R = "\U2557"),
    MID = list(L = "\U2560", M = "\U256C", R = "\U2563"),
    BOTTOM = list(L = "\U255A", M = "\U2569", R = "\U255D"),
    H = "\U2550",
    V = "\U2551"
  ),
  `single-double` = list(
    TOP = list(L = "\U2553", M = "\U2565", R = "\U2556"),
    MID = list(L = "\U255F", M = "\U256B", R = "\U2562"),
    BOTTOM = list(L = "\U2559", M = "\U2568", R = "\U255C"),
    H = "\U2500",
    V = "\U2551"
  ),
  `double-single` = list(
    TOP = list(L = "\U2552", M = "\U2564", R = "\U2555"),
    MID = list(L = "\U255E", M = "\U256A", R = "\U2561"),
    BOTTOM = list(L = "\U2558", M = "\U2567", R = "\U255B"),
    H = "\U2550",
    V = "\U2502"
  ),

  classic = list(
    TOP = list(L = "+", M = "+", R = "+"),
    MID = list(L = "+", M = "+", R = "+"),
    BOTTOM = list(L = "+", M = "+", R = "+"),
    H = "-",
    V = "|"
  )
)

box_line <- function(symbols, widths, pos = c("TOP", "MID", "BOTTOM")) {
  pos <- match.arg(pos)
  if (!length(widths)) return("")

  DECO <- symbols[[pos]]
  .cell <- function(i) {
    hr <- strrep(symbols$H, widths[i])
    if (i > 1) c(DECO$M, hr) else hr
  }

  cells <- unlist(lapply(seq_along(widths), .cell), use.names = FALSE)
  ansi_string(paste0(DECO$L, paste0(cells, collapse = ""), DECO$R))
}
