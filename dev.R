library(devtools)

check_man()
document()

test()
test(filter = "cli_table")
test(filter = "box")
test(filter = "colors")
test(filter = "heatmap")
check()
