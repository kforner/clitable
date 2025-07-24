library(devtools)

check_man()
document()

test()
test(filter = "cli_table")
check()
