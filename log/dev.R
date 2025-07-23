library(devtools)
options(width = Sys.getenv("COLUMNS"))
pkg <- as.package('.')
document(pkg)
load_all(pkg)

check_man(pkg)

test(pkg)


test(pkg, 'zzz')

covr::report(covr::package_coverage())
covr::codecov()

library(usethis)


build_vignettes()
