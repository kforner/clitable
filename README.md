clitable
=========

  <!-- badges: start -->
  [![R-CMD-check](https://github.com/kforner/clitable/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/kforner/clitable/actions/workflows/R-CMD-check.yaml)
  [![Codecov test coverage](https://codecov.io/gh/kforner/clitable/branch/main/graph/badge.svg)](https://app.codecov.io/gh/kforner/clitable?branch=main)
  [![CRAN status](https://www.r-pkg.org/badges/version/clitable)](https://CRAN.R-project.org/package=clitable)
  [![License: AGPL-3.0-or-later](https://img.shields.io/badge/license-AGPL--3.0--or--later-success)](https://www.gnu.org/licenses/agpl-3.0.html)
  <!-- badges: end -->

The aim of `clitable` is to print tables in the terminal, using ANSI strings thanks to the 'cli' and 'crayon' packages
to use formatting and colours. 

![screenshot](man/figures/clitable.png)

## Installation


Install it from github: 
```
### using devtools
# install devtools from CRAN if needed
install.packages('devtools') 

install_github('kforner/clitable')

### or using pak
# install pak from CRAN if needed
install.packages('pak') 
pak::pak("kforner/clitable")
```

