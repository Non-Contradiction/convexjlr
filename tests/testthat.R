Sys.setenv("R_TESTS" = "")
library(testthat)
library(convexjlr)

test_check("convexjlr")
