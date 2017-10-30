Sys.setenv("R_TESTS" = "")
JuliaCall::julia_setup()
library(testthat)
library(convexjlr)

test_check("convexjlr")
