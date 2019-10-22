Sys.setenv("R_TESTS" = "")
library(testthat)
library(mocapr)

test_check("mocapr")
