library(fars)
library(testthat)
testthat::expect_that(fars::fars_read_years(2013),is_a(class = "list"))


