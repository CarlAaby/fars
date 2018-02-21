library(fars)
library(testthat)
testthat::expect_that(fars::fars_summarize_years(2015),is_a(class = "tbl_df"))




