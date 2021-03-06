

# context("Basic Tests for vectorParseDate")
 library(testthat)
# library(lubridate)
library(VectorParseDate)
#
#

 lubridate::guess_formats(c("03/03/92"), vector_parse_date_formats())

test_that("best geusss format",{
  dt_v_1 <- c("03/03/92", "03/21/94", "03/02/99", "03/07/02")
  dt_v_5 <- c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA)
  dt_v_6 <- c("2011/21/01", "2011/22/2", "2011/4/1", "Junk", NA)
  dt_v_7 <- c("20112101", "20112202", "20110401", "Junk", NA)
  expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_1), "mdy")
  expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_5), "ymd")
  expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_6), "ydm")
  expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_7), "ydm")
  expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_7, check_func = vector_parse_date_last_120_yr), "ydm")
})

#> Test passed 🌈


#

# # vector_parse_success_grid(dts = dt_v_7)
# # vector_parse_success_grid(dts = dt_v_6)
# # vector_parse_success_grid(dts = dt_v_8)
# # vector_parse_success_grid(dts = dt_v_9, check_func = vector_parse_recent_better)
# # vector_parse_success_grid(dts = dt_v_9)
# # vector_parse_dates_guess_at_format(dts = dt_v_9, check_func = vector_parse_recent_better)
# # vector_parse_dates_guess_at_format(dts = dt_v_9)
#
# test_that("best geusss date", {
#   expect_equal(vector_parse_dates(c("03/03/92", "03/21/94", "03/02/99", "03/07/02")),
#                lubridate::parse_date_time(c("03/03/92", "03/21/94", "03/02/99", "03/07/02"), "%m-%d-%y"))
#   expect_equal(vector_parse_dates(dts = c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA)),
#                suppressWarnings(lubridate::parse_date_time(c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA), "%y-%m-%d")))
# })
# #> Test passed 😸
#
# #
# #
# # vector_parse_dates_guess_at_format(c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
# # vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002"))
# # vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002", '03/25/2002'))
# # vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002"), bool_check = vector_parse_date_usually_true)
