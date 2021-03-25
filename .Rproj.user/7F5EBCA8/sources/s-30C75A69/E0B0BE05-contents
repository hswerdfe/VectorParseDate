

context("Basic Tests for vectorParseDate")
library(testthat)
library(lubridate)
library(vectorParseDate)


dt_v_1 <- c("03/03/92", "03/21/94", "03/02/99", "03/07/02")
dt_v_2 <- c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002")
dt_v_3 <- c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002", '03/25/2002')
dt_v_8 <- c('2011/01/21', '2011/01/21', '2011/01/02', '2011/01/22')
dt_v_4 <- c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002")
dt_v_5 <- c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA)
dt_v_6 <- c("2011/21/01", "2011/22/2", "2011/4/1", "Junk", NA)
dt_v_7 <- c("20112101", "2011222", "201141", "Junk", NA)
dt_v_8 <- c('2011/01/21', '2011/01/21', '2011/01/02', '2011/01/22')
dt_v_9 <- c('2021/03/02', '2021/03/01', '2020-11-11')

test_that("best geusss format", {
    expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_1), "mdy")
    expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_5), "ymd")
    expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_6), "ydm")
    expect_equal(vector_parse_dates_guess_at_format(dts = dt_v_7), "ydm")

  })
#> Test passed 🌈

# vector_parse_success_grid(dts = dt_v_7)
# vector_parse_success_grid(dts = dt_v_6)
# vector_parse_success_grid(dts = dt_v_8)
# vector_parse_success_grid(dts = dt_v_9, check_func = vector_parse_recent_better)
# vector_parse_success_grid(dts = dt_v_9)
# vector_parse_dates_guess_at_format(dts = dt_v_9, check_func = vector_parse_recent_better)
# vector_parse_dates_guess_at_format(dts = dt_v_9)

test_that("best geusss date", {
  expect_equal(vector_parse_dates(c("03/03/92", "03/21/94", "03/02/99", "03/07/02")),
               lubridate::parse_date_time(c("03/03/92", "03/21/94", "03/02/99", "03/07/02"), "%m-%d-%y"))
  expect_equal(vector_parse_dates(dts = c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA)),
               suppressWarnings(lubridate::parse_date_time(c("2011/01/21", "2011/02/22", "2011/01/04", "Junk", NA), "%y-%m-%d")))
})
#> Test passed 😸

#
#
# vector_parse_dates_guess_at_format(c("03/03/92", "03/21/94", "03/02/99", "03/07/02"))
# vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002"))
# vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002", '03/25/2002'))
# vector_parse_dates_guess_at_format(c("03/03/2092", "03/21/2094", "03/02/2099", "03/07/2002"), bool_check = vector_parse_date_usually_true)
