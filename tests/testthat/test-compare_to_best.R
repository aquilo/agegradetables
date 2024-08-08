# https://testthat.r-lib.org/reference/index.html

test_that("get_agt_grade", {
  expect_equal(get_agt_grade("1:38:15", 21.6648, "M", 40), 62.742778)
})

test_that("convert_seconds_to_hms", {
  expect_equal(convert_seconds_to_hms(7299), "02:01:39")
})

test_that("get_records", {
  expect_equal(dim(get_records("F", 65))[2], 4)
})
