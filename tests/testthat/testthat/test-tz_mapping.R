
context("tz_mapping")

test_that("tz_mappings maps.",{
  
  expect_equal(tz_mapping(area_code = "DK"), "Europe/Copenhagen")
  expect_error(tz_mapping(area_code = "foo"))
  expect_error(tz_mapping(area_code = c("DK", "SE")))
  expect_true(length(tz_mapping(area_code = "SE")) == 1)
})
