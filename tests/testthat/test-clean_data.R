context("test-clean_data.R")

test_that("cleaning empty df fails", {
  df <- data.frame()
  expect_that(eq_clean_data(df), throws_error())
})

test_that("df is missing column", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  df <- df[c("LOCATION_NAME")]
  expect_that(eq_clean_data(df), throws_error())
})

test_that("ok clean scenario", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  clean_df <- eq_clean_data(df)
  expect_true(c("DATE") %in% colnames(clean_df))
  expect_type(clean_df$LATITUDE, "double")
  expect_type(clean_df$LONGITUDE, "double")
  expect_type(clean_df$DEATHS, "double")
})

test_that("location cleaning empty df fails", {
  df <- data.frame()
  expect_that(eq_location_clean(df), throws_error())
})

test_that("df is missing LOCATION_NAME column", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  df <- df[c("YEAR")]
  expect_that(eq_location_clean(df), throws_error())
})

test_that("ok location clean scenario", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  clean_df <- eq_location_clean(df)
  expect_equal(clean_df$LOCATION_NAME, toTitleCase(clean_df$LOCATION_NAME))
})
