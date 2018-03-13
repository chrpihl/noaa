context("test-mapping_tools.R")

test_that("mapping empty df fails", {
  df <- data.frame()
  expect_that(eq_map(df, "test"), throws_error())
})

test_that("df is missing annotation column", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  expect_that(eq_map(df, "i_do_not_exist"), throws_error())
})

test_that("ok mapping scenario", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  map <- df %>% eq_clean_data() %>% eq_location_clean() %>% eq_map("DATE")
  expect_true(any(class(m) == "htmlwidget"))
})

test_that("df is missing required column for label", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  df <- df[c("LOCATION_NAME")]
  expect_that(eq_create_label(df), throws_error())
})

test_that("ok create label scenario", {
  df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t")
  popup_text <- df %>% eq_create_label()
  expect_type(popup_text, "character")
  print(nrow(df$LOCATION_NAME))
  print(class(popup_text))
  print(popup_text)
  print(length(popup_text))
  expect_equal(length(popup_text), nrow(df$LOCATION_NAME))
})
