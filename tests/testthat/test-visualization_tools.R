context("test-visualization_tools.R")

test_that("ok theme is returned", {
  theme <- theme_time()
  expect_true(any(class(theme) == "theme"))
})

test_that("geom_timeline empty df input is ok", {
p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
  eq_clean_data() %>% eq_location_clean() %>%
  dplyr::filter(YEAR > 2100) %>%
  ggplot2::ggplot() +
  geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS, size = EQ_MAG_ML))
  expect_true(any(class(p) == "ggplot"))
})

test_that("geom_timeline missing required aesthetic fails", {
  p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
    eq_clean_data() %>% eq_location_clean() %>%
    dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                  COUNTRY %in% c("CHINA", "USA")) %>%
    ggplot2::ggplot() +
    geom_timeline(ggplot2::aes())
  expect_that(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), throws_error())
})

test_that("geom_timeline ok scenario", {
  p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
    eq_clean_data() %>% eq_location_clean() %>%
    dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                  COUNTRY %in% c("CHINA", "USA")) %>%
    ggplot2::ggplot() +
    geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS, size = EQ_MAG_ML))
  expect_true(any(class(p) == "ggplot"))
})

test_that("geom_timeline_label empty df input is ok", {
  p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
    eq_clean_data() %>% eq_location_clean() %>%
    dplyr::filter(YEAR > 2100) %>%
    ggplot2::ggplot() +
    geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS, size = EQ_MAG_ML)) +
    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5))
  expect_true(any(class(p) == "ggplot"))
})

test_that("geom_timeline_label missing required aesthetic fails", {
 p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
   eq_clean_data() %>% eq_location_clean() %>%
   dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                 COUNTRY %in% c("CHINA", "USA")) %>%
   ggplot2::ggplot() +
   geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS, size = EQ_MAG_ML)) +
   geom_timeline_label()
 expect_that(ggplot2::ggplot_gtable(ggplot2::ggplot_build(p)), throws_error())
})

test_that("geom_timeline_label ok scenario", {
 p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim="\t") %>%
   eq_clean_data() %>% eq_location_clean() %>%
   dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
                 COUNTRY %in% c("CHINA", "USA")) %>%
   ggplot2::ggplot() +
   geom_timeline(ggplot2::aes(x = DATE, colour = DEATHS, size = EQ_MAG_ML)) +
   geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5))
 expect_true(any(class(p) == "ggplot"))
})
