## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----read_and_clean, message = FALSE, warning = FALSE--------------------
library(magrittr)
library(noaa)
eq_data <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
                             delim = "\t") %>%
  eq_clean_data() %>% eq_location_clean()

## ----geom_timeline_label, eval = FALSE-----------------------------------
#  p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#                                           delim = "\t") %>%
#    eq_clean_data() %>% eq_location_clean() %>%
#    dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
#                  COUNTRY %in% c("CHINA", "USA")) %>%
#    ggplot2::ggplot(ggplot2::aes(x = DATE,
#                                 y = COUNTRY,
#                                 colour = DEATHS,
#                                 size = EQ_MAG_ML)) +
#    theme_time() +
#    geom_timeline() +
#    geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5)) +
#    ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
#  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
#  gt$layout$clip[gt$layout$name=="panel"] = "off"
#  grid::grid.draw(gt)

## ----eq_map, eval = FALSE------------------------------------------------
#  readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t") %>%
#  eq_clean_data() %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#  eq_map(annot_col = "popup_text")

