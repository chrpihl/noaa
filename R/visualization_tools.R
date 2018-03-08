#' This method returns a ggplot2 layer that contains the custom geom for plotting earthquake data on
#' a timeline.
#' The color aesthetic indicates the number of deaths as a result of the earthquake and the size
#' aesthetic indicates the
#' earthquake magnitude on the Richter scale.
#' A layer is a combination of data, stat and geom with a potential position adjustment.
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and inherit.aes =
#'                                                                  TRUE (the default),
#'                it is combined with the default mapping at the top level of the plot.
#'                You must supply mapping if there is no plot mapping.
#'                "x" is the date of the earthquake.
#'                "y" is an optional aesthetic indicating a stratification for which a separate
#'                timeline will be plotted for each value of the factor (for instance "country").
#'                "size" is the magnitude of the earthquake measured on the Richter scale.
#'                "color" is the number of deaths caused by the earthquake.
#' @param data The data to be displayed in this layer. There are three options:
#'             If NULL, the default, the data is inherited from the plot data as specified in the
#'             call to ggplot. A data.frame, or other object, will override the plot data.
#'             All objects will be fortified to produce a data frame.
#'             See fortify for which variables will be created.
#'             A function will be called with a single argument, the plot data.
#'             The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position
#' adjustment function.
#' @param na.rm If FALSE missing values are removed with a warning. If TRUE missing values are
#' silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default,
#' includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'                    This is most useful for helper functions that define both data and aesthetics
#'                    and shouldn't inherit behaviour from the default plot specification, e.g.
#'                    borders.
#' @param ... Additional parameters passed to the layer.
#'
#' @return A ggplot2 layer which contains the custom geom for visualizing earthquake data on a
#' timeline.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 ggplot_build
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 layer
#' @importFrom grid grid.draw
#' @importFrom readr read_delim
#'
#' @examples
#' p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                                           delim = "\t") %>%
#'      eq_clean_data() %>% eq_location_clean() %>%
#'      dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
#'                    COUNTRY %in% c("CHINA", "USA")) %>%
#'      ggplot2::ggplot() +
#'      geom_timeline(ggplot2::aes(x = DATE,
#'        colour = DEATHS,
#'        size = EQ_MAG_ML)) +
#'      ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
#' gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
#' gt$layout$clip[gt$layout$name=="panel"] = "off"
#' grid::grid.draw(gt)
geom_timeline <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimeline, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Function used internally to construct grid objects for the layer constructed by "geom_timeline".
#'
#' @return A grid object list ready to be added to a ggplot layer.
#'
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 draw_key_point
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 ggproto
#' @importFrom grid circleGrob
#' @importFrom grid gpar
#' @importFrom grid polylineGrob
#' @importFrom grid gList
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                         required_aes = c("x", "size", "colour"),
                         default_aes = ggplot2::aes(y = 0.3, colour = "grey", size = 1.0,
                                                    alpha = 0.6, shape = 21, fill = "grey",
                                                    stroke = 1.0),
                         draw_key = ggplot2::draw_key_point,
                         draw_group = function(data, panel_scales, coord) {
                           coords <- coord$transform(data, panel_scales)
                           points <- grid::circleGrob(
                             x = coords$x, y = coords$y,
                             #r = (sqrt(coords$size / pi)) / 65, # is size aesthetic based on area?
                             #r = coords$size / 150,
                             r = (2 ^ coords$size) / 1000, # Logarithmic value?
                             gp = grid::gpar(
                               fill = coords$colour,
                               col = coords$colour,
                               alpha = coords$alpha
                           ))

                           y_line_center <- unique(coords$y)

                           lines <- grid::polylineGrob(
                             x = unit(c(0, 1), "npc"),
                             y = unit(c(y_line_center, y_line_center), "npc"),
                             gp = grid::gpar(col = "grey")
                           )

                           return(grid::gList(points, lines))
                         })

#' Custom theme that makes the earthquake visualization prettier and expands the plot margins to
#' allow all text to fit inside the plot.
#'
#' @return A ggplot theme which can be applied to ggplot layers.
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom grid unit
#' @importFrom readr read_delim
#'
#' @examples
#' readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
#'               COUNTRY %in% c("CHINA", "USA")) %>%
#' ggplot2::ggplot(ggplot2::aes(x = DATE, y = COUNTRY, colour = DEATHS, size = EQ_MAG_ML)) +
#' theme_time() +
#' geom_timeline() +
#' ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
theme_time <- function() {
  ggplot2::theme(plot.background = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                 legend.position = "bottom",
                 plot.margin=grid::unit(c(4,4,0,0), "cm"),
                 panel.spacing =grid::unit(c(4,4,0,0), "cm")
  )
}

#' This method returns a ggplot2 layer that contains the custom geom for plotting earthquake data on
#' a timeline with text labels for each earthquake. The color aesthetic indicates the number of
#' deaths as a result of the earthquake and the size aesthetic indicates the earthquake magnitude on
#' the Richter scale. The n_max aesthetic is optional and indicates that only the n_max largest
#' (measured by Richter scale magnitude) earthquakes should be labelled.
#' A layer is a combination of data, stat and geom with a potential position adjustment.
#'
#' @param mapping Set of aesthetic mappings created by aes or aes_. If specified and
#' inherit.aes = TRUE (the default), it is combined with the default mapping at the top level of the
#' plot. You must supply mapping if there is no plot mapping.
#' "x" is the date of the earthquake.
#' "size" is the magnitude of the earthquake measured on the Richter scale.
#' "label" is the column containing the text that should be used as a label.
#' @param data The data to be displayed in this layer. There are three options:
#'             If NULL, the default, the data is inherited from the plot data as specified in the
#'             call to ggplot. A data.frame, or other object, will override the plot data. All
#'             objects will be fortified to produce a data frame. See fortify for which variables
#'             will be created. A function will be called with a single argument, the plot data.
#'             The return value must be a data.frame., and will be used as the layer data.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Position adjustment, either as a string, or the result of a call to a position
#' adjustment function.
#' @param na.rm If FALSE missing values are removed with a warning. If TRUE missing values are
#' silently removed.
#' @param show.legend logical. Should this layer be included in the legends? NA, the default,
#' includes if any aesthetics are mapped. FALSE never includes, and TRUE always includes
#' @param inherit.aes If FALSE, overrides the default aesthetics, rather than combining with them.
#'                    This is most useful for helper functions that define both data and aesthetics
#'                    and shouldn't inherit behaviour from the default plot specification, e.g.
#'                    borders.
#' @param ... Additional parameters passed to the layer.
#'
#' @return A ggplot2 layer which contains the custom geom for visualizing earthquake data on a
#' timeline with labels.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 labs
#' @importFrom ggplot2 ggplot_gtable
#' @importFrom grid grid.draw
#' @importFrom readr read_delim
#'
#' @examples
#' p <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                                           delim = "\t") %>%
#'      eq_clean_data() %>% eq_location_clean() %>%
#'      dplyr::filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML),
#'                    COUNTRY %in% c("CHINA", "USA")) %>%
#'      ggplot2::ggplot(ggplot2::aes(x = DATE,
#'        colour = DEATHS,
#'        size = EQ_MAG_ML)) +
#'      geom_timeline() +
#'      geom_timeline_label(ggplot2::aes(label = LOCATION_NAME, n_max = 5)) +
#'      ggplot2::labs(x = "DATE", color = "# deaths", size = "Richter scale value")
#' gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
#' gt$layout$clip[gt$layout$name=="panel"] = "off"
#' grid::grid.draw(gt)
geom_timeline_label <- function(mapping = NULL, data = NULL, stat = "identity",
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = GeomTimelineLabel, mapping = mapping,
    data = data, stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...)
  )
}

#' Function used internally to construct grid objects for the layer constructed by
#' "geom_timeline_label".
#'
#' @return A grid object list ready to be added to a ggplot layer.
#'
#' @importFrom dplyr %>%
#' @importFrom dplyr top_n
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 Geom
#' @importFrom ggplot2 draw_key_polygon
#' @importFrom grid textGrob
#' @importFrom grid polylineGrob
#' @importFrom grid gpar
#' @importFrom grid gList
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                 required_aes = c("x", "label", "size"),
                                 default_aes = ggplot2::aes(y = 0.3, n_max = 10000, stroke = 1.0,
                                                            size = 1.0, colour = "grey",
                                                            fill = "grey"),
                                 draw_key = ggplot2::draw_key_polygon,
                                 draw_group = function(data, panel_scales, coord) {
                                   data <- data %>% top_n(n = as.integer(data$n_max[1]), size)
                                   coords <- coord$transform(data, panel_scales)
                                   offset <- 0.05
                                   names <- grid::textGrob(
                                     label = coords$label,
                                     x = unit(coords$x, "npc"),
                                     y = unit(coords$y + offset, "npc"),
                                     just = c("left", "bottom"),
                                     gp = grid::gpar(fontsize = 10, col = 'black'),
                                     rot = 45
                                   )

                                   lines <- grid::polylineGrob(
                                     x = unit(c(coords$x, coords$x), "npc"),
                                     y = unit(c(coords$y, coords$y + offset), "npc"),
                                     id = rep(1:length(coords$x), 2),
                                     gp = grid::gpar(
                                       col = "grey"
                                     )
                                   )

                                   return(grid::gList(names, lines))
                                 })
