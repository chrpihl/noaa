#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#' @importFrom ggplot2 layer
#'
#' @examples
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

#' Title
#'
#' @param data
#' @param panel_scales
#' @param coord
#'
#' @return
#' @export
#' @importFrom ggplot2 ggproto, Geom, aes, draw_key_point
#' @importFrom grid circleGrob, gpar, polylineGrob, gList
#'
#' @examples
GeomTimeline <- ggplot2::ggproto("GeomTimeline", ggplot2::Geom,
                         required_aes = c("x", "size", "colour"),
                         default_aes = ggplot2::aes(y = 0.3, colour = "grey", size = 1.0, alpha = 0.6,
                                                   shape = 21, fill = "grey", stroke = 1.0),
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

#' Title
#'
#' @return
#' @export
#' @importFrom ggplot2 theme, element_blank, element_line
#'
#' @examples
theme_time <- function() {
  ggplot2::theme(plot.background = ggplot2::element_blank(),
                 panel.background = ggplot2::element_blank(),
                 axis.line.x = ggplot2::element_line(colour = 'black', size=0.5, linetype='solid'),
                 legend.position = "bottom",
                 plot.margin=unit(c(4,4,0,0), "cm"),
                 panel.spacing =unit(c(4,4,0,0), "cm")
  )
}

#' Title
#'
#' @param mapping
#' @param data
#' @param stat
#' @param position
#' @param na.rm
#' @param show.legend
#' @param inherit.aes
#' @param ...
#'
#' @return
#' @export
#' @importFrom ggplot2 layer
#'
#' @examples
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

#' Title
#'
#' @param data
#' @param panel_scales
#' @param coord
#'
#' @return
#' @export
#' @importFrom dplyr top_n
#' @importFrom ggplot2 ggproto, Geom, aes, draw_key_polygon
#' @importFrom grid textGrob, polylineGrob, gList
#' @importFrom magrittr %>%
#'
#' @examples
GeomTimelineLabel <- ggplot2::ggproto("GeomTimelineLabel", ggplot2::Geom,
                                 required_aes = c("x", "label", "size"),
                                 default_aes = ggplot2::aes(y = 0.3, n_max = 10000, stroke = 1.0, size = 1.0,
                                                            colour = "grey", fill = "grey"),
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
                                     gp = gpar(fontsize = 10, col = 'black'),
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

##clean_df %>% filter(YEAR >= 2000, !is.na(DEATHS), !is.na(EQ_MAG_ML), COUNTRY %in% c("CHINA", "USA")) %>%
#p <- clean_df %>% filter(YEAR >= 1900, !is.na(DEATHS), !is.na(EQ_MAG_ML), COUNTRY %in% c("CHINA", "USA")) %>%
#  ggplot(aes(x = DATE, y = COUNTRY, colour = DEATHS, size = EQ_MAG_ML)) + theme_time() +
#  geom_timeline() +
#  geom_timeline_label(aes(label = LOCATION_NAME, n_max = 5)) +
#  labs(x = "DATE", color = "# deaths", size = "Richter scale value")
#gt <- ggplot_gtable(ggplot_build(p))
#gt$layout$clip[gt$layout$name=="panel"] = "off"
#grid.draw(gt)
## Clear with "dev.off()"
##dev.off()
## clean_df %>% filter(YEAR >= 2010, !is.na(DEATHS), !is.na(EQ_MAG_ML)) %>% select(DATE, DEATHS, EQ_MAG_ML)
