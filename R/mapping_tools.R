#' This function uses leaflet to plot latitude and longitude for earthquake epicenters on a map.
#' The map is fully interactive and the epicenter markers are clickable which provides a short
#' text that is grabbed from a column chosen by the caller of the function. The radius of the
#' epicenter markers is based on the earthquake's magnitude.
#'
#' @param df The data frame containing the NOAA earthquake data.
#' @param annot_col The column containing the text displayed when an epicenter marker is clicked.
#' @return The leaflet htmlwidget containing the interactive map with earthquake visualization
#' overlayed.
#'
#' @export
#' @importFrom dplyr %>%
#' @importFrom dplyr filter
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom leaflet leaflet
#' @importFrom lubridate year
#' @importFrom readr read_delim
#'
#' @examples
#' readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' eq_map(annot_col = "DATE")
eq_map <- function(df, annot_col) {
  # Get the base map
  m <- leaflet() %>%
    leaflet::addTiles() %>%  # Add default OpenStreetMap map tiles
    leaflet::addCircleMarkers(lng=df$LONGITUDE, lat=df$LATITUDE, popup=df[[annot_col]],
                     radius=as.numeric(df$EQ_PRIMARY), fillOpacity = 0.2, weight=1)
  return(m)
}

#' Given a data frame containing NOAA earthquake data, this function returns a popup-text for each
#' observation in the data frame. This popup-text consists of location name, magnitude and
#' total number of deaths. If any of this information is not available, it will not show up in the
#' popup-text.
#'
#' @param df The data frame from which we will generate the popup-texts.
#' @return A list of popup-texts for the earthquake observations given as input.
#'
#' @export
#' @importFrom dplyr "%>%"
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#' @importFrom readr read_delim
#'
#' @examples
#' readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                   delim = "\t") %>%
#' eq_clean_data() %>%
#' dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#' dplyr::mutate(popup_text = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup_text")
eq_create_label <- function(df) {
  df_location_cleaned <- eq_location_clean(df)
  popup_text <- apply(df_location_cleaned, 1,
                      FUN = function(x) paste0(
                        if (!is.na(x[["LOCATION_NAME"]])) paste0("<b>Location:</b> ",
                                                                 x[["LOCATION_NAME"]],
                                                                 "<br>") else "",
                        if (!is.na(x[["EQ_PRIMARY"]])) paste0("<b>Magnitude: </b>",
                                                              x[["EQ_PRIMARY"]], "<br>") else "",
                        if (!is.na(x[["TOTAL_DEATHS"]])) paste0("<b>Total deaths: </b>",
                                                                x[["TOTAL_DEATHS"]]) else "")
  )
}
