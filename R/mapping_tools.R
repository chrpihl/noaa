#' Title
#'
#' @param df
#' @param annot_col
#'
#' @return
#' @export
#' @import leaflet
#' @importFrom magrittr %>%
#'
#' @examples
eq_map <- function(df, annot_col) {
  # Get the base map
  m <- leaflet() %>%
    addTiles() %>%  # Add default OpenStreetMap map tiles
    addCircleMarkers(lng=df$LONGITUDE, lat=df$LATITUDE, popup=df[[annot_col]],
                     radius=as.numeric(df$EQ_PRIMARY), fillOpacity = 0.2, weight=1)
  return(m)
}

eq_create_label <- function(df) {
  df_location_cleaned <- eq_location_clean(df)
  popup_text <- apply(df_location_cleaned, 1,
                      FUN = function(x) paste0(
                        if (!is.na(x[["LOCATION_NAME"]])) paste0("<b>Location:</b> ", x[["LOCATION_NAME"]], "<br>") else "",
                        if (!is.na(x[["EQ_PRIMARY"]])) paste0("<b>Magnitude: </b>", x[["EQ_PRIMARY"]], "<br>") else "",
                        if (!is.na(x[["TOTAL_DEATHS"]])) paste0("<b>Total deaths: </b>", x[["TOTAL_DEATHS"]]) else "")
  )
}

## Example 1
#readr::read_delim("../signif.txt", delim = "\t") %>%
#  eq_clean_data() %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#  eq_map(annot_col = "DATE")

# Example 2
#readr::read_delim("../signif.txt", delim = "\t") %>%
#  eq_clean_data() %>%
#  dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#  dplyr::mutate(popup_text = eq_create_label(.)) %>%
#  eq_map(annot_col = "popup_text")
