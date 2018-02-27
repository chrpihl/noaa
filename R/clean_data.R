#' Title
#'
#' @param raw_noaa_df
#'
#' @return
#' @export
#'
#' @examples
eq_clean_data <- function(raw_noaa_df) {
  # Add Date column created by uniting the year, month, day and converting it to the Date class
  # How should we handle NA's in any of the parts?
  raw_noaa_df$DATE <- as.Date(paste0(raw_noaa_df$YEAR, "-", raw_noaa_df$MONTH, "-", raw_noaa_df$DAY),
                              format = "%Y-%m-%d")
  # Convert LATITUDE and LONGITUDE to numeric class
  raw_noaa_df$LATITUDE <- as.numeric(raw_noaa_df$LATITUDE)
  raw_noaa_df$LONGITUDE <- as.numeric(raw_noaa_df$LONGITUDE)
  # Convert DEATHS to integer
  raw_noaa_df$DEATHS <- as.numeric(raw_noaa_df$DEATHS)
  return(raw_noaa_df)
}

#' Title
#'
#' @param raw_noaa_df
#'
#' @return
#' @export
#' @importFrom tools toTitleCase
#'
#' @examples
eq_location_clean <- function(raw_noaa_df) {
  # Strip COUNTRY from LOCATION_NAME
  raw_noaa_df$LOCATION_NAME <- sub("^[^:]+:[[:space:]]*", "", raw_noaa_df$LOCATION_NAME)
  # Convert LOCATION_NAME to title case
  raw_noaa_df$LOCATION_NAME <- tools::toTitleCase(tolower(raw_noaa_df$LOCATION_NAME))
  return(raw_noaa_df)
}

#raw_df <- readr::read_delim(file = "../signif.txt", delim = "\t")
#clean_df <- eq_clean_data(raw_df)
#location_cleaned_df <- eq_location_clean(raw_df)
