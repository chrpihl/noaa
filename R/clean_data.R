#' Prepares a NOAA data frame by adding a DATE column and converting data types of existing columns.
#'
#' @param raw_noaa_df This is the raw data frame read in directly from the NOAA csv file.
#'
#' @return The clean data frame with DATE column and correct data types.
#'
#' @export
#' @importFrom readr read_delim
#'
#' @examples
#' raw_df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                                                delim = "\t")
#' clean_df <- eq_clean_data(raw_df)
eq_clean_data <- function(raw_noaa_df) {
  # Add Date column created by uniting the year, month, day and converting it to the Date class
  # How should we handle NA's in any of the parts?
  raw_noaa_df$DATE <- as.Date(paste0(raw_noaa_df$YEAR, "-", raw_noaa_df$MONTH, "-",
                                     raw_noaa_df$DAY), format = "%Y-%m-%d")
  # Convert LATITUDE and LONGITUDE to numeric class
  raw_noaa_df$LATITUDE <- as.numeric(raw_noaa_df$LATITUDE)
  raw_noaa_df$LONGITUDE <- as.numeric(raw_noaa_df$LONGITUDE)
  # Convert DEATHS to integer
  raw_noaa_df$DEATHS <- as.numeric(raw_noaa_df$DEATHS)
  return(raw_noaa_df)
}

#' Prepares a NOAA data frame by stripping COUNTRY from the LOCATION_NAME and converting
#' LOCATION_NAME to title case.
#'
#' @param raw_noaa_df This is the raw data frame read in directly from the NOAA csv file.
#'
#' @return The clean data frame with title cased LOCATION_NAME without COUNTRY part.

#' @export
#' @importFrom readr read_delim
#' @importFrom tools toTitleCase
#'
#' @examples
#' raw_df <- readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"),
#'                                                delim = "\t")
#' location_cleaned_df <- eq_location_clean(raw_df)
eq_location_clean <- function(raw_noaa_df) {
  # Strip COUNTRY from LOCATION_NAME
  raw_noaa_df$LOCATION_NAME <- sub("^[^:]+:[[:space:]]*", "", raw_noaa_df$LOCATION_NAME)
  # Convert LOCATION_NAME to title case
  raw_noaa_df$LOCATION_NAME <- tools::toTitleCase(tolower(raw_noaa_df$LOCATION_NAME))
  return(raw_noaa_df)
}
