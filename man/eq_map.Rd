% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mapping_tools.R
\name{eq_map}
\alias{eq_map}
\title{This function uses leaflet to plot latitude and longitude for earthquake epicenters on a map.
The map is fully interactive and the epicenter markers are clickable which provides a short
text that is grabbed from a column chosen by the caller of the function. The radius of the
epicenter markers is based on the earthquake's magnitude.}
\usage{
eq_map(df, annot_col)
}
\arguments{
\item{df}{The data frame containing the NOAA earthquake data.}

\item{annot_col}{The column containing the text displayed when an epicenter marker is clicked.}
}
\value{
The leaflet htmlwidget containing the interactive map with earthquake visualization
overlayed.
}
\description{
This function uses leaflet to plot latitude and longitude for earthquake epicenters on a map.
The map is fully interactive and the epicenter markers are clickable which provides a short
text that is grabbed from a column chosen by the caller of the function. The radius of the
epicenter markers is based on the earthquake's magnitude.
}
\examples{
readr::read_delim(file = system.file("extdata", "signif.txt", package="noaa"), delim = "\\t") \%>\%
eq_clean_data() \%>\%
dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) \%>\%
eq_map(annot_col = "DATE")
}
