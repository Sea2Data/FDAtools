% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coastalCodAreasAFWG-datadoc.R
\docType{data}
\name{coastalCodAreasAFWG}
\alias{coastalCodAreasAFWG}
\title{Coastal cod areas AFWG}
\format{
\code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'polygonName'. See \code{\link[RstoxBase]{StratumPolygon}}.
}
\usage{
data(coastalCodAreasAFWG)
}
\description{
Definition for areas used for catch at age estimation of coastal cod in AFWG.
This is an adaptation of \code{\link[stox2.7preprocessing]{coastalCodAreas}}
}
\details{
Polygons are defined in WGS84 coordinates (unprojected).

The polygons may be saved as a stox 2.7 stratafile with \code{\link[stox2.7preprocessing]{writeSpDataFrameAsWKT}}
Areas may be plotted for inspection with \code{\link[RstoxFDA]{plotArea}} (see example).
}
\examples{
 # plot coastal cod areas AFWG
 data(coastalCodAreasAFWG)
 RstoxFDA::plotArea(areaDef=coastalCodAreasAFWG, title = "Coastal Cod areas")
}
\keyword{datasets}
