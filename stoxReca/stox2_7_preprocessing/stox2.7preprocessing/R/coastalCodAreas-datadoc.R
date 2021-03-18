#' Coastal cod areas
#'
#' Definition for areas used for catch at age estimation of coastal cod.
#' The areas overlap with area 00, 03, 04, 05, 06, and 07 defined by the Norwegian Directorate of Fisheries
#' (see: RstoxFDA::mainareaFdir2018), and are constructed from these by dividing them along the Norwegian
#' territorial line (12 nm of coast).
#'
#' Polygons are defined in WGS84 coordinates (unprojected).
#'
#' Area positions for use with applyPosToData in Stox 2.7 can be saved with \code{\link[stox2.7preprocessing]{writePolygonPositions}}
#' Areas may be plotted for inspection with \code{\link[RstoxFDA]{plotArea}} (see example).
#'
#' @docType data
#'
#' @usage data(coastalCodAreas)
#'
#' @format \code{\link[sp]{SpatialPolygonsDataFrame}} with area names identified in the column 'polygonName'. See \code{\link[RstoxBase]{StratumPolygon}}.
#'
#' @keywords datasets
#'
#' @examples
#'  # plot coastal cod areas
#'  data(coastalCodAreas)
#'  RstoxFDA::plotArea(areaDef=coastalCodAreas, title = "Coastal Cod areas")
"coastalCodAreas"
