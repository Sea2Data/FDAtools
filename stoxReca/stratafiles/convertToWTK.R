library(rgeos)
library(rgdal)
library(sp)

#' Writes sp::SpatialPolygonsDataFrame as Stox-WKT files (stratafiles)
#' @param shapefile sp::SpatialPolygonsDataFrame stratadefinition to convert
#' @param output filename to save output to
#' @param namecol column in shapefile that contains strata names
convertToWKT <- function(shapefile, output, namecol){
  
  if (file.exists(output)){
    stop(paste("File", output, "exists already."))
  }
  
  projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  shp <- spTransform(shapefile, CRS(projection))
  
  f<-file(output, open="w")
  
  for (i in 1:nrow(shp)){
    poly <- shp[i,]
    write(paste(as.character(poly[[namecol]]), writeWKT(poly, byid = F),sep="\t"), f)
  }
  close(f)
  
}