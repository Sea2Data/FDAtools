library(rgeos)
library(rgdal)
library(sp)
library(spdep)

#' Writes sp::SpatialPolygonsDataFrame as Stox-WKT files (stratafiles)
#' @param shape sp::SpatialPolygonsDataFrame stratadefinition to convert
#' @param output filename to save output to
#' @param namecol column in shape that contains strata names
writeSpDataFrameAsWKT <- function(shape, output, namecol){
  
  if (file.exists(output)){
    stop(paste("File", output, "exists already."))
  }

  projection="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
  shp <- spTransform(shape, CRS(projection))
  
  f<-file(output, open="w")
  
  for (i in 1:nrow(shp)){
    poly <- shp[i,]
    write(paste(as.character(poly[[namecol]]), writeWKT(poly, byid = F),sep="\t"), f)
  }
  close(f)
  
}

#' Writes sp::SpatialPolygons as Stox-WKT files (stratafiles)
#' @details 
#'  strata names are assumed to be found in the ID slot of spatialPolygons
#' @param shape sp::SpatialPolygonsDataFrame stratadefinition to convert
#' @param output filename to save output to
writeSpAsWKT <- function(shape, output){
  stratanames <- sapply(methods::slot(shape, "polygons"), function(x) methods::slot(x, "ID"))
  stratanames.df <- data.frame( ID=stratanames, row.names = stratanames)
  areaPolygons <- sp::SpatialPolygonsDataFrame(shape, stratanames.df)
  writeSpDataFrameAsWKT(areaPolygons, output, "ID")
}


#' (Borrowed from RstoxBase)
#' @details
#'  StoX strata defintions as stored as a two column tab-delimed file without headers.
#'  Column 1 contain strata names, and column 2 contain WKT encodings of polygons.
#' @param FilePath path to file containing StoX strata defininitions
#' @return SpatialPolygons with strata definition. Strata names in ID slots.
readStoxWKT <- function(FilePath) {
  
  readStoxMultipolygonWKTFromFile <- function(FilePath) {
    # If the input is an existing file path instead of a data.table from the project.xml:
    if(!file.exists(FilePath) || (file.exists(FilePath) && isTRUE(file.info(FilePath)$isdir))) {
      stop("The StoX multipolygon WKT file ", FilePath, " does not exist or is a directory.")
    }
    tab <- data.table::fread(FilePath, sep = "\t", header = FALSE, stringsAsFactors = FALSE, colClasses = c("character", "character"))
    names(tab) <- c("Stratum", "Polygon")
    tab
  }
  
  dataTable2SpatialPolygons <- function(DataTable) {
    
    # 
    polygonName <- as.character(DataTable$Stratum)
    multipolygon <- DataTable$Polygon
    
    # Convert each WKT strings to SpatialPolygons:
    spatialPolygonsList <- lapply(multipolygon, rgeos::readWKT)
    # Extract the Polygons objects to modify the IDs and merge to a SpatialPolygons:
    polygonsList <- lapply(spatialPolygonsList, function(x) slot(x, "polygons")[[1]])
    # Add the polygon names as IDs:
    for (ind in seq_along(polygonsList)) {
      polygonsList[[ind]]@ID <- polygonName[ind]
    }
    # Merge to a SpatialPolygons object:
    spatialPolygons = sp::SpatialPolygons(polygonsList)
    #plot(SpP, col = 1:5, pbg="white")
    
    spatialPolygons
  }
  
  # Read the file as data.table:
  dataTable <- readStoxMultipolygonWKTFromFile(FilePath)
  # Convert to SpatialPolygons:
  spol <- dataTable2SpatialPolygons(dataTable)
  sp::proj4string(spol) <- sp::CRS("+proj=longlat +datum=WGS84")
  return(spol)
}

#' Merges strata definitions.
#' @details
#'  strata names are assumed to be in the slot 'ID'.
#' @param shapes SpatialPolygons with strata defintions
#' @param groups list, mapping new names to groups of old names as identified in ID slots.
#' @return SpatialPolygons with merged strata defintions
#' @example 
#'  # read strata
#'  strata <- readStoxWKT("mainarea.txt")
#'  # define merge
#'  mergespec <- list(northsea=c("40", "41", "42", "8", "28"), skagerak=c("9"))
#'  writeSpAsWKT(newareas, "nssk.txt")
mergeStrata <- function(shapes, groups){
  
  if (!all(unlist(groups) %in% sapply(methods::slot(shapes, "polygons"), function(x) methods::slot(x, "ID")))){
    stop("Some of the strata to be merged does not exist in argument shapes.")
  }
  
  names <- c()
  newpolygons <- NULL
  for (g in names(groups)){
    newname <- g
    oldnames <- groups[[newname]]
    
    polygons <- shapes[sapply(methods::slot(shapes, "polygons"), function(x) methods::slot(x, "ID")) %in% oldnames,]
    newpolygon <- raster::aggregate(polygons)
    
    stopifnot(length(newpolygon) == 1)
    newpolygon@polygons[[1]]@ID <- newname
    if (is.null(newpolygons)){
      newpolygons <- newpolygon
    }
    else{
      newpolygons <- raster::bind(newpolygons, newpolygon, keepnames=T)
    }
    
  }
  
  return(newpolygons)
}


#' Extract a neighbour matrix for strata
#' @return binary matrix (0: not neighbours, 1: neighbours), with strata names as rows and columns
#' @noRd
extractNeighbours <- function(polygons){
  nb <- poly2nb(polygons)
  mat <- nb2mat(nb, style="B")
  colnames(mat) <- rownames(mat)
  return(mat)  
}

#' Save neighbours matrix for strata
#' @description
#'  Determine geographical neighbours and save the result on a format accepted by StoX
#' @details
#'  strata names are assumed to be in the slot 'ID'.
#'  Neighbourhood determination depends on the quality of shape file. 
#'  Odd geometries may cause problems.
#' @param polygons sp::SpatialPolygons with strata defintions
#' @param outfile file to write neighbour defintion to.
#' @examples 
#'  # read strata
#'  strata <- readStoxWKT("mainarea.txt")
#'  # save neigbhours
#'  saveNeighbours(strata, "mainarea_neighbours.txt")
saveNeighbours <- function(polygons, outfile){
  
  if (file.exists(outfile)){
    stop(paste("File", outfile, "already exists."))
  }
  
  neighbourmatrix <- extractNeighbours(polygons)
  out <- file(outfile, open="w")
  write("stratum\tneighbours", out)
  for (i in 1:nrow(neighbourmatrix)){
    write(paste(rownames(neighbourmatrix)[i], paste(colnames(neighbourmatrix)[neighbourmatrix[i,]==1], collapse=","), sep="\t"), out)
  }
  close(out)
}
