# shapefiles for terretorial line (12-mil) is downloaded from:
# https://kart.fiskeridir.no/fiskeri
# and for main ares from
# https://kart.fiskeridir.no/nedlasting
# 
library(rgdal)
library(rgeos)
library(maptools)
mainArea <- readOGR("~/shapefiles/fdir/fdir_annotated/Hovedområder_fom_2018/","Homr_2018", stringsAsFactors = F)

#extend territorial line southwards so that it extends below sourthern border of area 3.
terretorialLine <- readOGR("~/shapefiles/fdir/terretorialgrense", "Territorialgrense", stringsAsFactors = F)
newcoords <- rbind(coordinates(terretorialLine)[[1]][[1]], matrix(c(31.58550, 69.0), nrow = 1))
S1 <- Lines(list(sp::Line(newcoords)), ID = "territorialline")
modifiedTerretorialLine <- SpatialLines(list(S1))
proj4string(modifiedTerretorialLine) <- CRS(projargs = proj4string(terretorialLine))

coastalCodAreas <- mainArea[mainArea$HAVOMR %in% c("00","03","04","05","06","07"),]
plot(coastalCodAreas)
originalProjection <- proj4string(coastalCodAreas)

coastalCodAreas <- sp::spTransform(coastalCodAreas, CRSobj = sp::CRS("+init=epsg:3395"))
modifiedTerretorialLine <- sp::spTransform(modifiedTerretorialLine, CRSobj = sp::CRS("+init=epsg:3395"))

intersection <- gIntersection(coastalCodAreas, modifiedTerretorialLine, )
blpi <- gBuffer(intersection, width = 0.000001)
dpi <- gDifference(coastalCodAreas, blpi, byid = T) 

coastalCodAreas <- spTransform(coastalCodAreas, CRSobj = sp::CRS(originalProjection))
dpi <- spTransform(dpi, CRSobj = sp::CRS(originalProjection))


pols <- list()
pols$a000 <- Polygons(list(coastalCodAreas@polygons[[1]]@Polygons[[1]]),"s000")
pols$a300 <- Polygons(list(dpi@polygons[[2]]@Polygons[[1]]), "s300")
pols$a301 <- Polygons(list(dpi@polygons[[2]]@Polygons[[2]]), "s301")
pols$a400 <- Polygons(list(dpi@polygons[[3]]@Polygons[[2]]), "s400")
pols$a401 <- Polygons(list(dpi@polygons[[3]]@Polygons[[1]]), "s401")
pols$a500 <- Polygons(list(dpi@polygons[[4]]@Polygons[[1]]), "s500")
pols$a501 <- Polygons(list(dpi@polygons[[4]]@Polygons[[2]]), "s501")
pols$a600 <- Polygons(list(dpi@polygons[[5]]@Polygons[[2]]), "s600")
pols$a601 <- Polygons(list(dpi@polygons[[5]]@Polygons[[1]]), "s601")
pols$a700 <- Polygons(list(dpi@polygons[[6]]@Polygons[[2]]), "s700")
pols$a701 <- Polygons(list(dpi@polygons[[6]]@Polygons[[1]]), "s701")

stratasystem <- SpatialPolygons(pols)
sp::proj4string(stratasystem) <- originalProjection

plot(stratasystem, add=T, col="red")
pointLabel(coordinates(stratasystem),labels=names(stratasystem))

#
# grouped for AFWG
#

groupedStrata <- unionSpatialPolygons(SpatialPolygons(list(pols$a301, 
                                                      pols$a401, 
                                                      
                                                      pols$a300,
                                                      pols$a400,
                                                      
                                                      pols$a501,
                                                      pols$a000,
                                                      
                                                      pols$a500,
                                                      
                                                      pols$a601,
                                                      pols$a701,
                                                      
                                                      pols$a600,
                                                      pols$a700
                                                      )), c("s301-401",
                                                            "s301-401",
                                                            
                                                            "s300-400",
                                                            "s300-400",
                                                            
                                                            "s000-501",
                                                            "s000-501",
                                                            
                                                            "s500",
                                                            
                                                            "s601-701",
                                                            "s601-701",
                                                            
                                                            "s600-700",
                                                            "s600-700"
                                                            ))

sp::proj4string(groupedStrata) <- originalProjection

plot(groupedStrata)
pointLabel(coordinates(groupedStrata),labels=names(groupedStrata))


source("stratafiles.R")
writeSpAsWKT(stratasystem, "CoastalCodRecaStrata.txt")
saveNeighbours(stratasystem, "CoastalCodRecaStrataNeigbours.txt")

writeSpAsWKT(groupedStrata, "CoastalCodRecaStrataAFWG.txt")
saveNeighbours(groupedStrata, "CoastalCodRecaStrataNeigboursAFWG.txt")
