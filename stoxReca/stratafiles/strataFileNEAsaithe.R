source("stratafiles.R")

#
# example for manually fixing 'holes' in polygon files
#

hovedomr <- readStoxWKT("~/workspace/stox/reference/stratum/mainarea.txt")

# Fra ECA-config fil
#ngroups
#8
#groups
#3 2 10 11 13 14 15 16 17 18 24 1
#12
#4
#5 37 36 39 38
#0
#20 21 22 23 25 26 27
#6
#7 30 34 35 50
groups <- list()
groups[["3-2-10-11-13-14-15-16-17-18-24-1"]] <- c("3", "2", "10", "11", "13", "14", "15", "16", "17", "18", "24", "1")
groups[["4-12-20-21-22-23-25-26-27"]] <- c("4","12","20","21","22", "23","25","26","27")
groups[["5-37-36-39-38"]] <- c("5", "37", "36", "39", "38")
groups[["0"]] <- c("0")
groups[["6"]] <- c("6")
groups[["7-30-34-35"]] <- c("7", "30", "34", "35")


newstrata <- mergeStrata(hovedomr, groups)

#fix hole in strata 2
polylist <- lapply(newstrata@polygons, function(x){x})
polylist[[2]]@Polygons <- polylist[[2]]@Polygons[1:1]
polylist[[2]]@plotOrder <- polylist[[2]]@plotOrder[1:1]
attr(polylist[[2]], "comment") <- "0"

newstrata <- SpatialPolygons(polylist)
sp::proj4string(newstrata) <- sp::CRS("+proj=longlat +datum=WGS84")

plot(newstrata)

neighbours <- extractNeighbours(newstrata)
writeSpAsWKT(newstrata, "NEAsaitheStrata.txt")
saveNeighbours(newstrata, "NEAsaitheNeighbours.txt")
