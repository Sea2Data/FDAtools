#
# Attempts at generating mappings between are-coding systems based on polygon files
#

#based on ICES area
locations <- RstoxFDA::locationsFdir2018
locationsWPos <- RstoxFDA::appendPosition(locations, RstoxFDA::locationsFdir2018, "StratumName", "lat", "lon")
locationsWices <- RstoxFDA::appendAreaCode(data.table::as.data.table(locationsWPos), RstoxFDA::ICESareas, "lat", "lon", "Area_27")
locationsWices <- locationsWices[,c("StratumName", "HAVOMR", "Lokasjon", "Area_27")]
names(locationsWices) <- c("OMR-LOK", "HAVOMR", "Lokasjon", "ICES")

# plot missing
RstoxFDA::plotArea(areaDef = locations[locations$StratumName %in% sort(locationsWices$`OMR-LOK`[is.na(locationsWices$ICES)]),], areaLabels = F)

#based on ICES rectangels
locations <- RstoxFDA::locationsFdir2017
recs <- RstoxFDA::ICESrectangles
recs$StratumName <- recs$Area_27
locationsWPosRect <- RstoxFDA::appendPosition(locations, RstoxFDA::locationsFdir2017, "StratumName", "lat", "lon")
locationsWicesRect <- RstoxFDA::appendAreaCode(data.table::as.data.table(locationsWPosRect), recs, "lat", "lon", "Area_27")
locationsWicesRect <- locationsWicesRect[,c("StratumName", "HAVOMR", "Lokasjon", "Area_27")]
names(locationsWicesRect) <- c("OMR-LOK", "HAVOMR", "Lokasjon", "ICES")

# plot missing
RstoxFDA::plotArea(areaDef = locations[locations$StratumName %in% sort(locationsWicesRect$`OMR-LOK`[is.na(locationsWicesRect$ICES)]),], areaLabels = F)

#check consistency
d<-merge(locationsWices, locationsWicesRect, by="OMR-LOK", suffix=c(".ICESa", ".ICESrect"), all.y=T)
stopifnot(nrow(d) == nrow(locationsWicesRect))


# list differences
print(d[d$ICES.ICESa != d$ICES.ICESrect,c("ICES.ICESa", "ICES.ICESrect", "OMR-LOK")])

# plotDifferences
polygonsDifferent <- locations[locations$StratumName %in% d$`OMR-LOK`[!is.na(d$ICES.ICESa) & d$ICES.ICESa != d$ICES.ICESrect],]
plot(RstoxFDA::ICESareas)
plot(polygonsDifferent, add=T, col="red")

