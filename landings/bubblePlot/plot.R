library(ggplot2)
library(rgdal)
library(maps)

#' data files, local references
landingsfile <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2018_PR_2019-04-02.psv"

#downloaded from https://kart.fiskeridir.no/stat
mainareafile <- rgdal::readOGR("~/shapefiles/fdir/NMD_annotated/FDIR_Statistikk_hovedomraader","FDIR_Statistikk_hovedomraader", stringsAsFactors = F)

#check that cooridnates are not projected (e.g.: +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0)
print(proj4string(mainareafile))

readLssFile <- function(file, encoding="latin1", guessMax = 100000){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = guessMax)
  return(data.table::as.data.table(db))
}

#' Plots total weigh in areas as bubbleplot
#' @param data data.table::data.table() with sales notes
#' @param areaDef sp::patialPolygonsDataFrame() with spatial definitino for area codes
#' @param areaCol character() identifying column in 'areaDef' corresponding to column "Hovedområde (kode)" in 'data'
#' @param col bubble color
#' @param projection desired map projection
#' @param minlong appxoimately lower limit of longitude to show
#' @param maxlong appxoimately upper limit of longitude to show
#' @param minlat appxoimately lower limit of latitude to show
#' @param maxlat appxoimately ypper limit of latitude to show
#' @param titletext text for plot title
bubblePlot <- function(data, areaDef=mainarea, areaCol="HAVOMR", col="red", projection="azequalarea", minlong=-10, maxlong=40, minlat=57, maxlat=80, titletext=""){
  catchByArea <- aggregate(list(weight=data$Rundvekt/(1000*1000)), by=list(Area=data$`Hovedområde (kode)`), FUN=function(x){sum(x, na.rm=T)})
  
  pos <- sp::coordinates(areaDef)
  pos <- data.frame(Longitude=pos[,1], Latitude=pos[,2])
  pos$Area <- areaDef[[areaCol]]
  
  pos <- merge(pos, catchByArea, all.x=T)
  pos <- pos[!is.na(pos$weight),]
  
  if (is.null(minlong)){
    minlong <- min(pos$Longitude)    
  }
  if (is.null(maxlong)){
    maxlong <- max(pos$Longitude)  
  }
  if (is.null(minlat)){
    minlat <- min(pos$Latitude)  
  }
  if (is.null(maxlat)){
    maxlat <- max(pos$Latitude)  
  }
  
  selection_map <- map_data("world2")
  ggplot(selection_map) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill="black", colour = "white") +
    coord_map(projection, xlim=c(minlong,maxlong), ylim=c(minlat, maxlat))+
    xlab("Longitude") + 
    ylab("Latitude") + 
    geom_point(data=pos, aes(Longitude,Latitude,size = weight), shape=21, alpha = 0.7, colour = "black",fill=col,stroke = .2) +
    scale_size_area(max_size=10) +
    labs(size="weight (kT)") + 
    ggtitle(titletext) +
    theme(axis.title = element_blank(),
          axis.text = element_text(size=14),
          legend.text = element_text(size=14),
          legend.title = element_text(size=16))
  
}

landings <- readLssFile(landingsfile)
bubblePlot(landings, titletext = "all landings, all species, 2018")