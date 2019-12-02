source("landings_overview.R")
#stocks <- read.csv("data/stocks_COD_HAD_POK.csv", sep="\t", header = T, comment.char = "#", stringsAsFactors = F)
#metier <- read.csv("data/fleet_COD_HAD_POK.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
#metierlandings <- read.csv("data/fleet_wo_logbook_COD_HAD_POK.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
stocks <- read.csv("data/stocks.csv", sep="\t", header = T, comment.char = "#", stringsAsFactors = F)
metier <- read.csv("data/fleet.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
metierlandings <- read.csv("data/fleet_wo_logbook.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
areas <- read.csv("data/areas.csv", sep="\t", header = T, comment.char = "#", colClasses = c("character", "character", "character"), stringsAsFactors = F)

if (!all(stocks$ICESarea %in% areas$ICESarea)){
  print(stocks$ICESarea[!(stocks$ICESarea %in% areas$ICESarea)])
  stop("FDIR area not defined for all ICES areas")
}

#
# prep logbooks
#


log_all_areas <- log[substr(log$LOKASJON_START,1,2) %in% unique(areas$FDIRarea),]
log_all_areas <- log_all_areas[!is.na(log_all_areas$FANGSTART_NS),]
log_all_areas <- log_all_areas[log_all_areas$FANGSTART_FAO %in% unique(stocks$FAOcode),]


get_quarter <- function(month){
  if (month %in% c("01", "02", "03")){
    return("1")
  }
  if (month %in% c("04", "05", "06")){
    return("2")
  }
  if (month %in% c("07", "08", "09")){
    return("3")
  }
  if (month %in% c("10", "11", "12")){
    return("4")
  }
}
get_quarter <- Vectorize(get_quarter)

annotate_logbooks <- function(stock, logb){
  nr <- nrow(logb)
  # annotate area on logbooks
  logb$FDIRarea <- substr(logb$LOKASJON_START,1,2)

  logb <- merge(logb, areas, all.x=T)
  if (any(is.na(logb$ICESarea))){
    print(unique(logb[is.na(logb$ICESarea),c("FDIRarea")]))
    stop("Some logbook entries are not assigned ICES area")
  }
  
  if (nrow(logb)!=nr){
    stop("Too many rows after adding areas")
  }
  # annotate metier on logbooks
  logb[is.na(logb$MASKEVIDDE), "MASKEVIDDE"] <- "NON"
  logb <- merge(logb, metier, all.x=T)
  if (nrow(logb)!=nr){
    stop("Too many rows after adding metiers. Duplicates in metier config ?")
  }
  if (any(is.na(logb$metier))){
    print(unique(logb[is.na(logb$metier), c("REDSKAP_FAO", "MASKEVIDDE", "ICESareatype", "ICESarea")]))
    stop("Some logbook entries are not assigned metier")
  }
  # annotate quarter logbooks
  get_quarter <- Vectorize(get_quarter)
  
  logb$quarter <- get_quarter(substr(logb$STARTTIDSPUNKT, 6,7))
  if (nrow(logb)!=nr){
    stop()
  }
  return(logb)
}




#
# prep landings
#
annotate_landings <- function(stock, land){
  nr <- nrow(land)
  
  # annotate area on landings
  land <- merge(land, areas, all.x=T, by.x="Hovedområde (kode)", by.y="FDIRarea")
  if (nrow(land)!=nr){
    stop("Too many rows after adding areas")
  }
  
  if (any(is.na(land$ICESarea))){
    print(unique(land[is.na(land$ICESarea),c("FDIRarea")]))
    stop("Some landings were not ICES area")
  }
  
  # annotate metier on landings
  land <- merge(land, metierlandings, all.x=T, by.x=c("Redskap (kode)", "ICESarea", "ICESareatype"), by.y=c("REDSKAP_NS", "ICESarea", "ICESareatype"))
  if (nrow(land)!=nr){
    stop("Too many rows after adding metiers")
  }
  if (any(is.na(land$metier))){
    print(unique(land[is.na(land$metier),c("Redskap (kode)", "ICESarea", "ICESareatype")]))
    stop("Some landings were not assigned metier")
  }
  
  # annotate quarter on landings
  land$quarter <- get_quarter(substr(land$`Siste fangstdato`, 4,5))  
  
  if (nrow(land)!=nr){
    stop("Too many rows")
  }
  return(land)
}

land_all_areas <- landings[landings$`Hovedområde (kode)` %in% unique(areas$FDIRarea),]
land_all_areas <- land_all_areas[land_all_areas$`Art FAO (kode)` %in% stocks$FAOcode,]
land_all_areas <- land_all_areas[land_all_areas$`Redskap (kode)`!=90,]#oppdrett

#
# landings with corresponding logbooks
#
land_all_areas_w_logbook <- land_all_areas[land_all_areas$`Registreringsmerke (seddel)` %in% unique(log_all_areas$REGM),]


#
# landings with no corresponding logbooks
#
land_all_areas_wo_logbook <- land_all_areas[!(land_all_areas$`Registreringsmerke (seddel)` %in% unique(log_all_areas$REGM)),]



#
# aggregate catches
#

#' Reports total catch by ICES area, ICES area type, metier and quarter
#' Releative fractions are based on logbooks, totals are based on landings
#' @param species (FAO kode)
#' @param landings
#' @param logbooks annotated with ICES area, ICES area type, metier and quarter
#' @param huc string to indicate what should be aggregated ("consumption", "non-consumption", "all")
distribute_by_logbook <- function(species, landingsdata, logbooks, huc="consumption", nocoastal=F){
  if (nocoastal){
    landingsdata <- landingsdata[landingsdata$`Kyst/hav (kode)`==0,]
  }
  
 if (!all(landingsdata$`Registreringsmerke (seddel)` %in% unique(logbooks$REGM))){
   stop("Some vessels in landings are not found in logbooks")
 }
  logbooks <- logbooks[logbooks$FANGSTART_FAO==species,]
  landingsdata <- landingsdata[landingsdata$`Art FAO (kode)`==species,]
  
  if (huc=="all"){
    totalland <- sum(landingsdata$Rundvekt)    
  }
  else if (huc=="consumption"){
    landingsdata <- landingsdata[!is.na(landingsdata$`Anvendelse hovedgruppe (kode)`) & landingsdata$`Anvendelse hovedgruppe (kode)`==1,]
  }
  else if (huc=="non-consumption"){
    landingsdata <- landingsdata[is.na(landingsdata$`Anvendelse hovedgruppe (kode)`) | landingsdata$`Anvendelse hovedgruppe (kode)`!=1,]
  }
  else{
    stop(paste("Argument", huc, "not understood for parameter huc"))
  }
  
  if (nrow(logbooks)==0 | nrow(landingsdata)==0){
    return(data.frame(weightKG=numeric(), ICESarea=character(), ICESareatype=character(), quarter=character(), metier=character()))
  }
  
  prew <- sum(landingsdata$Rundvekt)
  
  totalland <- aggregate(list(totalland=landingsdata$Rundvekt),list(redskap=landingsdata$`Registreringsmerke (seddel)`), function(x){sum(x)})
  totalagg <- aggregate(list(totalmet=logbooks$RUNDVEKT),list(redskap=logbooks$REGM), function(x){sum(x)})
  logagg <- aggregate(list(totalcell=logbooks$RUNDVEKT),list(ICESarea=logbooks$ICESarea, ICESareatype=logbooks$ICESareatype, quarter=logbooks$quarter, metier=logbooks$metier, redskap=logbooks$REGM), function(x){sum(x)})
  logagg <- merge(logagg, totalagg)
  logagg <- merge(logagg, totalland)
  logagg$weightKG <- logagg$totalland*logagg$totalcell/logagg$totalmet
  logagg <- aggregate(list(weightKG=logagg$weightKG),list(ICESarea=logagg$ICESarea, ICESareatype=logagg$ICESareatype, quarter=logagg$quarter, metier=logagg$metier), function(x){sum(x)})
  
  # unassinged weight happens when landed species are not noted in logbooks etc spread proportionally on metiers
  unassigned <- prew - sum(logagg$weightKG)
  uw <- logagg$weightKG/sum(logagg$weightKG)
  logagg$weightKG <- logagg$weightKG + uw*unassigned
  
  if (any(is.na(logagg$weightKG))){
    print(logagg[is.na(logagg$weightKG),])
    stop("NA in weights")
  }
  if ((abs(sum(logagg$weightKG)-prew)/prew)>0.01){
    stop(paste("Weights differ by more than 1%:", (sum(logagg$weightKG)-prew)/prew))
  }
  
  return(logagg)
}

#' Reports total catch by ICES area, ICES area type, metier and quarter
#' @param species (FAO kode)
#' @param landings annotated with ICES area, ICES area type, metier and quarter
#' @param huc string to indicate what should be aggregated ("consumption", "non-consumption", "all")
distribute_by_landings <- function(species, landings, huc="consumption", nocoastal=F){
  landings <- landings[landings$`Art FAO (kode)`==species,]
  
  if (nocoastal){
    landings <- landings[landings$`Kyst/hav (kode)`==0,]
  }
  
  if (huc=="consumption"){
    landings$weight<-0
    landings[landings$`Anvendelse hovedgruppe (kode)`==1,"weight"] <- landings[landings$`Anvendelse hovedgruppe (kode)`==1,"Rundvekt"]
  }
  else if (huc=="non-consumption"){
    landings$weight<-0
    landings[landings$`Anvendelse hovedgruppe (kode)`!=1,"weight"] <- landings[landings$`Anvendelse hovedgruppe (kode)`!=1,"Rundvekt"]
  }
  else if (huc!="all"){
    stop(paste("Argument", huc, "not understood for parameter huc"))
  }

  if (nrow(landings)==0){
      return(data.frame(weightKG=numeric(), ICESarea=character(), ICESareatype=character(), quarter=character(), metier=character()))
  }
  
  landingsagg <- aggregate(list(weightKG=landings$weight),list(ICESarea=landings$ICESarea, ICESareatype=landings$ICESareatype, quarter=landings$quarter, metier=landings$metier), sum)
  
  if (any(is.na(landingsagg$weightKG))){
    stop("NA in weights")
  }
  
  return(landingsagg)
}

output="./output"
Country="NO"
Year="2018"
ReportingCategory="R"
SamplesOrigin="NA"
write_intercatch <-function(species, data){
  print(species)
  print(paste("eksporting: ", sum(data$weightKG)))
  print(paste("frac areas: ", sum(data$weightKG)/sum(land_all_areas[land_all_areas$`Art FAO (kode)`==species, "Rundvekt"])))
  print(paste("frac NO: ", sum(data$weightKG)/sum(landings[landings$`Art FAO (kode)`==species, "Rundvekt"])))
  if (sum(landings[landings$`Art FAO (kode)`==species, "Rundvekt"])<sum(data$weightKG)){
    stop("Problems with aggregation")
  }
  stream <- file(paste(output, paste(species, Year, "nor_intercatch_wgnssk.csv", sep="_"), sep="/"), open="w")
  data <- data[order(data$ICESarea, data$metier, data$quarter, data$CatchCategory),]
  data$id <- paste(data$ICESarea, data$ICESareatype, data$metier, data$quarter, sep="/")
  for (id in unique(data$id)){
    cell <- data[data$id==id,]
    write(paste("HI", Country, Year, SeasonType="Quarter", cell[1, "quarter"], cell[1,"metier"], cell[1,"ICESareatype"], cell[1,"ICESarea"], "NA", "NA", -9, "NA", sep=","), stream)
    for (i in 1:nrow(cell)){
      write(paste("SI", Country, Year, SeasonType="Quarter",cell[i, "quarter"], cell[i,"metier"], cell[i,"ICESareatype"], cell[i,"ICESarea"],"NA", species, "NA", cell[i, "CatchCategory"], ReportingCategory, "NA", "NA", SamplesOrigin, "NA", "T", round(cell[i,"weightKG"]/1000, digits=2), -9,-9,"", "", "", sep=","), stream)
    }
  }
  close(stream)
}

runstock <- function(stock){
  species <- stocks[stocks$stocckcode==stock,"FAOcode"][1]
  stockareas <- unique(stocks[stocks$stocckcode==stock,"ICESarea"])
  logb <- annotate_logbooks(stock, log_all_areas)
  land_w_logbook <- land_all_areas_w_logbook
  land_wo_logbook <- annotate_landings(stock, land_all_areas_wo_logbook)
  nocoastal <- !stocks[stocks$stocckcode==stock,"includecoast"][1]
  
  if (all(stocks$FAOcode!=species | stocks$BMShuc)){
    consumption_log <- distribute_by_logbook(species, land_w_logbook, logb, "consumption", nocoastal)
    consumption_log <- consumption_log[consumption_log$ICESarea %in% stockareas,]
    consumption_land <- distribute_by_landings(species, land_wo_logbook, "consumption", nocoastal)
    consumption_land <- consumption_land[consumption_land$ICESarea %in% stockareas,]
    
    consumption <- rbind(consumption_log, consumption_land)
    consumption <- aggregate(list(weightKG=consumption$weightKG), list(ICESarea=consumption$ICESarea, ICESareatype=consumption$ICESareatype, quarter=consumption$quarter, metier=consumption$metier), sum)
    consumption$CatchCategory <- "L"
    
    nonconsumption_log <- distribute_by_logbook(species, land_w_logbook, logb, "non-consumption", nocoastal)
    nonconsumption_log <- nonconsumption_log[nonconsumption_log$ICESarea %in% stockareas,]
    nonconsumption_land <- distribute_by_landings(species, land_wo_logbook, "non-consumption", nocoastal)
    nonconsumption_land <- nonconsumption_land[nonconsumption_land$ICESarea %in% stockareas,]
    
    nonconsumption <- rbind(nonconsumption_log, nonconsumption_land)
    nonconsumption <- aggregate(list(weightKG=nonconsumption$weightKG), list(ICESarea=nonconsumption$ICESarea, ICESareatype=nonconsumption$ICESareatype, quarter=nonconsumption$quarter, metier=nonconsumption$metier), sum)
    nonconsumption$CatchCategory <- "B"
    
    write_intercatch(species, rbind(consumption, nonconsumption))
  }
  else if (all(stocks$FAOcode!=species | !stocks$BMShuc)){
    all_log <- distribute_by_logbook(species, land_w_logbook, logb, "all", nocoastal)
    all_log <- all_log[all_log$ICESarea %in% stockareas,]
    all_land <- distribute_by_landings(species, land_wo_logbook, "all", nocoastal)
    all_land <- all_land[all_land$ICESarea %in% stockareas,]
    
    alld <- rbind(all_land, all_log)
    alld <- aggregate(list(weightKG=alld$weightKG), list(ICESarea=alld$ICESarea, ICESareatype=alld$ICESareatype, quarter=alld$quarter, metier=alld$metier), sum)
    alld$CatchCategory <- "L"
    
    write_intercatch(species, alld)
    
  }
  else{
    stop("BMShuc pr area not supported")
  }
}

runall <- function(){
  for (stock in unique(stocks$stocckcode)){
    runstock(stock)
  }
}
runall()


# last dagbok og logbok
# avgrens til område (inkl naboområder)


