library(readr)
library(Rstox)
library(rgdal)

fdir.polygons <- readOGR("data/Hovedområder_fom_2018/","Homr_2018")

readLssFile <- function(file, encoding="latin1", guessMax = 100000){
  loc <- readr::default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- readr::read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, guess_max = guessMax)
  return(db)
}

omr_cod <- c("09", "08", "28", "42", "41", "40", "46") #egentlig kun halve 09 og halve 46, ingen fangst i feil halvdel ihht logbøker
omr_had <- c("09", "08", "28", "42", "41", "40", "43") #egentlig kun halve 09, ingen fangst i feil halvdel ihht logbøker
omr_saithe <- c("09", "08", "28", "42", "41", "40", "43", "47")

codcodes <- c("1022", "102201", "102202", "102203", "102204")
hadcodes <- c("1027", "102701", "102702", "102703", "102704")
pokcodes <- c("1032")

spatial_union <- unique(c(omr_saithe, omr_had, omr_cod))

landings <- readLssFile("data/FDIR_HI_LSS_FANGST_2019_PR_2020-03-03.psv")
landings <- landings[landings$`Fartøynasjonalitet (kode)`=="NOR",]

landings_spatial_union <- landings[landings$`Hovedområde (kode)` %in% spatial_union,]
konsum_spatial_union <- landings_spatial_union[landings_spatial_union$`Anvendelse hovedgruppe (kode)`==1,]
annet_spatial_union <- landings_spatial_union[landings_spatial_union$`Anvendelse hovedgruppe (kode)`!=1,]
anvendelse <- merge(aggregate(list(konsum=konsum_spatial_union$Rundvekt), list(art=konsum_spatial_union$`Art FAO (kode)`), sum), aggregate(list(annet=annet_spatial_union$Rundvekt), list(art=annet_spatial_union$`Art FAO (kode)`), sum), all=T)
anvendelse <- anvendelse[order(anvendelse$konsum, decreasing = T),]

polycod <- fdir.polygons[fdir.polygons$HAVOMR %in% omr_cod,]
polysaith <- fdir.polygons[fdir.polygons$HAVOMR %in% omr_saithe,]
polyhad <- fdir.polygons[fdir.polygons$HAVOMR %in% omr_had,]
log <- Rstox::readErsFile("data/FDIR_HI_ERS_2019_PR_2020-03-04.psv")
logcod <- log[log$FANGSTART_FAO=="COD",]
logsaithe <- log[log$FANGSTART_FAO=="POK",]
loghad <- log[log$FANGSTART_FAO=="HAD",]



comp <- function(landing, logb, omr, art){
  logb$omr <- substr(logb$LOKASJON_START, 1, 2)
  logb <- logb[logb$FANGSTART_FAO==art,]
  logb <- logb[logb$omr %in% omr,]
  landing <- landing[landing$`Hovedområde (kode)` %in% omr,]
  landing <- landing[landing$`Art FAO (kode)`==art,]
  
  landing <- landing[landing$`Registreringsmerke (seddel)` %in% unique(logb$REGM),]
  if (!all(logb$REGM %in% unique(landing$`Registreringsmerke (seddel)`))){
    nn <- logb[!(logb$REGM %in% unique(landing$`Registreringsmerke (seddel)`)),]
    warning(paste(nrow(nn), "logbook records (", length(unique(nn$REGM)), "regms)  with no corresponding regm in landings"))
  }
  
  tab <- merge(aggregate(list(log=logb$RUNDVEKT), list(omr=logb$omr), sum), aggregate(list(land=landing$Rundvekt), list(omr=landing$`Hovedområde (kode)`), sum))
  tab$lo <- log(tab$log/tab$land)
  tab$reldiff <- (tab$log - tab$land)/tab$land
  tab <- tab[order(tab$land, decreasing = T),]
  tab$frac <- tab$land/sum(tab$land)
  tab$actot <- cumsum(tab$land)/sum(tab$land)
  return(tab)
}

comp(landings, log, omr_saithe, "POK")
comp(landings, log, omr_had, "HAD")
comp(landings, log, omr_cod, "COD")


landings_cod <- landings[landings$`Hovedområde (kode)` %in% omr_cod & landings$`Art - FDIR (kode)`  %in% codcodes,]
landings_had <- landings[landings$`Hovedområde (kode)` %in% omr_had & landings$`Art - FDIR (kode)` %in% hadcodes,]
landings_pok <- landings[landings$`Hovedområde (kode)` %in% omr_saithe & landings$`Art - FDIR (kode)` %in% pokcodes,]
