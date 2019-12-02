library(readr)
library(imrParsers)

colt <- cols(
  Dokumentnummer = col_character(),
  `Dokumenttype (kode)` = col_integer(),
  Dokumenttype = col_character(),
  `Dokument versjonsnummer` = col_integer(),
  `Dokument salgsdato` = col_character(),
  `Dokument versjonstidspunkt` = col_character(),
  `Salgslag ID` = col_integer(),
  `Salgslag (kode)` = col_integer(),
  Salgslag = col_character(),
  `Mottakernasjonalitet (kode)` = col_character(),
  Mottakernasjonalitet = col_character(),
  Mottaksstasjon = col_character(),
  `Landingskommune (kode)` = col_integer(),
  Landingskommune = col_character(),
  `Landingsfylke (kode)` = col_integer(),
  Landingsfylke = col_character(),
  `Landingsnasjon (kode)` = col_character(),
  Landingsnasjon = col_character(),
  Produksjonsanlegg = col_character(),
  `Produksjonskommune (kode)` = col_character(),
  Produksjonskommune = col_character(),
  `Fiskerkommune (kode)` = col_integer(),
  Fiskerkommune = col_character(),
  `Fiskernasjonalitet (kode)` = col_character(),
  Fiskernasjonalitet = col_character(),
  Fartøynavn = col_character(),
  `Fartøy ID` = col_integer(),
  `Registreringsmerke (seddel)` = col_character(),
  `Radiokallesignal (seddel)` = col_character(),
  `Største lengde` = col_double(),
  `Lengdegruppe (kode)` = col_integer(),
  Lengdegruppe = col_character(),
  `Bruttotonnasje 1969` = col_integer(),
  `Bruttotonnasje annen` = col_integer(),
  Byggeår = col_integer(),
  Ombyggingsår = col_integer(),
  Motorkraft = col_integer(),
  Motorbyggeår = col_integer(),
  `Fartøy gjelder fra dato` = col_character(),
  `Fartøy gjelder til dato` = col_character(),
  `Fartøytype (kode)` = col_character(),
  Fartøytype = col_character(),
  `Kvotefartøy reg.merke` = col_character(),
  `Fartøykommune (kode)` = col_integer(),
  Fartøykommune = col_character(),
  `Fartøyfylke (kode)` = col_integer(),
  Fartøyfylke = col_character(),
  `Fartøynasjonalitet (kode)` = col_character(),
  Fartøynasjonalitet = col_character(),
  `Mottakende fartøy reg.merke` = col_character(),
  `Mottakende fartøy rkal` = col_character(),
  `Mottakende fartøytype (kode)` = col_character(),
  `Mottakende fart.type` = col_character(),
  `Mottakende fartøynasj. (kode)` = col_character(),
  `Mottakende fart.nasj` = col_character(),
  Fangstår = col_integer(),
  `Siste fangstdato` = col_character(),
  `Kvotetype (kode)` = col_character(),
  Kvotetype = col_character(),
  `Redskap (kode)` = col_integer(),
  Redskap = col_character(),
  `Redskap - hovedgruppe (kode)` = col_character(),
  `Redskap - hovedgruppe` = col_character(),
  `Fangstfelt (kode)` = col_character(),
  `Kyst/hav (kode)` = col_integer(),
  `Hovedområde (kode)` = col_character(),
  Hovedområde = col_character(),
  `Lokasjon (kode)` = col_character(),
  `Sone (kode)` = col_character(),
  Sone = col_character(),
  Områdegruppering = col_character(),
  `Hovedområde FAO (kode)` = col_integer(),
  `Hovedområde FAO` = col_character(),
  `Nord/sør for 62 grader nord` = col_character(),
  `Fangstdagbok (nummer)` = col_integer(),
  `Fangstdagbok (turnummer)` = col_integer(),
  Landingsdato = col_character(),
  Landingsklokkeslett = col_datetime(format = ""),
  `Dellanding (signal)` = col_integer(),
  `Neste mottaksstasjon` = col_character(),
  `Forrige mottakstasjon` = col_character(),
  Linjenummer = col_integer(),
  `Art - FDIR (kode)` = col_character(),
  `Art - FDIR` = col_character(),
  `Art - gruppe (kode)` = col_character(),
  `Art - gruppe` = col_character(),
  `Art - hovedgruppe (kode)` = col_character(),
  `Art - hovedgruppe` = col_character(),
  `Art FAO (kode)` = col_character(),
  `Art FAO` = col_character(),
  `Produkttilstand (kode)` = col_integer(),
  Produkttilstand = col_character(),
  `Konserveringsmåte (kode)` = col_integer(),
  Konserveringsmåte = col_character(),
  `Landingsmåte (kode)` = col_integer(),
  Landingsmåte = col_character(),
  `Kvalitet (kode)` = col_integer(),
  Kvalitet = col_character(),
  `Størrelsesgruppering (kode)` = col_integer(),
  `Anvendelse (kode)` = col_integer(),
  Anvendelse = col_character(),
  `Anvendelse hovedgruppe (kode)` = col_integer(),
  `Anvendelse hovedgruppe` = col_character(),
  `Antall stykk` = col_integer(),
  Bruttovekt = col_double(),
  Produktvekt = col_double(),
  Rundvekt = col_double()
)

loc <- default_locale()
loc$decimal_mark <- ","
loc$encoding <- "latin1"

omr_cod <- c("09", "08", "28", "42", "41", "40", "46") #egentlig kun halve 09 og halve 46, ingen fangst i feil halvdel ihht logbøker
omr_had <- c("09", "08", "28", "42", "41", "40", "43") #egentlig kun halve 09, ingen fangst i feil halvdel ihht logbøker
omr_saithe <- c("09", "08", "28", "42", "41", "40", "43", "47")

codcodes <- c("1022", "102201", "102202", "102203", "102204")
hadcodes <- c("1027", "102701", "102702", "102703", "102704")
pokcodes <- c("1032")

spatial_union <- unique(c(omr_saithe, omr_had, omr_cod))

landings <- read_delim("data//FDIR_HI_LSS_FANGST_2018_PR_2019-04-02.psv", delim="|", locale = loc, trim_ws=TRUE, na=c("", "na", "NA"), col_names = T, col_types = colt)
landings <- landings[landings$`Fartøynasjonalitet (kode)`=="NOR",]

landings_spatial_union <- landings[landings$`Hovedområde (kode)` %in% spatial_union,]
konsum_spatial_union <- landings_spatial_union[landings_spatial_union$`Anvendelse hovedgruppe (kode)`==1,]
annet_spatial_union <- landings_spatial_union[landings_spatial_union$`Anvendelse hovedgruppe (kode)`!=1,]
anvendelse <- merge(aggregate(list(konsum=konsum_spatial_union$Rundvekt), list(art=konsum_spatial_union$`Art FAO (kode)`), sum), aggregate(list(annet=annet_spatial_union$Rundvekt), list(art=annet_spatial_union$`Art FAO (kode)`), sum), all=T)
anvendelse <- anvendelse[order(anvendelse$konsum, decreasing = T),]

polycod <- imrParsers::fdir.polygons[imrParsers::fdir.polygons$HAVOMR %in% omr_cod,]
polysaith <- imrParsers::fdir.polygons[imrParsers::fdir.polygons$HAVOMR %in% omr_saithe,]
polyhad <- imrParsers::fdir.polygons[imrParsers::fdir.polygons$HAVOMR %in% omr_had,]
log<-imrParsers::parse_ers_tab("data/EFD_2018.psv")
logcod <- log[log$FANGSTART_FAO=="COD",]
logsaithe <- log[log$FANGSTART_FAO=="POK",]
loghad <- log[log$FANGSTART_FAO=="HAD",]



#cod
plot(polycod, border="white")
plot(ices.polygons, add=T)
points(logcod$START_LG, logcod$START_LT, col="red", main="COD")

#saithe
plot(polysaith, border="white")
plot(ices.polygons, add=T)
points(logsaithe$START_LG, logsaithe$START_LT, col="red", main="POK")

#haddock
plot(polyhad, border="white")
plot(ices.polygons, add=T)
points(loghad$START_LG, loghad$START_LT, col="red", main="HAD")

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
