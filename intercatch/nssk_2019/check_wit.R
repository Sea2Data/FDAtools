library(readr)

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
  Landingsklokkeslett = col_character(),
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

stocks <- read.csv("data/stocks.csv", sep="\t", header = T, comment.char = "#", stringsAsFactors = F)
metier <- read.csv("data/fleet.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
metierlandings <- read.csv("data/fleet_wo_logbook.csv", sep="\t", header=T, comment.char = "#", stringsAsFactors = F)
areas <- read.csv("data/areas.csv", sep="\t", header = T, comment.char = "#", colClasses = c("character", "character", "character"), stringsAsFactors = F)



loc <- default_locale()
loc$decimal_mark <- ","
loc$encoding <- "latin1"

load_landings_areas <- function(filename, oldcolnames=F){
  if (oldcolnames){
    landings <- read_delim(filename, delim="|", locale = loc, trim_ws=TRUE, na=c("", "na", "NA"), col_names = names(colt$cols), col_types = colt, skip=1)
  }
  else{
    landings <- read_delim(filename, delim="|", locale = loc, trim_ws=TRUE, na=c("", "na", "NA"), col_names = T, col_types = colt)  
  }
  land_all_areas <- landings[landings$`Hovedområde (kode)` %in% unique(areas$FDIRarea),]
  land_all_areas <- land_all_areas[land_all_areas$`Redskap (kode)`!=90,]#oppdrett
  return(land_all_areas)
}

landings <- list()
landings[["2013"]] <- load_landings_areas("data/2013.lss", T)
landings[["2014"]] <- load_landings_areas("data/2014.lss", T)
landings[["2015"]] <- load_landings_areas("data/2015.lss", T)
landings[["2016"]] <- load_landings_areas("data/2016.lss", T)
landings[["2018"]] <- load_landings_areas("data//FDIR_HI_LSS_FANGST_2018_PR_2019-04-02.psv")


plot_landings_bms_timeseries <- function(faospecies="WIT"){
  
  frac <- c()
  year <- c()
  for (l in names(landings)){
    yl <- landings[[l]]
    specland<-yl[yl$`Art FAO (kode)`==faospecies,]
    specland$huc <- specland$`Anvendelse hovedgruppe (kode)`==1
    a <- aggregate(list(tot=specland$Rundvekt), list(usage=specland$huc), sum)
    a <- a[order(a$usage),]
    a$usage <- c("BMS","Landing")
    frac <- c(frac, a$tot[1]/(a$tot[1]+a$tot[2]))
    year <- c(year, l)
  }
  
  barplot(frac, names.arg = year, ylab="fraction BMS", main=faospecies)

}



wit_by_usage <- aggregate(list(tot=wit$Rundvekt), list(anv=wit$`Anvendelse hovedgruppe`), sum)
wit_nonhuc <- wit[wit$`Anvendelse hovedgruppe (kode)`!=1,]
wit_nonhuc_by_vessel<-aggregate(list(weight=wit_nonhuc$Rundvekt), list(vessel=wit_nonhuc$`Registreringsmerke (seddel)`), sum)
wit_nonhuc_by_vessel <- wit_nonhuc_by_vessel[order(wit_nonhuc_by_vessel$weight, decreasing=T),]

wit_nonhuc_by_gear<-aggregate(list(weight=wit_nonhuc$Rundvekt), list(gear=wit_nonhuc$Redskap), sum)
wit_nonhuc_by_gear <- wit_nonhuc_by_gear[order(wit_nonhuc_by_gear$weight, decreasing=T),]
