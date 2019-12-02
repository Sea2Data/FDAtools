library(readr)

#' function for reading LSS formatted landings file, latin1 encoded, '|' delimited columns with ',' as decimal separator, and headers
#' Detailed documentation for the format is found at ces: prosjekt/ressurs/mare/fiskstat/sluttseddel/sluttseddel_LSS/LSS_Dokumentasjon_prSept2016.pdf
#' Uses readr for faster parsing.
#' @param file path to LSS file
#' @param encoding character set used for encoding LSS file, defaults to latin1 / iso-8859-1
#' @param colum types
#' @return landings
read_psv <- function(file, encoding="latin1", col_types=NULL){
  loc <- default_locale()
  loc$decimal_mark <- ","
  loc$encoding <- encoding
  db <- read_delim(file, delim="|", col_names=T, trim_ws=TRUE, na=c("", "na", "NA"), locale=loc, col_types = col_types)
  return(db)
}

#' Plot temporal partitioning of landings
#' @param landings parsed and possibly filtered landings, assumes siste fangstdato formatted as Date.
#' @param species vector of species to retain 'Art (bokmål)'
#' @param main title for plot
plot_landings_vs_time <- function(landings, species, main){
  filteredlandings <- landings[landings$`Art (bokmål)` %in% species,]
  
  tab <- aggregate(list(weight=filteredlandings$Rundvekt), by=list(date=filteredlandings$`Siste fangstdato`), FUN=sum)
  
  plot(tab$date, tab$weight, main=main, xlab="date", ylab="weight (kg)")
}

plot_all_species_vs_time <- function(landings){
  
  for (s in unique(landings$`Art (bokmål)`)){
    plot_landings_vs_time(landings, c(s), s)
  }
}

#' Plots weight partitioning of selected species or species group by gear
#' @param landings parsed and possibly filtered landings
#' @param species vector of species to retain, as they appear in 'Art (bokmål)'
#' @param main title for plot
plot_weight_by_gear_for_species <- function(landings, species, main){
  filteredlandings <- landings[landings$`Art (bokmål)` %in% species,]
  tab <- aggregate(list(weight=filteredlandings$Rundvekt), list(gear=filteredlandings$`Hovedgruppe redskap (bokmål)`), FUN=sum)
  barplot(tab$weight, names=tab$gear, ylab="weight (kg)", main=main)
}



# ces/prosjekt is here mapped to /Volumes/prosjekt
#landingsfile = "/Volumes/prosjekt/ressurs/mare/fiskstat/sluttseddel/sluttseddel_LSS/mottatt2018/01_2018/FDIR_HI_LSS_FANGST_2017_PR_2018-01-09.psv"
landingsfile = "~/landingsets/LSS/2016.lss"
#parse file
landings = read_psv(landingsfile)

# format date of last catch as a date
landings$`Siste fangstdato` <- as.Date(landings$`Siste fangstdato`, format = "%d.%m.%Y")

#get rid of byproducts
landings <- landings[!is.na(landings$Rundvekt) & landings$Rundvekt>0,]

# filtering for defining fishery
areas <- c("28", "08")
upper_length <- 15
kyst_hav <- c(8)
main_gears <- c("Trål", "Line", "Garn")

landings <- landings[landings$`Hovedområde (kode)` %in% areas,]
landings <- landings[landings$`Største lengde`<upper_length,]
landings <- landings[landings$`Kyst/hav (kode)` %in% kyst_hav,]
landings <- landings[landings$`Hovedgruppe redskap (bokmål)` %in% main_gears,]

#list of species in this fishery (all seasons)
table(landings$`Art (bokmål)`)

#species groups of interest
sharks_and_rabbitfish <- c("Pigghå", "Håbrann", "Annen hai", "Havmus")
skates <- c("Storskate", "Annen skate og rokke", "Piggskate", "Spisskate")

#separate by geartype
trawl <- landings[landings$`Hovedgruppe redskap (bokmål)`=="Trål",]
gillnet <- landings[landings$`Hovedgruppe redskap (bokmål)`=="Garn",]
longline <- landings[landings$`Hovedgruppe redskap (bokmål)`=="Line",]

#overview of gear by species or species groups
plot_weight_by_gear_for_species(landings, c("Pigghå"), "Pigghå")
plot_weight_by_gear_for_species(landings, skates, "skates")

#overview of temproal distribution for landings by species or species groups
plot_landings_vs_time(trawl, sharks_and_rabbitfish, "Trawl, sharks and rf")
plot_landings_vs_time(gillnet, sharks_and_rabbitfish, "Gillnet, sharks and rf")
plot_landings_vs_time(longline, sharks_and_rabbitfish, "Longline, sharks and rf")
plot_landings_vs_time(trawl, skates, "Trawl, skates")
plot_landings_vs_time(gillnet, skates, "Gillnet, skates")
plot_landings_vs_time(longline, skates, "Longline, skates")

#note it is the combination of vessel and date of last catch that best defines a landing (not the number of rows in landings, even after filtering by species)
print(paste("Landings: ", nrow(unique(landings[,c("Registreringsmerke (seddel)", "Siste fangstdato")])), ", rows:", nrow(landings), sep = ""))

# example for filtering by time time categories
#
# make dataset for jan-feb
#
janfeb <- landings[landings$`Siste fangstdato`>=as.Date("2016-01-01") & landings$`Siste fangstdato`<as.Date("2016-03-01"),]
