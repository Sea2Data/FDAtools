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


landings <- readLssFile("data/FDIR_HI_LSS_FANGST_2019_PR_2020-03-03.psv")
landings <- landings[landings$`Fartøynasjonalitet (kode)`=="NOR",]

log <- Rstox::readErsFile("data/FDIR_HI_ERS_2019_PR_2020-03-04.psv")

#Finner ingen landinger for LEZ
sum(landings$`Art FAO (kode)` == "LEZ")
#ihht ASFIS skal LEZ være Lepidorhombus spp, i NMD taxa finner jeg glassvar og fireflekket var ved søk på Lepidorhombus
# Finner glassvar i landinger
nametab <- table(landings$`Art - FDIR`)
nametab[order(names(nametab))]
Lepidorhombus <- landings[landings$`Art - FDIR` == "Glassvar",]
# Disse er kodet som MEG som er riktig artskode. Rekoder disse i logbøker og landinger:
landings$`Art FAO (kode)`[landings$`Art FAO (kode)` == "MEG"] <- "LEZ"
log$FANGSTART_FAO[!is.na(log$FANGSTART_FAO) & log$FANGSTART_FAO == "MEG"] <- "LEZ"
sum(landings$`Art FAO (kode)` == "LEZ")

#Finner ingen landinger for ANF
sum(landings$`Art FAO (kode)` == "ANF")
#artlsiten til direktoratet fører også opp MON og ANG for breiflabber
sum(landings$`Art FAO (kode)` %in% c("ANF","MON","ANG"))
ikkeLophiidae <- landings[!(landings$`Art FAO (kode)` %in% c("ANF","MON","ANG")),]
#ser over om noe av de resterende burde klassifiseres som breiflabb
nametab <- table(ikkeLophiidae$`Art - FDIR`)
nametab[order(names(nametab))]
#rekoder breiflabb
landings$`Art FAO (kode)`[landings$`Art FAO (kode)` %in% c("ANF","MON","ANG")] <- "ANF"
log$FANGSTART_FAO[!is.na(log$FANGSTART_FAO) & log$FANGSTART_FAO %in% c("ANF","MON","ANG")] <- "ANF"
sum(landings$`Art FAO (kode)` == "ANF")

# COD og HAD pleier å være brukt konsistent, så jeg sjekker ikke det noe nærmere.


#
# reduserer logbøker og landinger til arter det skal rapporteres for
#
warning("Removing COD and HAD. Arved takes care of that")
landings <- landings[landings$`Art FAO (kode)` %in% c("ANF", "LEZ"),]
log <- log[log$FANGSTART_FAO %in% c("ANF", "LEZ"),]
