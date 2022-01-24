# comapares logbook cleaning implementation in RstoxFDA with the one used in this package (st27p)


if (file.exists("~/temp/land_h_vasket.xml")){
  stop("Clean up temp files first: ~/temp/land_h_vasket.xml")
}
if (file.exists("~/temp/land_h.xml")){
  stop("Clean up temp files first: ~/temp/land_h.xml")
}

#read landings and logbooks and filter by species
land <- RstoxData::readLssFile("~/landingsets/LSS/FDIR_HI_LSS_FANGST_2021_PR_2022-01-04.psv")
logb <- RstoxData::readErsFile("~/logbooks/FDIR_HI_ERS_2021_PR_2021-12-02.psv")

land <- land[!is.na(land$`Art FAO (kode)`) & land$`Art FAO (kode)`=="COD",]
land <- land[!is.na(land$Rundvekt),]

#convert landings to format known by st27p
land_h <- RstoxData::convertToLandingData(land)
RstoxData:::WriteLanding(land_h, FileNames = "~/temp/land_h.xml", namespaces = "http://www.imr.no/formats/landinger/v2")
land_sp27 <- stox2.7preprocessing::readLandings("~/temp/land_h.xml")
vasket <- stox2.7preprocessing::adjustWithLogbook(land_sp27, logb, "COD", c(50:52))
stox2.7preprocessing::writeStox27LandingXML("~/temp/land_h_vasket.xml", vasket)

#clean with st27p routine
vasket_torsk <- RstoxData::readXmlFile("~/temp/land_h_vasket.xml")

#do same filtering as st27p
logb <- logb[!is.na(logb$LOKASJON_START),]
logb <- logb[!is.na(logb$START_LT),]
logb <- logb[!is.na(logb$START_LG),]
logb <- logb[!is.na(logb$STARTTIDSPUNKT),]
logb <- logb[!is.na(logb$RUNDVEKT),]
logb <- logb[!is.na(logb$FANGSTART_FAO),]
logb <- logb[logb$FANGSTART_FAO=="COD",]


#clean with RstoxFDA
landAdjPrefilt <- RstoxFDA:::logbookAdjustment(land, logb, gearCodes = c("50","51", "52"), polygons = NULL)
landAdj <- landAdjPrefilt

landAdjPrefiltWPos <- RstoxFDA:::logbookAdjustment(land, logb, gearCodes = c("50","51", "52"))
landAdjWPos <- landAdjPrefiltWPos


#convert to same format as vasket_torsk_comp
landAdjLD <- RstoxData::convertToLandingData(landAdj)

#compare
landAdjComp <- merge(landAdjLD$ConvertedData$Produkt, landAdjLD$ConvertedData$Fangstdata)
vasket_torsk_comp <- merge(vasket_torsk$Produkt, vasket_torsk$Fangstdata)
ss<-vasket_torsk_comp[,list(Rundvekt=sum(Rundvekt, na.rm=T)), by=list(omr=get("Hovedområde_kode"), redskap=get("Redskap_kode"))]
dd<-landAdjComp[,list(Rundvekt=sum(Rundvekt, na.rm=T)), by=list(omr=get("Hovedområde_kode"), redskap=get("Redskap_kode"))]
orig<-land[,list(Rundvekt=sum(Rundvekt, na.rm=T)), by=list(omr=get("Hovedområde (kode)"), redskap=get("Redskap (kode)"))]
comp <- merge(dd,ss, by=c("omr","redskap"), suffixes = c(".st27p", ".rstoxFDA"))
comp <- merge(comp,orig, by=c("omr","redskap"))
comp$difference.st27p <- comp$Rundvekt - comp$Rundvekt.st27p
comp$difference.rstoxFDA <- comp$Rundvekt - comp$Rundvekt.rstoxFDA
comp$difference <- comp$Rundvekt.rstoxFDA - comp$Rundvekt.st27p
comp$rdifference <- comp$difference / comp$Rundvekt.st27p
comp<-comp[order(abs(comp$difference), decreasing = T),]
