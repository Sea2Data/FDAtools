library(stox2.7preprocessing)

fileName <- "~/temp/torsk_adjusted_coastalCod_2020_mini.xml"
logbooks <- "~/logbooks/FDIR_HI_ERS_2020_PR_2021-03-03.psv"
landings <- "~/landingsets/xml/filtered/torsk_2020.xml"

if (file.exists(fileName)){
  stop(paste("The file", fileName, "already exists."))
}

message("Read landings ...")
originalLandings <- readLandings(landings)
message("Read logbooks ...")
logbooks <- RstoxData::readErsFile(logbooks)

#
# remove columns with troublesome encoding.
# Some landings file have encoding not consistent with the XML declaration
# provide more permanent fix later
#

if (!is.null(originalLandings$Fiskerkommune)){
  originalLandings$Fiskerkommune <- as.character(NA)
}
if (!is.null(originalLandings[["Fart\u00F8ykommune"]])){
  originalLandings[["Fart\u00F8ykommune"]] <- as.character(NA)
}
if (!is.null(originalLandings$Landingskommune)){
  originalLandings$Landingskommune <- as.character(NA)
}
if (!is.null(originalLandings$Produksjonskommune)){
  originalLandings$Produksjonskommune <- as.character(NA)
}

message("Encode coastal cod area ...")
areaEncodedLandings <- encodeCostalCodArea(originalLandings)

adjustedLandings <- areaEncodedLandings

factorSLUH=1.671
factorSLMH=1.311
message("Applying seasonal conversion factors to NOR vessels < 28 m, north of 62N for January through April.")
message(paste("Applying seasonal conversion factor", factorSLUH, "for gutted fish without head."))
adjustedLandings <- adjustConversionFactor(adjustedLandings, factorSLUH, c(211, 214))
message(paste("Applying seasonal conversion factor", factorSLMH, "for gutted fish with head."))
adjustedLandings <- adjustConversionFactor(adjustedLandings, factorSLMH, 210)


message("Adjust landings with logbooks for Trawls (gear 50-52)...")
adjustedLandings <- adjustWithLogbook(adjustedLandings, logbooks, speciesFAO = "COD", gearCodes = 50:52)
message("Re-Encode coastal cod area based on positions...")
adjustedLandings <- encodeCostalCodArea(adjustedLandings)

comparison <- compareLandings(areaEncodedLandings, adjustedLandings)

message(paste("Total original landings:", sum(originalLandings$Rundvekt, na.rm=T), "Total adjusted landings: ", sum(adjustedLandings$Rundvekt, na.rm=T), "Difference:", sum(originalLandings$Rundvekt, na.rm=T) - sum(adjustedLandings$Rundvekt, na.rm=T)))
message(paste("Sum of absolute differences for gear/area combinations for trawls (gear 50-52):", format(sum(abs(comparison$difference[comparison$Redskap_kode %in% 50:52])), digits=2)))
message(paste("Sum of absolute differences for gear/area combinations for non-trawls (gear not 50-52):", format(sum(abs(comparison$difference[!(comparison$Redskap_kode %in% 50:52)])), digits = 2)))

message(paste("Relative difference in coastal-cod region after adjustment:", format(sum(comparison$difference[startsWith(comparison[["Hovedomr\u00E5de_kode"]], "s")])/sum(comparison$original.weight[startsWith(comparison[["Hovedomr\u00E5de_kode"]], "s")]),digits=2)))


comparisonArea <- compareLandings(areaEncodedLandings, adjustedLandings, includeGear=F)
message(paste("Change in coastal cod aras within 12 nm:",
              format(sum(comparisonArea$difference[comparisonArea[["Hovedomr\u00E5de_kode"]] %in% c("s000", "s301", "s401", "s501", "s601", "s701")]), digits=1), "kg"))
message(paste("Change in coastal cod aras outside 12 nm:",
              format(sum(comparisonArea$difference[comparisonArea[["Hovedomr\u00E5de_kode"]] %in% c("s300", "s400", "s500", "s600", "s700")]), digits=1), "kg"))

#
# fix keys (linjenummer)
#

adjustedLandings$Linjenummer <- 1:nrow(adjustedLandings)

#
# filter
#



message("NB: Keeping only example areas, gear, and season.")
coastalAreaAdjustedLandings <- adjustedLandings[adjustedLandings[["Hovedomr\u00E5de_kode"]] %in% c("s400", "s500", "s401", "s501"),]
coastalAreaAdjustedLandings <- coastalAreaAdjustedLandings[coastalAreaAdjustedLandings$Redskap_kode %in% c(33,22,61),]
coastalAreaAdjustedLandings <- coastalAreaAdjustedLandings[substr(coastalAreaAdjustedLandings$SisteFangstdato,4,5) %in% c("01","02","03","04","05","06"),]


message("Write adjusted landings. ")
writeStox27LandingXML(fileName, coastalAreaAdjustedLandings)
