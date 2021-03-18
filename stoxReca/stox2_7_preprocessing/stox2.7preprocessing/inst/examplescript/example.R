library(stox2.7preprocessing)

message("Read landings ...")
originalLandings <- readLandings("~/workspace/stox/ECA_prosjekter/Kysttorsk_AFWG_2018/input/landing/torsk_2018_2020_03_23.xml")
message("Read logbooks ...")
logbooks <- RstoxData::readErsFile("~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv")

# add in conversionfactor adjustment when finished

message("Encode coastal cod area ...")
areaEncodedLandings <- encodeCostalCodArea(originalLandings)
message("Adjust landings with logbooks for Trawls (gear 50-59)...")
adjustedLandings <- adjustWithLogbook(areaEncodedLandings, logbooks, speciesFAO = "COD", gearCodes = 50:59)
message("Re-Encode coastal cod area based on positions...")
adjustedLandings <- encodeCostalCodArea(adjustedLandings)

comparison <- compareLandings(areaEncodedLandings, adjustedLandings)

message(paste("Total original landings:", sum(originalLandings$Rundvekt), "Total adjusted landings: ", sum(adjustedLandings$Rundvekt), "Difference:", sum(originalLandings$Rundvekt) - sum(adjustedLandings$Rundvekt)))
message(paste("Sum of absolute differences for gear/area combinations for trawls (gear 50-59):", format(sum(abs(comparison$difference[comparison$Redskap_kode %in% 50:59])), digits=2)))
message(paste("Sum of absolute differences for gear/area combinations for non-trawls (gear not 50-59):", format(sum(abs(comparison$difference[!(comparison$Redskap_kode %in% 50:59)])), digits = 2)))

message(paste("Relative difference in coastal-cod region after adjustment:", format(sum(comparison$difference[startsWith(comparison$Hovedområde_kode, "s")])/sum(comparison$original.weight[startsWith(comparison$Hovedområde_kode, "s")]),digits=2)))


comparisonArea <- compareLandings(areaEncodedLandings, adjustedLandings, includeGear=F)
message(paste("Change in coastal cod aras within 12 nm:",
              format(sum(comparisonArea$difference[comparisonArea$Hovedområde_kode %in% c("s000", "s301", "s401", "s501", "s601", "s701")]), digits=1), "kg"))
message(paste("Change in coastal cod aras outside 12 nm:",
              format(sum(comparisonArea$difference[comparisonArea$Hovedområde_kode %in% c("s300", "s400", "s500", "s600", "s700")]), digits=1), "kg"))

message("NB: Keeping only coastal cod areas.")
coastalCodAdjustedLandings <- adjustedLandings[adjustedLandings$Hovedområde_kode %in% c("s300", "s400", "s500", "s600", "s700", "s000", "s301", "s401", "s501", "s601", "s701"),]

message("Write adjusted landings. May take some hours.")
writeStox27LandingXML("~/temp/coastalCodAdjustedLandings2018.xml", coastalCodAdjustedLandings)
