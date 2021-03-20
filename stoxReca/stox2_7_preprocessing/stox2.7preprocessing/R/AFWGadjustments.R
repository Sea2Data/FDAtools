#' Prepare AFWG cod
#' @description
#'  Prepares landings for AFWG cod as agreed on WKBarFar 2021.
#' @details
#'  This function adjust landings by applying seasonal conversion factors,
#'  logbook adjustment and recoding of Norwegain coast AFWG areas,
#'  as on WKBarFar 2021.
#'  Some control parameters are calculated and messaged.
#'
#'  The resulting adjusted landings may be used by Stox 2.7. In order to get the necessary
#'  resource files for using the coastal cod areas as stratas see documentation for
#'  \code{\link[stox2.7preprocessing]{coastalCodAreas}} and \code{\link[stox2.7preprocessing]{coastalCodAreasAFWG}}
#'
#'  To apply any of these adjustments indvidually
#'  consider the functions \code{\link[stox2.7preprocessing]{adjustConversionFactor}},
#'  \code{\link[stox2.7preprocessing]{adjustWithLogbook}} and
#'  \code{\link[stox2.7preprocessing]{encodeCostalCodArea}}
#'
#'  Default settings prepares coastal cod.
#'  File writing is quite slow and make take several hours. Filtering on areas may speed this up a little bit.
#'  Run with 'coastalCod' TRUE to save data only for the Norwegain coast AFWG areas
#'  Run with 'coastalCod' FALSE to save data only for all other areas (note that further filtering will be required in Stox to get the AFWG area)
#'  Running with parameter seasonalConversionFactor=FALSE data without seasonal conversion factors.
#' @param fileName filename where adjusted landings in XML format will be saved
#' @param landings input: landings in XML format
#' @param logbooks logbooks in PSV format.
#' @param seasonalConversionFactor if TRUE seasonal conversion factors are applied for gutted fish
#' @param coastalCod if TRUE only the Norwegain coast AFWG areas are saved, otherwise all other areas are saved.
#' @export
processLandingsAllAdjustmentsAFWG <- function(fileName, landings, logbooks, seasonalConversionFactor=T, coastalCod=T){
  message("Read landings ...")
  originalLandings <- readLandings("~/workspace/stox/ECA_prosjekter/Kysttorsk_AFWG_2018/input/landing/torsk_2018_2020_03_23.xml")
  message("Read logbooks ...")
  logbooks <- RstoxData::readErsFile("~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv")

  if (seasonalConversionFactor){
    factorSLUH=1.671
    factorSLMH=1.311
    message("Applying seasonal conversion factors to NOR vessels < 28 m, north of 62N for January through April.")
    message(paste("Applying seasonal conversion factor", factorSLUH, "for gutted fish without head."))
    landings <- adjustConversionFactor(landings, factorSLUH, c(211, 214))
    message(paste("Applying seasonal conversion factor", factorSLMH, "for gutted fish with head."))
    landings <- adjustConversionFactor(landings, factorSLMH, 210)
  }

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

  message(paste("Relative difference in coastal-cod region after adjustment:", format(sum(comparison$difference[startsWith(comparison[["Hovedomr\u00E5de_kode"]], "s")])/sum(comparison$original.weight[startsWith(comparison[["Hovedomr\u00E5de_kode"]], "s")]),digits=2)))


  comparisonArea <- compareLandings(areaEncodedLandings, adjustedLandings, includeGear=F)
  message(paste("Change in coastal cod aras within 12 nm:",
                format(sum(comparisonArea$difference[comparisonArea[["Hovedomr\u00E5de_kode"]] %in% c("s000", "s301", "s401", "s501", "s601", "s701")]), digits=1), "kg"))
  message(paste("Change in coastal cod aras outside 12 nm:",
                format(sum(comparisonArea$difference[comparisonArea[["Hovedomr\u00E5de_kode"]] %in% c("s300", "s400", "s500", "s600", "s700")]), digits=1), "kg"))

  if (coastalCod){
    message("NB: Keeping only coastal cod areas.")
    coastalCodAdjustedLandings <- adjustedLandings[adjustedLandings[["Hovedomr\u00E5de_kode"]] %in% c("s300", "s400", "s500", "s600", "s700", "s000", "s301", "s401", "s501", "s601", "s701"),]
  }
  else{
    message("NB: Removing coastal cod areas.")
    coastalCodAdjustedLandings <- adjustedLandings[!(adjustedLandings[["Hovedomr\u00E5de_kode"]] %in% c("s300", "s400", "s500", "s600", "s700", "s000", "s301", "s401", "s501", "s601", "s701")),]
  }

  message("Write adjusted landings. May take some hours.")
  writeStox27LandingXML(fileName, coastalCodAdjustedLandings)
}
