# add logbook info, filter by gear


#' Adjust landings with logbooks
#' @description
#'  Adjust landings with information from logbooks for a specific species.
#' @details
#'  Logbooks contain catch report pr fishing operation as defined in legislation for different gears.
#'  Parameters like catch date and area are defined in landings (sales notes) by what value was dominant for the trip
#'  Applying this function alters the definition of some of these parameters to be resolved to fishin operation when information is available
#'  The information from logbooks are not simply copied over, but the landed weight is redistributed over
#'  to reflect the partitioning of weights reported in logbooks
#'  In addition positions are added when logbook entries are available (new columns FOlat and FOlon).
#'
#'  A trip may be reflected in several sales notes. This function distributes the landed
#'  weight for each of them on the temporal and spatial spatial variables.
#'  That is: each sales-note line with a corresponding logbook entry, is replaced by
#'  a set of sale-note lines, one for each fishing operation in the corresponding trip.
#'  This is done regardless of parameters that may be specific to certain fishing operations,
#'  such as gear, and it is done without attempt to make a realistic partitioning of
#'  parameters determined at landing, such as usage.
#'
#'  trips are deduced by vessel identifiers (radio call-signal) and dates (date of last catch
#'  in landings, and date of fishing operation in logbooks). Consistnecy of gears reported
#'  between logboks and landings are not enforced.
#'
#'  Only codes are changed. Corresponding description columns are not. E.g.:
#'  Area code (Hovedområde_kode) will be reflect fishing operation,
#'  while area description (Hovedområde_bokmål) will reflect dominant area for trip.
#' @param landings landings as parsed by \code{\link[stox2.7preprocessing]{readLandings}}
#' @param logbooks logbooks as parsed by \code{\link[RstoxData]{readErsFile}}
#' @param speciesFAO 3-alpha FAO species code, e.g. "COD"
#' @param gearCodes gear codes (NS9400) that should be fetched from logbooks. If null all gears are included.
adjustLogbook <- function(landings, logbooks, speciesFAO, gearCodes){
  logbooks <- logbooks[logbooks$FANGSTART_FAO %in% speciesFAO,]
  if (nrow(logbooks)==0){
    stop(paste("No logbook records found for species:", paste(speciesFAO, collapse=",")))
  }
  if (!is.null(gearCodes)){
    logbooks <- logbooks[logbooks$REDSKAP_NS %in% gearCodes,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found for species:", paste(speciesFAO, collapse=","), "and gear codes:", paste(gearCodes, collapse=",")))
    }
  }

  landings$lastCatch <- as.POSIXct(landings$SisteFangstdato, format="%d.%m.%Y")

  tripids <- RstoxFDA::makeTripIds(landings, vesselIdCol = "Radiokallesignal_seddel", lastCatchCol = "lastCatch")
  logbooks <- RstoxFDA::appendTripIdLogbooks(logbooks, tripids, vesselIdCol = "RC", timeCol ="STARTTIDSPUNKT")
  logbooks <- logbooks[!is.na(logbooks$tripid),]
  logbooks$FOarea <- substr(logbooks$LOKASJON_START, 1, 2)
  logbooks$FOloc <- substr(logbooks$LOKASJON_START, 3, 4)
  logbooks$FOlat <- logbooks$START_LT
  logbooks$FOlon <- logbooks$START_LG
  logbooks$FOtime <- logbooks$STARTTIDSPUNKT

  # calculate fraction of trip catch of species for each haul
  # add fishin operation parameters (FO)
  tripartitions <- RstoxFDA::calculateLogbookPartitionByTrip(logbooks, groupCols = c("FOtime", "FOarea", "FOloc", "FOlat", "FOlon", "FANGSTART_FAO"))
  tripartitions <- merge(tripartitions$fractions, tripartitions$groupDefinition)

  landings <- RstoxFDA::appendTripIdLandings(landings, tripIds = tripids, vesselIdCol = "Radiokallesignal_seddel", lastCatchCol = "lastCatch")
  hasLogb <- landings[!is.na(landings$tripid) & landings$tripid %in% tripartitions$tripid,]
  rest <- landings[is.na(landings$tripid) | !(landings$tripid %in% tripartitions$tripid),]
  rest$FOlon <- as.character(NA)
  rest$FOlat <- as.character(NA)

  hasLogb <- merge(hasLogb, tripartitions, by="tripid")
  hasLogb$Rundvekt <- hasLogb$Rundvekt*hasLogb$fraction
  hasLogb$Hovedområde_kode <- hasLogb$FOarea
  hasLogb$Lokasjon_kode <- hasLogb$FOloc
  hasLogb$SisteFangstdato <- strftime(hasLogb$FOtime, format="%d.%m.%Y")
  hasLogb <- hasLogb[,names(hasLogb) %in% names(rest), with=F]

  landings <- rbind(rest, hasLogb)
  return(landings)
}
