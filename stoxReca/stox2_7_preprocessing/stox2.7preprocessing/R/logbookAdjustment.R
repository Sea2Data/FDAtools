#' Adjust landings with logbooks
#' @description
#'  Adjust landings with information from logbooks for a specific species.
#' @details
#'  Logbooks contain catch reports for each fishing operation as defined in legislation for different gears.
#'  Parameters like catch date and area are defined in landings (sales notes) by a summary value for each trip.
#'  E.g. the dominant area for the trip (the area with more catch), and the date of the last catch of the trip.
#'  Applying this function alters the definition of some of these parameters to be resolved to fishing operation
#'  when information is available from logbook records.
#'  The information from logbooks are not simply copied over, but the landed weight is redistributed
#'  to reflect the partitioning of weights reported in logbooks
#'  In addition positions are added when logbook/VMS entries are available
#'  (columns FOlat and FOlon are added to landings for latitdue and longitude of fishing operation, respectively).
#'
#'  In general, the difference in definition between logbooks and landing are less likely to be relevant
#'  for shorter trips. Trip length potential is limited by vessel size.
#'  Logbook records was first introduced for trawlers, and later for all vessels larger than or equal to 15 m.
#'  Selection for certain gears or vessel sizes is supported by the parameters 'gearCodes' and 'vesselSize'
#'
#'  Logbook records may contain records of activities other than fishing. Some of them may relate to
#'  activity that does not consitute removal of fish from the sea (such as transfer of fish between vessels).
#'  Selection of activities to include is supported by the parameter 'activities'. Catch is occationally
#'  recorded for operations which do not handle fish (such as STE for steaming or SET for setting of gear), these
#'  are likely misrecorded and should possibly be included. Codes for transport and production (HAU and PRO) may
#'  indicate processing and transport of the catch weight and should probably be excluded. By default only
#'  codes for fishing (FIS), harvesting from another ships gear (REL) and research-catches (SCR) are included.
#'
#'  A trip may be reflected in several sales notes. This function distributes the landed
#'  weight for each of them on the temporal and spatial spatial variables.
#'  That is: each sales-note line with a corresponding logbook entry is replaced by
#'  a set of sale-note lines, one for each fishing operation in the corresponding trip.
#'  This is done regardless of parameters that may be specific to certain fishing operations,
#'  such as gear, and it is done without attempt to make a realistic partitioning of
#'  parameters determined at landing, such as usage.
#'
#'  trips are deduced by vessel identifiers (radio call-signal) and dates (date of last catch
#'  in landings, and date of fishing operation in logbooks). Consisteecy of gears reported
#'  between logboks and landings are not enforced.
#'
#'  Only codes are changed. Corresponding description columns are not. E.g.:
#'  Area code (Hovedområde_kode) may reflect fishing operation after logbook-adustments,
#'  while area description (Hovedområde_bokmål) will reflect dominant area for trip.
#'
#' @param landings landings as parsed by \code{\link[stox2.7preprocessing]{readLandings}}
#' @param logbooks logbooks as parsed by \code{\link[RstoxData]{readErsFile}}
#' @param speciesFAO 3-alpha FAO species code, e.g. "COD"
#' @param gearCodes Gear codes (NS9400) that should be fetched from logbooks. If null, all available gears are included.
#' @param vesselSize smallest vessel size to include from logbooks. If null, all availalbe vessels sizes are included.
#' @param activities activity-types to include from logbooks. If null, all available activites are included.
#' @return landings with columns FOlat and FOlon added, and adjusted definitions for columns:
#' \describe{
#'  \item{Hovedområde_kode}{dominant area for trip OR area of fishing operation}
#'  \item{Lokasjon_kode}{dominant location (sub area) OR location of fishing operation}
#'  \item{SisteFangstdato}{date of last catch OR date of fishing operation}
#'  \item{Rundvekt}{live weight (round weight) listed in sales-note OR weight ascribed to fishing operation}
#'  \item{Produktvekt}{product weight listed in sales-note OR weight ascribed to fishing operation}
#'  \item{Bruttovekt}{gross weight of landed fish listed in sales-note OR weight ascribed to fishing operation}
#'  Definitions depend on whether logbook information was available for the sales-note.
#' }
#' @export
adjustWithLogbook <- function(landings, logbooks, speciesFAO, gearCodes=NULL, vesselSize=NULL, activities=c("FIS", "REL", "SCR")){
  logbooks <- logbooks[logbooks$FANGSTART_FAO %in% speciesFAO,]
  if (nrow(logbooks)==0){
    stop(paste("No logbook records found for species:", paste(speciesFAO, collapse=",")))
  }
  if (!is.null(gearCodes)){
    logbooks <- logbooks[logbooks$REDSKAP_NS %in% gearCodes,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }
  if (!is.null(vesselSize)){
    logbooks <- logbooks[logbooks$STØRSTE_LENGDE >= vesselSize,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }
  if (!is.null(activities)){
    logbooks <- logbooks[logbooks$AKTIVITET_KODE %in% activities,]
    if (nrow(logbooks)==0){
      stop(paste("No logbook records found that match criteria."))
    }
  }

  landings$lastCatch <- as.POSIXct(landings$SisteFangstdato, format="%d.%m.%Y")

  tripids <- RstoxFDA::makeTripIds(landings, vesselIdCol = "Radiokallesignal_seddel", lastCatchCol = "lastCatch")
  logbooks <- suppressWarnings(RstoxFDA::appendTripIdLogbooks(logbooks, tripids, vesselIdCol = "RC", timeCol ="STARTTIDSPUNKT"))
  logbooks <- logbooks[!is.na(logbooks$tripid),]
  logbooks$FOarea <- substr(logbooks$LOKASJON_START, 1, 2)
  logbooks$FOloc <- substr(logbooks$LOKASJON_START, 3, 4)
  logbooks$FOlat <- logbooks$START_LT
  logbooks$FOlon <- logbooks$START_LG
  logbooks$FOtime <- logbooks$STARTTIDSPUNKT

  logbooks <- logbooks[!is.na(logbooks$FOarea) & !is.na(logbooks$FOloc) & !is.na(logbooks$FOlat) & !is.na(logbooks$FOlon) & !is.na(logbooks$FOtime) & !is.na(logbooks$FANGSTART_FAO),]
  if (nrow(logbooks)==0){
    stop(paste("No logbook records found that match criteria, and have all necessary information."))
  }

  # calculate fraction of trip catch of species for each haul
  # add fishin operation parameters (FO)
  tripartitions <- RstoxFDA::calculateLogbookPartitionByTrip(logbooks, groupCols = c("FOtime", "FOarea", "FOloc", "FOlat", "FOlon", "FANGSTART_FAO"))
  tripartitions <- merge(tripartitions$fractions, tripartitions$groupDefinition)
  tripartitions$FOlat <- as.numeric(tripartitions$FOlat)
  tripartitions$FOlon <- as.numeric(tripartitions$FOlon)

  landings <- RstoxFDA::appendTripIdLandings(landings, tripIds = tripids, vesselIdCol = "Radiokallesignal_seddel", lastCatchCol = "lastCatch")
  hasLogb <- landings[!is.na(landings$tripid) & landings$tripid %in% tripartitions$tripid &
                        !is.na(landings$ArtFAO_kode) &
                        landings$ArtFAO_kode == speciesFAO &
                        landings$Redskap_kode %in% gearCodes,] #last clause necessary because of some inconsistency in gear coding between logbooks and landings
  rest <- landings[!(!is.na(landings$tripid) & landings$tripid %in% tripartitions$tripid &
                       !is.na(landings$ArtFAO_kode) &
                       landings$ArtFAO_kode == speciesFAO &
                       landings$Redskap_kode %in% gearCodes),]
  rest$FOlon <- as.numeric(NA)
  rest$FOlat <- as.numeric(NA)

  hasLogb <- merge(hasLogb, tripartitions, by.x=c("tripid", "ArtFAO_kode"), by.y=c("tripid", "FANGSTART_FAO"), all.x=T, allow.cartesian=T)
  hasLogb$Rundvekt <- hasLogb$Rundvekt*hasLogb$fraction
  hasLogb$Bruttovekt <- hasLogb$Bruttovekt*hasLogb$fraction
  hasLogb$Produktvekt <- hasLogb$Produktvekt*hasLogb$fraction
  hasLogb$Hovedområde_kode <- hasLogb$FOarea
  hasLogb$Lokasjon_kode <- hasLogb$FOloc
  hasLogb$SisteFangstdato <- strftime(hasLogb$FOtime, format="%d.%m.%Y")
  hasLogb <- hasLogb[,names(hasLogb) %in% names(rest), with=F]

  landings <- rbind(rest, hasLogb)
  return(landings)
}
