
#' annotate metier
#' @description 
#'  annotate fleet / metier based on landings (gear code). 
#'  Appropriate conversion tables must be constructed.
#' @param lsslandings lss landings, as parsed by \code{\link[RstoxData]{readLssFile}}
#' @param metierfile File mapping gear to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @param metiercolumn name of column to insert metiers into. The column 'Mottaksstasjon' is mapped to 'LandingSite' in StoxLandingData  (see \code{\link[RstoxData]{Stoxlanding}}).
#' @return 'landings' with columns 'metiers' inserted into the column provided by 'metiercolumn'.
annotateMetierFromLandings <- function(lsslandings, metierfile, metiercolumn="Mottaksstasjon"){
  metiertable=RstoxFDA::readMetierTable(metierfile)
  lsslandings <- RstoxFDA::appendMetier(lsslandings, metiertable, gearColumn = "Redskap (kode)", metierColName = "Fleet")
  lsslandings[[metiercolumn]] <- lsslandings$Fleet
  lsslandings$Fleet <- NULL
  return(as.data.frame(lsslandings))
}

#' annotate metier
#' @description 
#'  annotate fleet / metier based on gear code and mesh size. 
#'  Appropriate conversion tables must be constructed.
#' @details 
#' landings is data frame with landings as parsed by \code{\link[RstoxData]{readLssFile}}
#' with the mesh-size column appended (MASKEVIDDE). Mesh sizes can be appended from logbooks using
#' 'annotateMeshSize'.
#'  
#' The configuration in 'metierfileMeshed' will be used for all gears where mesh-size is available (MASKEVIDDE is provided).
#' The configuration may still specify the gear as unmeshed (and hence ignore mesh-size)
#' 
#' The configuration in 'metierfileUnmeshed' will be used for all gears where mesh-size is not available. 
#' This includes meshed gear with missing mesh-size information.
#'  
#' @param landings data frame with landings as parsed by \code{\link[RstoxData]{readLssFile}}
#' @param metierfileMeshed File mapping gear and mesh size to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @param metierfileUnmeshed File mapping gear to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @return 'landings' with columns 'metiers' inserted into the column provided by 'metiercolumn'.
annotateMetierMeshSize <- function(landings, metierfileMeshed, metierfileUnmeshed, metiercolumn="Mottaksstasjon"){
  
  if (!("MASKEVIDDE" %in% names(landings))){
    stop("The column MASKEVIDDE must be provided in 'stoxLandings'")
  }

  meshed <- landings[!is.na(landings$MASKEVIDDE),]
  unMeshed <- landings[is.na(landings$MASKEVIDDE),]
  
  metiertableMeshed <- RstoxFDA::readMetierTable(metierfileMeshed)
  metiertableUnmeshed <- RstoxFDA::readMetierTable(metierfileUnmeshed)
  
  meshed <- appendMetier(meshed, metiertableMeshed, gearColumn = "Redskap (kode)", meshSizeColumn = "MASKEVIDDE", metierColName = "Fleet")
  meshed[[metiercolumn]] <- meshed$Fleet
  meshed$Fleet <- NULL
  unMeshed <- appendMetier(unMeshed, metiertableUnmeshed, gearColumn = "Redskap (kode)", metierColName = "Fleet")
  unMeshed[[metiercolumn]] <- unMeshed$Fleet
  unMeshed$Fleet <- NULL
  
  landings <- rbind(meshed, unMeshed)
  
  return(landings)
}

#' Annotate mesh size
#' @description 
#'  Assigns trip ids and mesh sizes (MASKEVIDDE) from logbooks
#'  Only landings with corresponding trips in logbooks are annotated with mesh size. The other will have NA for this parameter.
#'  Only one mesh size is extracted for each trip (even when more mesh sizes have been used.)
#' @param landings data frame with landings as parsed by \code{\link[RstoxData]{readLssFile}}
#' @param logbooks
#' @return landings with the columns 'tripd' and 'MASKVEIDDE' added
annotateMeshSize <- function(landings, logbooks){

  if (length(unique(landings$`Art FAO (kode)`))!=1){
    stop("Excpects landings of one species only. Multiple codes found for Art FAO (kode)")
  }
    
  logb <- logbooks
  if (!any(!is.na(logb$FANGSTART_FAO) & (logb$FANGSTART_FAO %in% unique(landings$`Art FAO (kode)`)))){
    stop(paste("Species not found in logbooks:", paste(unique(landings$`Art FAO (kode)`), collapse=",")))
  }
  logb <- logb[!is.na(logb$FANGSTART_FAO) & (logb$FANGSTART_FAO %in% unique(landings$`Art FAO (kode)`)),]
  if (!any(logb$RC %in% unique(landings$`Radiokallesignal (seddel)`))){
    stop(paste("Vessels not found in logbooks:", paste(unique(logb$RC), collapse=",")))
  }
  logb <- logb[logb$RC %in% unique(landings$`Radiokallesignal (seddel)`),]
  logb <- logb[as.integer(substring(logb$LOKASJON_START,1,2)) %in% as.integer(landings$`Hovedområde (kode)`),]
  
  if (nrow(logb)==0){
    stop("No logbook entries matched criteria")
  }
  
  message("Annotating mesh size")
  tripIds <- RstoxFDA::makeTripIds(landings, vesselIdCol="Radiokallesignal (seddel)", lastCatchCol = "Siste fangstdato")
  landings <- RstoxFDA::appendTripIdLandings(landings, tripIds = tripIds, vesselIdCol="Radiokallesignal (seddel)", lastCatchCol = "Siste fangstdato")
  suppressWarnings(logb <- RstoxFDA::appendTripIdLogbooks(logb, tripIds, vesselIdCol = "RC", timeCol = "STARTTIDSPUNKT"))
  
  if (any(is.na(logb$tripid))){
    vessels <- length(unique(logb$RC[is.na(logb$tripid)]))
    message(paste(sum(is.na(logb$tripid)), " logbook catches (out of ", nrow(logb), ") from ", vessels, " vessels (out of ", length(unique(logb$RC)), ") could not be matched to landings.", sep=""))
  }
  
  #extract one mesh size pr gear and trip
  meshsizes <- logb[,c("tripid", "REDSKAP_NS", "MASKEVIDDE")]
  meshsizes <- meshsizes[!duplicated(paste(meshsizes$tripid, meshsizes$REDSKAP_NS)),]
  
  landings <- merge(landings, meshsizes, by.x=c("tripid", "Redskap (kode)"), by.y=c("tripid", "REDSKAP_NS"), all.x=T)
  
  return(landings)
  
}


annotateMetier <- function(landings, metierUnMeshed="./metiertable_unmeshed.txt", metierMeshed=NULL, logbooks=NULL, metiercolumn="Mottaksstasjon"){
  if (is.null(logbooks)){
    return(annotateMetierFromLandings(landings))
  }
  
  if (is.null(metierMeshed)){
    stop("Must provide metierMeshed when logbooks are provided")
  }
  
  landings <- annotateMeshSize(landings, logbooks)
  landings <- annotateMetierMeshSize(landings, metierMeshed, metierUnMeshed, metiercolumn)
  
  landings$tripid <- NULL
  landings$MASKEVIDDE <- NULL
  
  return(landings)
}

saveLandingsLSS <- function(landings, filename){
  stop("Implement properlyy")
  data.table::fwrite(landings, filename, sep="|")
}

saveLandingsXML <- function(landings, filename){
  ll <- RstoxData::convertToLandingData(landings)
  if (is.null(ll$ConvertedData$metadata)){
    ll$ConvertedData$metadata <- list()
    ll$ConvertedData$metadata$useXsd="landingerv2"
  }
  RstoxData:::WriteLanding(ll, filename)
}

#read landings
landings <- RstoxData::readLssFile("~/landingsets/LSS/FDIR_HI_LSS_FANGST_2021_PR_2022-05-02.psv")
logbooks <- RstoxData::readErsFile("~/logbooks/ers_detaljert/FDIR_HI_ERS_2021_PR_2022-05-02.psv")

# filter
# it is not neceesary to filter completely. The specifics is handled in stox, but it makes it easier to annotate metiers if we get rid of some stuff
landings <- landings[landings$`Art FAO (kode)`=="POK",]
landings <- landings[landings$`Nord/sør for 62 grader nord`!="Sør for 62°N",]

# annotate metier into landingsite-column
landings <- annotateMetier(landings, "./metiertable_unmeshed.txt", "metiertable_meshed.txt", logbooks)
saveLandingsXML(landings, "test.xml")