library(RstoxFDA)
library(RstoxData)
library(data.table)
library(Rstox)

#' checks if an value is among a set of options.
checkParam <- function(paramname, value, options){
  if (!(value %in% options)){
    stop(paste("Parameter", paramname, "must be one of", paste(options, collapse=","), ". Got:", value))
  }
}

#' checks if a value is unique
checkUnique <- function(paramname, values){
  if (length(unique(values))>1){
    stop(paste("paramname must be unique. Got:", paste(unique(values)), collapse=","))
  }
}

#' Extract landings
#' @description  
#'  Extract landings from StoX project. Format internal to StoX 2.7
#' @param stoxprojectname name of, or path to stox projected
#' @param force force re-run of stox project before exporting
#' @return data.frame with landings
extractLandings <- function(stoxprojectname, force=F){
  if (force){
    Rstox::runRScripts(stoxprojectname)
  }
  
  # load eca configuration and parameterization
  prepdata <- Rstox::loadProjectData(stoxprojectname, var = "prepareRECA")
  
  if (is.null(prepdata$prepareRECA)){
    stop("Results from Reca data preparation not found. Run with force=T to force re-running of stox project")
  }
  
  #POSIXct needed for data.table conversion
  stoxLandings <- prepdata$prepareRECA$StoxExport$landing
  stoxLandings$sistefangstdato <- as.POSIXct.POSIXlt(stoxLandings$sistefangstdato)
  return(stoxLandings)
}

#' annotate landings
#' @description annotates landings with intercatch fields that can be directly extracted from the data
#' @param stoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' @param seasonType the temporal resolution for the intercatc export, may be 'Month', 'Quarter' or 'Year'
#' @param country ISO 3166 2-alpha code for country submitting data
#' @param usageMap list relating intercatch codes for the field "Usage" to the column 'hovedgruppeanvendelsebokmål' in landings
#' @param unitCATON unit for landings, may be kg or t.
#' @return stoxLandings with the following added columns (corresponding to intercatch fields):
#'  \describe{
#'   \item{Country}{}
#'   \item{Year}{}
#'   \item{SeasonType}{}
#'   \item{Season}{}
#'   \item{Species}{}
#'   \item{CatchCategory}{}
#'   \item{ReportingCategory}{}
#'   \item{DataToFrom}{}
#'   \item{Usage}{}
#'   \item{UnitCATON}{}
#'   \item{CATON}{}
#'   \item{OffLandings}{}
#'  }
annotateFromLandings <- function(stoxLandings, seasonType="Quarter", country="NO", usageMap=list(H=c("Konsum"), I=c("Mel og olje", "Dyrefor/fiskefor, agn og annet")), unitCATON="t"){
  
  stoxLandings$Country <- country
  
  if (any(is.na(stoxLandings$sistefangstdato))){
    stop("Date of catch (sistefangstdato) missing for some landings.")
  }
  stoxLandings$Year <- substr(stoxLandings$sistefangstdato, 1,4)
  
  checkParam("seasonType", seasonType, c("Quarter", "Month", "Year"))
  stoxLandings$SeasonType <- seasonType
  
  checkParam("unitCATON", unitCATON, c("kg", "t"))
  
  #
  # annotate season
  #
  if (seasonType == "Quarter"){
    stoxLandings$Season <- substr(quarters(stoxLandings$sistefangstdato, T),2,2)
  }
  else if (seasonType == "Month"){
    stoxLandings$Season <- substr(stoxLandings$sistefangstdato, 6,7)
  }
  else if (seasonType == "Year"){
    stoxLandings$Season <- stoxLandings$year
  }
  else{
    #assert false
    stop("Error (seasonType)")
  }
  
  #
  # extract species code from data
  #
  
  stoxLandings$Species <- stoxLandings$artfaokode
  if (length(unique(stoxLandings$Species)) != 1){
    stop("Landings does not contain unique species code (artfaokode)")
  }
  
  stoxLandings$CatchCategory <- "L"
  stoxLandings$ReportingCategory <- "R"
  stoxLandings$DataToFrom <- "NA"
  
  missingUsage <- unique(stoxLandings$hovedgruppeanvendelsebokmål[!is.na(stoxLandings$hovedgruppeanvendelsebokmål) & !(stoxLandings$hovedgruppeanvendelsebokmål %in% c(usageMap$H, usageMap$I))])
  if (length(missingUsage) > 0){
    stop("usageMap not defined for all usage codes (hovedgruppeanvendelsebokmål). Missing:", paste(missing, collapse=","))
  }
  
  stoxLandings$Usage <- "NA"
  stoxLandings$Usage[!is.na(stoxLandings$hovedgruppeanvendelsebokmål) & stoxLandings$hovedgruppeanvendelsebokmål %in% usageMap$H] <- "H"
  stoxLandings$Usage[!is.na(stoxLandings$hovedgruppeanvendelsebokmål) & stoxLandings$hovedgruppeanvendelsebokmål %in% usageMap$I] <- "I"
  
  stoxLandings$UnitCATON <- unitCATON
  
  if (unitCATON == "kg"){
    stoxLandings$CATON <- stoxLandings$rundvekt    
  }
  else if (unitCATON == "t"){
    stoxLandings$CATON <- stoxLandings$rundvekt / 1000
  }
  else{
    stop("Error UnitCATON")
  }
  
  stoxLandings$OffLandings <- NA
  
  return(stoxLandings)
}

#' annotate metier
#' @description 
#'  annotate fleet / metier based on landings (gear code). 
#'  Appropriate conversion tables must be constructed.
#' @param stoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' @param metierfile File mapping gear to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @return 'stoxLandings' with columns 'Fleet' appended.
annotateMetierFromLandings <- function(stoxLandings, metierfile){
  metiertable=RstoxFDA::readMetierTable(metierfile)
  stoxLandings <- RstoxFDA::appendMetier(data.table::data.table(stoxLandings), metiertable, gearColumn = "redskapkode", metierColName = "Fleet")
  return(as.data.frame(stoxLandings))
}

#' annotate metier
#' @description 
#'  annotate fleet / metier based on gear code and mesh size. 
#'  Appropriate conversion tables must be constructed.
#' @details 
#' stoxLandings is data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' with the mesh-size column appended (MASKEVIDDE). Mesh sizes can be appended from logbooks using
#' 'annotateMeshSize'.
#'  
#' The configuration in 'metierfileMeshed' will be used for all gears where mesh-size is available (MASKEVIDDE is provided).
#' The configuration may still spesify the gear as unmeshed (and hence ignore mesh-size)
#' 
#' The configuration in 'metierfileUnmeshed' will be used for all gears where mesh-size is not available. 
#' This includes meshed gear with missing mesh-size information.
#'  
#' @param stoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' @param metierfileMeshed File mapping gear and mesh size to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @param metierfileUnmeshed File mapping gear to metier. See \code{\link[RstoxFDA]{readMetierTable}} for format.
#' @return 'stoxLandings' with columns 'Fleet' appended.
annotateMetierMeshSize <- function(stoxLandings, metierfileMeshed, metierfileUnmeshed){
  if (!("MASKEVIDDE" %in% names(stoxLandings))){
    stop("The column MASKEVIDDE must be provided in 'stoxLandings'")
  }
  
  stoxLandings <- data.table::data.table(stoxLandings)
  
  meshed <- stoxLandings[!is.na(stoxLandings$MASKEVIDDE),]
  unMeshed <- stoxLandings[is.na(stoxLandings$MASKEVIDDE),]
  
  metiertableMeshed <- RstoxFDA::readMetierTable(metierfileMeshed)
  metiertableUnmeshed <- RstoxFDA::readMetierTable(metierfileUnmeshed)
  
  meshed <- appendMetier(meshed, metiertableMeshed, gearColumn = "redskapkode", meshSizeColumn = "MASKEVIDDE", metierColName = "Fleet")
  unMeshed <- appendMetier(unMeshed, metiertableUnmeshed, gearColumn = "redskapkode", metierColName = "Fleet")
  
  stoxLandings <- rbind(meshed, unMeshed)
  
  stoxLandings <- as.data.frame(stoxLandings)
  return(stoxLandings)
}

#' annotate ICES areas
#' @description 
#'  Annotates ICES areas based on mainarea (as defined by Norwegian Directorate of Fisheries).
#'  Supports annotation of area types: SubAreas, Divisions, or SubDivisions, but not statistical rectangles.
#' @details 
#'  Positions for each landing is imputed based on proved main areas and corresponding ICES areas are annotated based on that position.
#'  The maximal available resolution is used, and the corresponding area types are deduced.
#'  If other area types than the maximal available resolution is desired, all SubAreas, Divisions, or SubDivisions to use must be noted in full area name notation in the parameter 'areas'.
#'  full area names has the form 27.3 for SubArea, 27.3.a for Division, or 27.3.a.20 for SubDivision.
#' @param stoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' @param areas character vector specifying the which SubAreas, Divisions, or SubDivisions to use. If NULL areas are encoded at the maximal available resolution.
#' @param mainareaPolygons polygons (\code{\link[sp]{SpatialPolygonsDataFrame}}) for main areas.
#' @param mainareaCol name of column in 'mainareaPolygons' that contain the main areas in standard notation (area 01-09 prefixed with 0).
#' @param ICESpolygons polygons (\code{\link[sp]{SpatialPolygonsDataFrame}}) for ICES areas.
#' @param ICESareaCol name of column in 'ICESpolygons' that contain the name of the polygons in full area name notation.
#' @return 'stoxLandings' with columns 'Area' and 'AreaType' appended.
annotateAreaFromLandings <- function(stoxLandings, areas=NULL, mainareaPolygons=RstoxFDA::mainareaFdir2018, mainareaCol="polygonName", ICESpolygons=RstoxFDA::ICESareas, ICESareaCol="Area_Full"){
  stoxLandings$areaCode <- sprintf("%02d", stoxLandings$hovedområdekode)
  stoxLandings <- RstoxFDA::appendPosition(stoxLandings, mainareaPolygons, "areaCode", latColName = "lat", lonColName = "lon", polygonName = mainareaCol)
  
  stoxLandings <- RstoxFDA::appendAreaCode(data.table::as.data.table(stoxLandings), areaPolygons = ICESpolygons, latName = "lat", lonName = "lon", colName = "Area", polygonName = "Area_Full")
  stoxLandings <- as.data.frame(stoxLandings)
  
  if (!is.null(areas)){
    for (a in areas){
      if (length(grep(a, areas))>1){
        stop("Overlapping areas specifed (parameter 'areas')")
      }
      stoxLandings$Area[grep(a, stoxLandings$Area)] <- a
    }
    
    if (!all(stoxLandings$Area %in% areas)){
      missing <- unique(stoxLandings$Area[!(stoxLandings$Area %in% areas)])
      stop(paste("The data contains more areas than those specified in parameter 'areas':", paste(missing, collapse=","), ". These have been removed. They may have been introduced by logbook adjustments, even if they are filtered in the stox project."))
    }
  }
  
  #deduce AreaType
  stoxLandings$AreaType <- as.character(NA)
  stoxLandings$AreaType[sapply(strsplit(stoxLandings$Area, "\\."), FUN=function(x){length(x)})==1] <- "AreaTop"
  stoxLandings$AreaType[sapply(strsplit(stoxLandings$Area, "\\."), FUN=function(x){length(x)})==2] <- "SubArea"
  stoxLandings$AreaType[sapply(strsplit(stoxLandings$Area, "\\."), FUN=function(x){length(x)})==3] <- "Div"
  stoxLandings$AreaType[sapply(strsplit(stoxLandings$Area, "\\."), FUN=function(x){length(x)})==4] <- "SubDiv"
  if (any(is.na(stoxLandings$AreaType))){
    stop("AreaType could not be deduced for all Areas.")
  }
  
  #remove temp columns
  stoxLandings$lon <- NULL
  stoxLandings$lat <- NULL
  stoxLandings$areaCode <- NULL
  
  return(stoxLandings)
}

#' Annotate mesh size
#' @description 
#'  Assigns trip ids and mesh sizes (MASKEVIDDE) from logbooks
#'  Only landings with corresponding trips in logbooks are annotated with mesh size. The other will have NA for this parameter.
#'  Only one mesh size is extracted for each trip (even when more mesh sizes have been used.)
#' @param stoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings')
#' @param logbooks
#' @return stoxLandings with the columns 'tripd' and 'MASKVEIDDE' added
annotateMeshSize <- function(stoxLandings, logbooks){
  logb <- RstoxData::readErsFile(logbooks)
  logb <- logb[!is.na(logb$FANGSTART_FAO) & (logb$FANGSTART_FAO %in% unique(stoxLandings$artfaokode)),]
  logb <- logb[logb$RC %in% unique(stoxLandings$radiokallesignalseddel),]
  logb <- logb[as.integer(substring(logb$LOKASJON_START,1,2)) %in% as.integer(stoxLandings$hovedområdekode),]
  
  stoxLandings <- data.table::data.table(stoxLandings)
  message("Annotating mesh size")
  tripIds <- RstoxFDA::makeTripIds(stoxLandings, vesselIdCol="radiokallesignalseddel", lastCatchCol = "sistefangstdato")
  stoxLandings <- RstoxFDA::appendTripIdLandings(stoxLandings, tripIds = tripIds, vesselIdCol="radiokallesignalseddel", lastCatchCol = "sistefangstdato")
  suppressWarnings(logb <- RstoxFDA::appendTripIdLogbooks(logb, tripIds, vesselIdCol = "RC", timeCol = "STARTTIDSPUNKT"))
  
  if (any(is.na(logb$tripid))){
    vessels <- length(unique(logb$RC[is.na(logb$tripid)]))
    message(paste(sum(is.na(logb$tripid)), " logbook catches (out of ", nrow(logb), ") from ", vessels, " vessels (out of ", length(unique(logb$RC)), ") could not be matched to landings.", sep=""))
  }
  
  #extract one mesh size pr gear and trip
  meshsizes <- logb[,c("tripid", "REDSKAP_NS", "MASKEVIDDE")]
  meshsizes <- meshsizes[!duplicated(paste(meshsizes$tripid, meshsizes$REDSKAP_NS)),]
  meshsizes$REDSKAP_NS <- as.integer(meshsizes$REDSKAP_NS)
  
  stoxLandings <- merge(stoxLandings, meshsizes, by.x=c("tripid", "redskapkode"), by.y=c("tripid", "REDSKAP_NS"), all.x=T)
  stoxLandings <- as.data.frame(stoxLandings)
  
  return(stoxLandings)

}

#' write HI line
#' @noRd
writeHI <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea,
                    DepthRange="NA", UnitEffort="NA", Effort="-9", AreaQualifier="NA"){
  writeLines(con=stream, paste("HI", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, UnitEffort, Effort, AreaQualifier, sep=","))
}

#' write SI line
#' @noRd
writeSI <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, Species, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin, UnitCATON, CATON, 
                    OffLandings=NA, varCATON="-9", DepthRange="NA", Stock="NA", QualityFlag="NA", InfoFleet="", InfoStockCoordinator="", InfoGeneral=""){
  
  if (is.na(OffLandings)){
    OffLandings <- "-9"
  }
  else{
    OffLandings <- format(OffLandings, digits=2)    
  }
  writeLines(con=stream, paste("SI", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, Species, Stock, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin, QualityFlag, UnitCATON, format(CATON, digits=2), OffLandings, varCATON, InfoFleet, InfoStockCoordinator, InfoGeneral, sep=","))
}

#' write SD line
#' @noRd
writeSD <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, Species, CatchCategory, ReportingCategory, Sex, CANUMtype, AgeLength, PlusGroup, unitMeanWeight, unitCANUM, UnitAgeOrLength, UnitMeanLength, Maturity, NumberCaught, MeanWeight, MeanLength, 
                    DepthRange="NA", Stock="NA",SampledCatch="-9", NumSamplesLngt="-9", NumLngtMeas="-9", NumSamplesAge="-9", NumAgeMeas="-9", varNumLanded="-9", varWgtLanded="-9", varLgtLanded="-9"){
  writeLines(con=stream, paste("SD", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, Species, Stock, CatchCategory, ReportingCategory, Sex, CANUMtype, AgeLength, PlusGroup, SampledCatch, NumSamplesLngt, NumLngtMeas, NumSamplesAge, NumAgeMeas, unitMeanWeight, unitCANUM, UnitAgeOrLength, UnitMeanLength, Maturity, format(NumberCaught, digits=4), format(MeanWeight,digits=2), format(MeanLength, digits=2), varNumLanded, varWgtLanded, varLgtLanded, sep=","))
}


#' Intercatch export
#' @description 
#'  Exports Catch at age estimate from StoX-Reca to intercatch (StoX 2.7).
#'  
#'  Requires some annotation of additional information. The function 'annotateFromLandings' takes care
#'  of annotations that can be directly extracted from the sales notes
#'  
#'  Area and metierannotation (fleet-annotation) may require additional resources depending on the requested detail.
#'  A minimal approach can be realised with the functions 'annotateAreaFromLandings' and 'annotateMetierFromLandings'.
#'  
#'  The function 'runExample' illustrates how annotation functions may be combined to produce an intercatch export.
#' @details
#'  Only single species export is supported.
#' @param stoxprojectname name of, or path to stox projected
#' @param annotatedStoxLandings data frame with landings as extracted from stox 2.7 (see 'extractLandings'), annotated with necessary columns for intercatch
#' @param exportfile file to write intercatch exchange format
#' @param samplesOrigin information of origin of samples for SI line. See intercatch exchange format SampleOrigin.
#' @param SDfleets fleets / metier that SD lines should be exported for. NULL means all fleets, NA no fleets.
#' @param plusGroup plus group for the SD lines (NULL means no plus group)
#' @param unitCANUM unit for catch at age in numbers, may be k,m or n for thosuands, millions or unit (ones) respectively
#' @param force force re-run of stox project before exporting
exportIntercatch <- function(stoxprojectname, annotatedStoxLandings, exportfile, samplesOrigin="U", SDfleets=NULL, plusGroup=NULL, unitCANUM="k", force=F){
  
  neededColumns <- c("Year", "Season", "Fleet", "Area","Country", "Species", "SeasonType", "AreaType", "CatchCategory",
                     "ReportingCategory", "DataToFrom", "Usage", "UnitCATON", "CATON", "OffLandings")
  missingColumns <- neededColumns[!(neededColumns %in% names(annotatedStoxLandings))]
  if (length(missingColumns) > 0){
    stop(paste("Some columns that are needed for intercatch export are not annotated on landings. Missing: "), paste(missingColumns, collapse=","))
  }
  
  if (is.null(SDfleets)){
    SDfleets <- unique(annotatedStoxLandings$Fleet)
  }
  
  missingFleets <- SDfleets[!is.na(SDfleets) & !(SDfleets %in% annotatedStoxLandings$Fleet)]
  if (length(missingFleets) > 0){
    stop(paste("Not all specified fleets / metiers found in landings. Missing:", paste(missingFleets, collaps=",")))
  }
  
  checkParam("unitCANUM", unitCANUM, c("k", "m", "n"))
  
  if (force){
    Rstox::runRScripts(stoxprojectname)
  }
  
  # load eca configuration and parameterization
  prepdata <- Rstox::loadProjectData(stoxprojectname, var = "prepareRECA")
  projecttempres <- prepdata$prepareRECA$StoxExport$temporalresolution
  rundata <- Rstox::loadProjectData(stoxprojectname, var = "runRECA")
  
  if (is.null(prepdata$prepareRECA)){
    stop("Results from Reca data preparation not found. Run with force=T to force re-running of stox project")
  }
  if (is.null(rundata$runRECA) & length(SDfleets) > 1){
    stop("Results from Reca parameterisation not found. Run with force=T to force re-running of stox project")
  }
  if (!is.null(rundata$runRECA) & rundata$runRECA$GlobalParameters$CC & length(SDfleets) > 1){
    stop("Intercatch export not implemented for stock splitting (coastal cod analysis)")
  }
  
  if (length(unique(annotatedStoxLandings$SeasonType)) != 1){
    stop("Intercatch export is only supported for data with unique season types.")
  }
  if (length(unique(annotatedStoxLandings$Species)) != 1){
    stop("Intercatch export is only supported for data with unique species.")
  }
  
  stream <- file(exportfile, open="w")
  
  for (year in unique(annotatedStoxLandings$Year)){ #exp 1 cat
    for (season in unique(annotatedStoxLandings$Season)){ #exp 4 cat
      for (fleet in unique(annotatedStoxLandings$Fleet)){ #exp many cat
        for (area in unique(annotatedStoxLandings$Area)){ #exp many cat
          data <- annotatedStoxLandings[  annotatedStoxLandings$Year == year &
                                            annotatedStoxLandings$Season == season &
                                            annotatedStoxLandings$Fleet == fleet &
                                            annotatedStoxLandings$Area == area,]
          # dont write lines for cells with no catch
          if (nrow(data) > 0){
            checkUnique("Country", data$Country)
            checkUnique("SeasonType", data$SeasonType)
            checkUnique("AreaType", data$AreaType)
            
            writeHI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area)                    
            
            
            for (catchCategory in unique(annotatedStoxLandings$CatchCategory)){ #exp 1 cat
              for (reportingCategory in unique(annotatedStoxLandings$ReportingCategory)){ #exp 1 cat
                for (dataToFrom in unique(annotatedStoxLandings$DataToFrom)){ #exp 1 cat
                  for (species in unique(data$Species)){
                    data <- annotatedStoxLandings[annotatedStoxLandings$CatchCategory == catchCategory &
                                                    annotatedStoxLandings$ReportingCategory == reportingCategory &
                                                    annotatedStoxLandings$DataToFrom == dataToFrom &
                                                    annotatedStoxLandings$Year == year &
                                                    annotatedStoxLandings$Season == season &
                                                    annotatedStoxLandings$Fleet == fleet &
                                                    annotatedStoxLandings$Area == area &
                                                    annotatedStoxLandings$Species == species,,]
                    checkUnique("UnitCATON", data$UnitCATON)
                    
                    #intercatch does not allow multiple usages within the variables filtered for above.
                    #extract most common
                    tab <- aggregate(list(w=data$rundvekt), by=list(usage=data$Usage), FUN=function(x){sum(x, na.rm=T)})
                    tab <- tab[order(tab$w, decreasing = T),]
                    usage <- tab[1,"usage"]
                    
                    if (!(fleet %in% SDfleets) & nrow(data)>0){
                      writeSI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, DataToFrom = dataToFrom, Usage = usage, SamplesOrigin = "NA", UnitCATON = data$UnitCATON[1], CATON = sum(data$CATON, na.rm=T), OffLandings = sum(data$OffLandings))
                    }
                    else if ((fleet %in% SDfleets) & nrow(data)>0){
                      message(paste("Predicting catch at age for", paste(data$Year[1], data$Season[1], data$Fleet[1], data$Area[1], collapse=",")))
                      #
                      # run prediction for cell
                      #
                      AgeLength <- prepdata$prepareRECA$AgeLength
                      WeightLength <- prepdata$prepareRECA$WeightLength
                      GlobalParameters <- rundata$runRECA$GlobalParameters
                      
                      decompLandings <- Rstox:::getLandings(data, AgeLength, WeightLength, projecttempres)
                      pred <- Reca::eca.predict(AgeLength, WeightLength, decompLandings, GlobalParameters)

                      if (unitCANUM == "k"){
                        unit <- "thousands"
                      } 
                      else if (unitCANUM == "m"){
                        unit <- "millions"
                      }
                      else if (unitCANUM == "n"){
                        unit <- "ones"
                      }
                      else{
                        stop("Error: unitCANUM")
                      }
                      
                      ageMat <- Rstox:::getCatchMatrix(pred, plusgr = plusGroup, var = "Abundance", unit = unit)
                      ageGroupPar <- Rstox:::getAgeGroupParamaters(pred, plusgr = plusGroup)
                      
                      #format plusgroup for report
                      plg <- "-9"
                      if (!is.null(plusGroup)){
                        plg <- plusGroup
                      }
                      
                      writeSI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, DataToFrom = dataToFrom, Usage = usage, SamplesOrigin = samplesOrigin, UnitCATON = data$UnitCATON[1], CATON = sum(data$CATON), OffLandings = sum(data$OffLandings))
                      for (age in ageMat$means$age){
                        lowerage <- gsub("\\+", "", age) #remove plus sign from plus group
                        caa <- ageMat$means$mean[ageMat$means$age==age]
                        meanW <- ageGroupPar$meanWeightG[ageGroupPar$age==age]
                        meanL <- ageGroupPar$meanLengthCm[ageGroupPar$age==age]
                        
                        #Sex is mandatory in the sense that the field must be filled (but accepts N=indetermined). Intercatch doc says its not mandatory
                        writeSD(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, 
                                Sex = "N", CANUMtype="Age", AgeLength = lowerage, PlusGroup=plg, unitMeanWeight="g", unitCANUM=unitCANUM, UnitAgeOrLength="year", UnitMeanLength="cm", Maturity="NA", NumberCaught=caa, MeanWeight=meanW, MeanLength=meanL)
                      }
                    }
                  }            
                }
              }
            } 
          }
        }
      }
    }
  }
  close(stream)
}

#' Compare StoX and intercatch
#' @description 
#'  Reads data from stox project and compare it with data exported for intercatch
#' @param stoxprojectname name of or path to StoX-Reca project
#' @param intercatchfile path to file with data in intercatch exchange format
checks <- function(stoxprojectname, intercatchfile){
  landings <- extractLandings(stoxprojectname)
  intercatchdata <- RstoxData::parseInterCatch(intercatchfile)
  
  #compare species
  cat(paste("Species StoX-Reca:", paste(unique(landings$artfaokode), collapse=","), "\n"))
  cat(paste("Species intercatch (IC):", paste(unique(intercatchdata$SI$Species), collapse=","), "\n"))
  
  #compare total weights
  sis <- intercatchdata$SI
  sis$CATON[sis$UnitCATON=="kg"] <- sis$CATON[sis$UnitCATON=="kg"]/1000
  
  totstox <- sum(landings$rundvekt)/1000
  totIC <- sum(sis$CATON)
  cat("\n")
  cat(paste("Totalvekt StoX-Reca (t):", totstox, "\n"))
  cat(paste("Totalvekt IC (t):", totIC, "\n"))
  diff <- totstox - totIC
  reldiff <- diff / totstox
  cat(paste("Difference: ", format(diff, digits=2), " t (", format(reldiff*100, digits=1), "%)\n", sep=""))
  
  #compare sum of products
  SISD <- merge(intercatchdata$SI, intercatchdata$SD)
  SISD$SIid <- paste(SISD$Country, SISD$Year, SISD$SeasonType, SISD$Season, SISD$Fleet, SISD$AreaType, SISD$FishingArea, SISD$DepthRange, SISD$Species, SISD$Stock, SISD$CatchCategory, SISD$ReportingCategory, SISD$DataToFrom, sep="-")
  
  SISD$NumberCaught[SISD$unitCANUM=="k"] <- SISD$NumberCaught[SISD$unitCANUM=="k"]*1000
  SISD$NumberCaught[SISD$unitCANUM=="m"] <- SISD$NumberCaught[SISD$unitCANUM=="m"]*1000*1000
  
  SISD$MeanWeight[SISD$unitMeanWeight=="g"] <- SISD$MeanWeight[SISD$unitMeanWeight=="g"]/1000
  SOP <- sum(SISD$NumberCaught*SISD$MeanWeight)
  SOPt <- SOP/1000
  total <- sum(SISD$CATON[!duplicated(SISD$SIid)])
  
  diffSOP <- total - SOPt
  reldiffSOP <- diff / total
  
  cat("\n")
  cat(paste("Total weight IC (t):", format(total, digits=2),"\n"))
  cat(paste("Total SOP IC (t):", format(SOPt, digits=2),"\n"))
  cat(paste("Difference: ", format(diffSOP, digits=2), " t (", format(reldiffSOP*100, digits=1), "%)\n", sep=""))
}

#' Example of intercatch workflow
#' @param stoxprojectname name of or path to StoX-Reca project
#' @param logbook path to PSV-formatted logbook file.
#' @param exportfile file to write intercatch export to
#' @param metierconfigMeshed path to file with metierconfiguration when mesh-size is available
#' @param metierconfigUnmeshed path to file with metierconfiguration when mesh-size is not available
#' @param SDfleets fleets / metier that SD lines should be exported for. NULL means all fleets, NA no fleets.
#' @param plusGroup plus group for the SD lines (NULL means no plus group)
#' @param unitCANUM unit for catch at age in numbers, may be k,m or n for thosuands, millions or unit (ones) respectively
#' @param force force re-run of stox project before exporting
#' @examples 
#'  runExample("~/workspace/stox/ECA_prosjekter/NSSK/ECA_NSSK_sei_2019", 
#'             "intercatchExport/metiertable_meshed.txt", 
#'             "intercatchExport/metiertable_unmeshed.txt", 
#'             logbook = "~/logbooks/FDIR_HI_ERS_2019_PR_2020-03-04.psv", 
#'             exportfile = "test.csv", 
#'             plusGroup = 10, 
#'             SDfleets = c("OTB_DEF_>=120_0_0_all", "GNS_DEF_all_0_0_all"))
#'  checks("~/workspace/stox/ECA_prosjekter/NSSK/ECA_NSSK_sei_2019", "test.csv")
runExample <- function(stoxprojectname, logbook, exportfile, metierconfigMeshed, metierconfigUnmeshed, SDfleets=NULL, plusGroup=NULL, unitCANUM="k", force=F){
  landings <- extractLandings(stoxprojectname)
  landings <- annotateFromLandings(landings)
  landings <- annotateMeshSize(landings, logbook)
  landings <- annotateAreaFromLandings(landings)
  landings <- annotateMetierMeshSize(landings, metierconfigMeshed, metierconfigUnmeshed)
  Sys.sleep(5) #give some time to notice warnings and messages
  exportIntercatch(stoxprojectname, landings, exportfile, SDfleets = SDfleets, plusGroup=plusGroup, unitCANUM=unitCANUM, force=force)
}
