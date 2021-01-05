library(RstoxFDA)
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

  return(prepdata$prepareRECA$StoxExport$landing)
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

#'
annotateMetier <- function(stoxLandings){
  warning("Dummy metier annotation")
  stoxLandings$Fleet <- "Fleet"
  return(stoxLandings)
}

#'
annotateArea <- function(stoxLandings){
  warning("Dummy area annotation")
  stoxLandings$Area <- "Area"
  stoxLandings$AreaType <- "AreaType"
  return(stoxLandings)
}

#' write HI line
writeHI <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea,
                    DepthRange="NA", UnitEffort="NA", Effort="-9", AreaQualifier="NA"){
  writeLines(con=stream, paste("HI", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, UnitEffort, Effort, AreaQualifier, sep=","))
}

#' write SI line
writeSI <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, Species, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin, UnitCATON, CATON, OffLandings, 
                    varCATON="-9", DepthRange="NA", Stock="NA", QualityFlag="NA", InfoFleet="", InfoStockCoordinator="", InfoGeneral=""){
  writeLines(con=stream, paste("SI", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, Species, Stock, CatchCategory, ReportingCategory, DataToFrom, Usage, SamplesOrigin, QualityFlag, UnitCATON, format(CATON, digits=2), format(OffLandings, digits=2), varCATON, InfoFleet, InfoStockCoordinator, InfoGeneral, sep=","))
}

#' write SD line
writeSD <- function(stream,
                    Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, Species, CatchCategory, ReportingCategory, Sex, CANUMtype, AgeLength, PlusGroup, unitMeanWeight, unitCANUM, UnitAgeOrLength, UnitMeanLength, Maturity, NumberCaught, MeanWeight, MeanLength, 
                    DepthRange="NA", Stock="NA",SampledCatch="-9", NumSamplesLngt="-9", NumLngtMeas="-9", NumSamplesAge="-9", NumAgeMeas="-9", varNumLanded="-9", varWgtLanded="-9", varLgtLanded="-9"){
  writeLines(con=stream, paste("SD", Country, Year, SeasonType, Season, Fleet, AreaType, FishingArea, DepthRange, Species, Stock, CatchCategory, ReportingCategory, Sex, CANUMtype, AgeLength, PlusGroup, SampledCatch, NumSamplesLngt, NumSamplesAge, NumAgeMeas, unitMeanWeight, unitCANUM, UnitAgeOrLength, UnitMeanLength, Maturity, format(NumberCaught, digits=4), format(MeanWeight,digits=2), format(MeanLength, digits=2), varNumLanded, varWgtLanded, varLgtLanded, sep=","))
}


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
  
  for (catchCategory in unique(annotatedStoxLandings$CatchCategory)){ #exp 1 cat
    for (reportingCategory in unique(annotatedStoxLandings$ReportingCategory)){ #exp 1 cat
      for (dataToFrom in unique(annotatedStoxLandings$DataToFrom)){ #exp 1 cat
        for (year in unique(annotatedStoxLandings$Year)){ #exp 1 cat
          for (usage in unique(annotatedStoxLandings$Usage)) #exp 2 cat
            for (season in unique(annotatedStoxLandings$Season)){ #exp 4 cat
              for (fleet in unique(annotatedStoxLandings$Fleet)){ #exp many cat
                for (area in unique(annotatedStoxLandings$Area)){ #exp many cat
                  data <- annotatedStoxLandings[annotatedStoxLandings$CatchCategory == catchCategory &
                                                  annotatedStoxLandings$ReportingCategory == reportingCategory &
                                                  annotatedStoxLandings$DataToFrom == dataToFrom &
                                                  annotatedStoxLandings$Year == year &
                                                  annotatedStoxLandings$Usage == usage &
                                                  annotatedStoxLandings$Season == season &
                                                  annotatedStoxLandings$Fleet == fleet &
                                                  annotatedStoxLandings$Area == area,]
                  checkUnique("Country", data$Country)
                  checkUnique("SeasonType", data$SeasonType)
                  checkUnique("AreaType", data$AreaType)
                  writeHI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area)
                  
                  for (species in unique(data$Species)){
                    data <- data[data$Species == species,]
                    checkUnique("UnitCATON", data$UnitCATON)
                    
                    if (!(fleet %in% SDfleets) | nrow(data)==0){
                      writeSI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, DataToFrom = dataToFrom, Usage = usage, SamplesOrigin = "NA", UnitCATON = data$UnitCATON[1], CATON = sum(data$CATON), OffLandings = sum(data$OffLandings))
                    }
                    else if ((fleet %in% SDfleets) & nrow(data)>0){
                      
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
                        writeSD(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, 
                                Sex = "", CANUMtype="Age", AgeLength = lowerage, PlusGroup=plg, unitMeanWeight="g", unitCANUM=unitCANUM, UnitAgeOrLength="year", UnitMeanLength="cm", Maturity="NA", NumberCaught=caa, MeanWeight=meanW, MeanLength=meanL)
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

run <- function(stoxprojectname, exportfile, SDfleets=NULL, plusGroup=NULL, unitCANUM="k", force=F){
  landings <- extractLandings(stoxprojectname)
  landings <- annotateFromLandings(landings)
  landings <- annotateArea(landings)
  landings <- annotateMetier(landings)
  exportIntercatch(stoxprojectname, landings, exportfile, SDfleets = SDfleets, plusGroup=plusGroup, unitCANUM=unitCANUM, force=force)
}

