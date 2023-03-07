#
# Adaptation of script for intercatch export to StoX 3
#
# Exports landings to intercatch and runs Reca for the segments where SD lines are requested.
# Needs a stox project to be set up with necessary filtering and Reca-parameterization
#
# In order to get correct metier/fleet annotations, that stox project will need landings data that is pre-processed,
# and metiers must be annotated in one of the columns in the landings format.
# This would most sensibly be annotated in the gear column, but if native gear codes are needed for Reca parameterisation another column may be abused for the purpose.
# The default option is therefore landingssite, which is not otherwise required for intercatch.
# 
# In addition, the columns Usage and species must be converted to intercatch codes. This can be done in Stox, or on the StoxLandingData prior to calling exportInterCatch.
#

library(RstoxFDA)
library(RstoxData)
library(data.table)


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


#' Compare StoX and intercatch
#' @description 
#'  Reads data from stox project and compare it with data exported for intercatch
#' @param StoxLandingData 
#' @param intercatchfile path to file with data in intercatch exchange format
checks <- function(StoxLandingData, intercatchfile){

  intercatchdata <- RstoxData::parseInterCatch(intercatchfile)
  
  #compare species
  cat(paste("Species StoX-Reca:", paste(unique(StoxLandingData$Landing$Species), collapse=","), "\n"))
  cat(paste("Species intercatch (IC):", paste(unique(intercatchdata$SI$Species), collapse=","), "\n"))
  
  #compare total weights
  sis <- intercatchdata$SI
  sis$CATON[sis$UnitCATON=="kg"] <- sis$CATON[sis$UnitCATON=="kg"]/1000
  
  totstox <- sum(StoxLandingData$Landing$RoundWeight)/1000
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
  SISD$CATON[SISD$UnitCATON=="kg"] <- SISD$CATON[SISD$UnitCATON=="kg"]/1000
  
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


#' export intercatch data from StoX project
#' @description 
#'  export intercatch data from StoX project
#'  Need metier annotations hacked into StoxLandingData somehow. Provide the column containint metiers in 'metierColumn'.
#' @details
#'   Consult the InterCatch exchange format definitions when necessary: https://www.ices.dk/data/Documents/Intercatch/IC-ExchangeFormat1-0.pdf
#' @param StoxLandingData StoxLandingData
#' @param RecaParameterData Reca parameterizattion data.
#' @param exportfile file to write intercatc data to
#' @param seasonType the temporal resolution for the intercatc export, may be 'Month', 'Quarter' or 'Year'
#' @param country ISO 3166 2-alpha code for country submitting data
#' @param unitCATON unit for landings, may be kg or t.
#' @param unitCANUM unit for catch at age in numbers, may be k,m or n for thosuands, millions or unit (ones) respectively
#' @param samplesOrigin information of origin of samples for SI line. See intercatch exchange format SampleOrigin.
#' @param plusGroup plus group for the SD lines (NULL means no plus group)
#' @param metierColumn the column in StoxLandingData containing metier (fleet) category for landings
#' @param icesAreaColumn column where ices areas are annotated to the desired resolution. area type will be inferred
#' @param SDfleets fleets / metier that SD lines should be exported for. NULL means all fleets, NA no fleets.
exportIntercatch <- function(StoxLandingData, RecaParameterData, exportfile, seasonType="Quarter", country="NO", unitCATON="kg", unitCANUM="n", samplesOrigin="U", plusGroup=NULL, metierColumn="LandingSite", icesAreaColumn="IcesArea", SDfleets=NULL){

  if (!all(nchar(StoxLandingData$Landing$Species)==3)){
    stop("species must be provided as FAO three letter species-code")
  }
  if (!all(StoxLandingData$Landing$Usage %in% c("I","H", NA))){
    stop("usage must be encoded as I (industrial) or H (human consumption")
  }
  
  StoxLandingData$Landing$Area <- StoxLandingData$Landing[[icesAreaColumn]]  
  StoxLandingData$Landing$AreaType <- as.character(NA)
  StoxLandingData$Landing$AreaType[sapply(strsplit(StoxLandingData$Landing$Area, "\\."), FUN=function(x){length(x)})==1] <- "AreaTop"
  StoxLandingData$Landing$AreaType[sapply(strsplit(StoxLandingData$Landing$Area, "\\."), FUN=function(x){length(x)})==2] <- "SubArea"
  StoxLandingData$Landing$AreaType[sapply(strsplit(StoxLandingData$Landing$Area, "\\."), FUN=function(x){length(x)})==3] <- "Div"
  StoxLandingData$Landing$AreaType[sapply(strsplit(StoxLandingData$Landing$Area, "\\."), FUN=function(x){length(x)})==4] <- "SubDiv"
  StoxLandingData$Landing$AreaType[sapply(strsplit(StoxLandingData$Landing$Area, "\\."), FUN=function(x){length(x)})==5] <- "Unit"
  
  if (any(is.na(StoxLandingData$Landing$AreaType))){
    stop("AreaType could not be deduced for all Areas.")
  }
  
  StoxLandingData$Landing$Fleet <- StoxLandingData$Landing[[metierColumn]]
  
  StoxLandingData$Landing$Country <- country

  checkParam("seasonType", seasonType, c("Quarter", "Month", "Year"))
  StoxLandingData$Landing$SeasonType <- seasonType
  
  checkParam("unitCATON", unitCATON, c("kg", "t"))
  
  
  #
  # annotate season
  #
  if (seasonType == "Quarter"){
    StoxLandingData$Landing$Season <- substr(quarters(StoxLandingData$Landing$CatchDate, T),2,2)
  }
  else if (seasonType == "Month"){
    StoxLandingData$Landing$Season <- substr(StoxLandingData$Landing$CatchDate, 6,7)
  }
  else if (seasonType == "Year"){
    StoxLandingData$Landing$Season <- StoxLandingData$Landing$year
  }
  else{
    #assert false
    stop("Error (seasonType)")
  }
  
  #
  # extract species code from data
  #
  
  if (length(unique(StoxLandingData$Landing$Species)) != 1){
    stop("Landings does not contain unique species code")
  }
  
  StoxLandingData$Landing$CatchCategory <- "L"
  StoxLandingData$Landing$ReportingCategory <- "R"
  StoxLandingData$Landing$DataToFrom <- "NA"
    
  StoxLandingData$Landing$UnitCATON <- unitCATON
  
  if (unitCATON == "kg"){
    StoxLandingData$Landing$CATON <- StoxLandingData$Landing$RoundWeight    
  }
  else if (unitCATON == "t"){
    StoxLandingData$Landing$CATON <- StoxLandingData$Landing$RoundWeight / 1000
  }
  else{
    stop("Error UnitCATON")
  }
  
  StoxLandingData$Landing$OffLandings <- NA
  
  neededColumns <- c("Year", "Season", "Fleet", "Area","Country", "Species", "SeasonType", "AreaType", "CatchCategory",
                     "ReportingCategory", "DataToFrom", "Usage", "UnitCATON", "CATON", "OffLandings")
  
  missingColumns <- neededColumns[!(neededColumns %in% names(StoxLandingData$Landing))]
  if (length(missingColumns) > 0){
    stop(paste("Some columns that are needed for intercatch export are not annotated on landings. Missing: "), paste(missingColumns, collapse=","))
  }
  
  if (is.null(SDfleets)){
    SDfleets <- unique(StoxLandingData$Landing[[metierColumn]])
  }
  
  missingFleets <- SDfleets[!is.na(SDfleets) & !(SDfleets %in% StoxLandingData$Landing[[metierColumn]])]
  if (length(missingFleets) > 0){
    stop(paste("Not all specified fleets / metiers found in landings. Missing:", paste(missingFleets, collapse=",")))
  }
  
  checkParam("unitCANUM", unitCANUM, c("k", "m", "n"))
  
  
  stream <- file(exportfile, open="w")
  for (year in unique(StoxLandingData$Landing$Year)){ #exp 1 cat
    for (season in unique(StoxLandingData$Landing$Season)){ #exp 4 cat
      for (fleet in unique(StoxLandingData$Landing$Fleet)){ #exp many cat
        for (area in unique(StoxLandingData$Landing$Area)){ #exp many cat
          data <- StoxLandingData$Landing[  StoxLandingData$Landing$Year == year &
                                  StoxLandingData$Landing$Season == season &
                                  StoxLandingData$Landing$Fleet == fleet &
                                  StoxLandingData$Landing$Area == area,]
          # dont write lines for cells with no catch
          if (nrow(data) > 0){
            checkUnique("Country", data$Country)
            checkUnique("SeasonType", data$SeasonType)
            checkUnique("AreaType", data$AreaType)
            
            writeHI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area)                    
            
            
            for (catchCategory in unique(StoxLandingData$Landing$CatchCategory)){ #exp 1 cat
              for (reportingCategory in unique(StoxLandingData$Landing$ReportingCategory)){ #exp 1 cat
                for (dataToFrom in unique(StoxLandingData$Landing$DataToFrom)){ #exp 1 cat
                  for (species in unique(data$Species)){
                    data <- StoxLandingData$Landing[StoxLandingData$Landing$CatchCategory == catchCategory &
                                          StoxLandingData$Landing$ReportingCategory == reportingCategory &
                                          StoxLandingData$Landing$DataToFrom == dataToFrom &
                                          StoxLandingData$Landing$Year == year &
                                          StoxLandingData$Landing$Season == season &
                                          StoxLandingData$Landing$Fleet == fleet &
                                          StoxLandingData$Landing$Area == area &
                                          StoxLandingData$Landing$Species == species,]
                    checkUnique("UnitCATON", data$UnitCATON)
                    
                    #intercatch does not allow multiple usages within the variables filtered for above.
                    #extract most common
                    tab <- aggregate(list(w=data$RoundWeight), by=list(usage=data$Usage), FUN=function(x){sum(x, na.rm=T)})
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
                      SL <- list()
                      SL$Landing <- data
                      result <- RstoxFDA::RunRecaModels(RecaParameterData, SL)
                      
                      if (unitCANUM == "k"){
                        unit <- "10^3 individuals"
                      } 
                      else if (unitCANUM == "m"){
                        unit <- "10^6 individuals"
                      }
                      else if (unitCANUM == "n"){
                        unit <- "individuals"
                      }
                      else{
                        stop("Error: unitCANUM")
                      }
                      
                      ageMat <- RstoxFDA::ReportRecaCatchAtAge(result, PlusGroup = plusGroup, Unit = unit)
                      meanWtab <- RstoxFDA::ReportRecaWeightAtAge(result, PlusGroup = plusGroup, Unit = "g")
                      meanLtab <- RstoxFDA::ReportRecaLengthAtAge(result, PlusGroup = plusGroup, Unit = "cm")
                      
                      #format plusgroup for report
                      plg <- "-9"
                      if (!is.null(plusGroup)){
                        plg <- plusGroup
                      }
                      
                      writeSI(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, DataToFrom = dataToFrom, Usage = usage, SamplesOrigin = samplesOrigin, UnitCATON = data$UnitCATON[1], CATON = sum(data$CATON), OffLandings = sum(data$OffLandings))
                      for (age in ageMat$NbyAge$Age){
                        lowerage <- gsub("\\+", "", age) #remove plus sign from plus group
                        caa <- ageMat$NbyAge$CatchAtAge[ageMat$NbyAge$Age==age]
                        meanW <- meanWtab$MeanWeightByAge$MeanIndividualWeight[meanWtab$MeanWeightByAge$Age==age]
                        meanL <- meanLtab$MeanLengthByAge$MeanIndividualLength[meanLtab$MeanLengthByAge$Age==age]
                        
                        #Sex is mandatory in the sense that the field must be filled (but accepts N=indetermined). Intercatch doc says its not mandatory
                        writeSD(stream, Country = data$Country[1], Year = year, SeasonType = data$SeasonType[1], Season = season, Fleet = fleet, AreaType = data$AreaType[1], FishingArea = area, Species = species, CatchCategory = catchCategory, ReportingCategory = reportingCategory, 
                                Sex = "N", CANUMtype="Age", AgeLength = age, PlusGroup=plg, unitMeanWeight="g", unitCANUM=unitCANUM, UnitAgeOrLength="year", UnitMeanLength="cm", Maturity="NA", NumberCaught=caa, MeanWeight=meanW, MeanLength=meanL)
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

stoxCalculations <- RstoxFramework::runProject("~/stoxprosjekter/testing/reca_neasaithe_2021/")
landings <- stoxCalculations$landings_FilterFishery #readRDS("landings_example.rds")
parameterization <- stoxCalculations$ParameterizeReca #readRDS("parameterization.rds")

speciesConversion <- readRDS("speciesConversion.rds")
usageConversion <- readRDS("usageConversion.rds")
landings$Landing$Species <- RstoxFDA::convertCodes(landings$Landing$Species, speciesConversion)
landings$Landing$Usage <- RstoxFDA::convertCodes(landings$Landing$Usage, usageConversion, strict = F)

exportIntercatch(landings, parameterization, "test.csv", plusGroup = 12)

checks(landings, "test.csv")
