library(RstoxData)

#' get area
#' @description
#'  Extracts area code or other column from polygon file for some coordinates
#' @details
#'  Coordinates must be given in the same projection as 'polygons'
#' @param polygons \code{\link[sp]{SpatialPolygonsDataFrame}}
#' @param longitude num: vector of longitudes
#' @param latitude num: vector of latitudes
#' @param column chr: identifies a column in 'polygons' whose value is to be extracted for each position
#' @return vector with the value of 'column' for each position
#' @examples
#'  data(areas)
#'  # get area code for 21.1 E 69.9 N, and 21.1 E 70 N
#'  getAreaProprerty(areas, c(21.1,21.1), c(69.9,70), "HAVOMR")
#' @export
getAreaProprerty <- function(polygons, longitude, latitude, column){
  
  if (length(longitude) != length(latitude)){
    stop("longitude and latitude vectors are not of same length")
  }
  if (!(column %in% names(polygons))){
    stop(paste("Column", column, "does not exist in 'polygons'"))
  }
  
  if (length(latitude) < 1){
    return(polygons[[column]][0])
  }
  
  points <- sp::SpatialPoints(data.frame(lon=longitude, lat=latitude), proj4string = sp::CRS(sp::proj4string(polygons)))
  props <- sp::over(points, polygons)[[column]]
  
  return(props)
}

#' Set Fdir area based on positions
#' @param samples samples to be annotated, as returned by \code{\link{readSamples}}
#' @param areafile rdsfile with areas, project specific format.
#' @return samples with area filled based on position
annotate_areas <- function(samples, areafile="fdir.areas.2018.rds"){
  areas <- readRDS(areafile)
  
  #use stop position if start not found
  samples$lat <- samples$latitudestart
  samples$lon <- samples$longitudestart
  samples$lat[is.na(samples$lat)] <- samples$latitudeend[is.na(samples$lat)]
  samples$lon[is.na(samples$lon)] <- samples$longitudeend[is.na(samples$lon)]
  
  #set area when position is found, but no system 2 area coding
  selection <- !is.na(samples$lat) & !is.na(samples$lon) & (is.na(samples$system) | samples$system != 2 | is.na(samples$area))
  areas <- getAreaProprerty(areas, samples$lon[selection], samples$lat[selection], "HAVOMR")
  samples$system[selection] <- 2
  samples$area[selection] <- areas

  samples$lat <- NULL
  samples$lon <- NULL
  
  return(samples)
    
}

#' Read sample data.
#' @param bioticfile sample data in biotic 3.0 format.
#' @return \code{\link[data.table]{data.table}} with columns from fishstation, individual and agedetermination
readSamples <- function(bioticfile){
  
  biotic <- RstoxData::readXmlFile(bioticfile)
  individuals <- merge(biotic$fishstation, biotic$individual, by=names(biotic$fishstation)[names(biotic$fishstation) %in% names(biotic$individual)])
  individuals <- merge(individuals, biotic$catchsample, by=names(individuals)[names(individuals) %in% names(biotic$catchsample)])
  
  keysInd <- names(individuals)[names(individuals) %in% names(biotic$agedetermination)]
  keysIndAge <- c(keysInd, "preferredagereading")
  keysAge <- c(names(individuals)[names(individuals) %in% names(biotic$agedetermination)], "agedeterminationid")
  
  #chose age to some age when preferredagereading is not set
  ageids <- unique(biotic$agedetermination[,keysAge, with=F])
  ageids <- ageids[!duplicated(ageids[,keysInd, with=F])]
  individuals <- merge(individuals, ageids, by=keysInd, all.x=T)
  individuals$preferredagereading[is.na(individuals$preferredagereading)] <- individuals$agedeterminationid[is.na(individuals$preferredagereading)]
  individuals$agedeterminationid <- NULL
  
  individuals <- merge(individuals, biotic$agedetermination, by.x=keysIndAge, by.y=keysAge, all.x=T)
  
  return(individuals)
}

#' Sample summary table
#' @param individuals All samples for a population of interest. 
#'  \code{\link[data.table]{data.table}} with mesured fish, as returned by \code{\link{readSamples}}.
#' @param totaLanded total weight (tonnes) of landings in the population the samples in 'individuals' was sampled from.
#' @param agedBySampleType logical wether to derived age-samples based on declared sample types. Otherwise age-read content is used.
#' @param agesampletypes sample type codes classified as age-sample
#' @return One row of sample summary tabe for the data in 'individuals':
#'  \describe{
#'   \item{no.unique.vessels}{Number of unique vessels sampled for length measurements}
#'   \item{no.length.samples}{Number of catches sampled for length measurements}
#'   \item{no.lengthmeasured.individuals}{number of individiual specimen measured for length}
#'   
#'   \item{no.unique.vessels.agesamples}{Number of unique vessels sampled for age}
#'   \item{no.age.samples}{Number of catches sampled for age}
#'   \item{no.aged.individuals}{Number of individual specimen that age was determined for}
#'   
#'   \item{landings.tonnes}{Provided by the parameter 'totalLanded'}
#'   \item{lengthsamples.pr.1000.t}{Number of catches sampled for length pr 1000 tonnes.}
#'   \item{agesamples.pr.1000.t}{Number of catches sampled for age pr 1000 tonnes.}
#'   \item{aged.inidividuals.pr.1000.t}{Number of individuals aged pr 1000 tonnes.}
#'  }
countSamples <- function(individuals, totalLanded, agedBySampleType=F, agesampletypes=c(20,21,22,23,24,25,26,27,30,31)){
  
  if (length(unique(individuals$startyear))>1){
    stop("Multiple years in samples.")
  }
  
  if (!agedBySampleType){
    message("Inferring age samples from ages determined.")
    agedindividuals <- individuals[!is.na(individuals$age),]    
  } else{
    message(paste("Inferring age samples by sample types:", paste(agesampletypes, collapse=", ")))
    agedindividuals <- individuals[(!is.na(individuals$sampletype) & individuals$sampletype %in% agesampletypes),]    
  }
  
  #treat platform 9000 "komersielle fartøy" as unkown catch platform
  individuals$catchplatform[!is.na(individuals$catchplatform) & individuals$catchplatform == "9000"] <- NA
  
  if (sum(is.na(individuals$catchplatform)) > 0){
    message(paste0(individuals$startyear[1], ": ", length(unique(individuals$serialnumber[is.na(individuals$catchplatform)])), " catches without catchplatform."))
  }

  if (totalLanded > 0){
    lengthsamples.pr.1000.t = length(unique(individuals$serialnumber)) * 1000 / totalLanded
    agesamples.pr.1000.t = length(unique(agedindividuals$serialnumber)) * 1000 / totalLanded
    aged.inidividuals.pr.1000.t = nrow(agedindividuals) * 1000 / totalLanded
  }
  else{
    lengthsamples.pr.1000.t = NA
    agesamples.pr.1000.t = NA
    aged.inidividuals.pr.1000.t = NA
  }
  
  row <- data.table::data.table(year = individuals$startyear[1],
                                
                                no.unique.vessels = length(unique(individuals$catchplatform)),
                                no.length.samples = length(unique(individuals$serialnumber)),
                                no.lengthmeasured.individuals = nrow(individuals),
                                
                                no.unique.vessels.agesamples = length(unique(agedindividuals$catchplatform)),
                                no.age.samples = length(unique(agedindividuals$serialnumber)),
                                no.aged.individuals = nrow(agedindividuals),
                                
                                landings.tonnes = totalLanded,
                                lengthsamples.pr.1000.t = lengthsamples.pr.1000.t,
                                agesamples.pr.1000.t = agesamples.pr.1000.t,
                                aged.inidividuals.pr.1000.t = aged.inidividuals.pr.1000.t
                                
                                )
  return(row)
}

#' Sampling overview across years
#' @description
#'  Creates data table with overview of sampling activity for AFWG
#' @param samplefiles list mapping years to sample file locations (biotic)
#' @param landingsfiles list mapping years to landing file locations (LSS)
#' @param speciesSamples species codes to extract from samples (biotic: catchsamples/catchcategory)
#' @param speciesLanding species codes to extract from landings (lss: ART - FAO kode)
#' @param areas area codes to extract data for
#' @param missiontypes mission types to include from samples.
#' @param addAreas logical, whether to add areas based on position
#' @return \code{\link[data.table]{data.table}} with one row for each year, formatted as output from ~\code{\link{countSamples}}
makesampleCountTable <- function(samplefiles, landingsfiles, speciesSamples, speciesLanding, areas, missiontypes, addAreas=T){
  
  filterBiotic <- function(samples){return(samples[!is.na(samples$system) & samples$system==2 & as.integer(samples$area) %in% as.integer(areas),])}
  filterLanding <- function(landings){return(landings[landings$`Fartøynasjonalitet (kode)`=="NOR" & landings$`Redskap (kode)` != 90 & landings$`Hovedområde (kode)` %in% areas,])}
  
  if (!all(names(samplefiles) %in% names(landingsfiles))){
    stop("Landings and samples are not provided for the same years")
  }
  
  message(paste("Processing data for species:", paste(speciesLanding, collapse=", ")))
  
  tab <- NULL
  for (year in names(samplefiles)){
    samples <- readSamples(samplefiles[[year]])
    landings <- RstoxData::readLssFile(landingsfiles[[year]])
    
    # filter to AFWG area and restrict to commerical samples / mission types
    
    samples <- samples[samples$missiontype %in% missiontypes,]
    samples <- samples[samples$catchcategory %in% speciesSamples,]
    if (addAreas){
      samples <- annotate_areas(samples)
    }
    samples <- filterBiotic(samples)
    landings <- filterLanding(landings)
    landings <- landings[landings$`Art FAO (kode)` %in% speciesLanding,]
    
    sampCount <- countSamples(samples, sum(landings$Rundvekt, na.rm=T)/1000)
    
    tab <- rbind(tab, sampCount)
    
  }
  
  tab$filterMissiontypes <- paste(sort(missiontypes), collapse=",")
  tab$filterAreas <- paste(sort(areas), collapse=",")
  tab$filterSpeciesTsn <- paste(speciesSamples, collapse=",")
  tab$filterSpeciesFAO <- paste(speciesLanding, collapse=",")
  
  return(tab)
}


saveAFWGtable <- function(outfile="sampleCount.xlsx"){
  samplefiles <- list()
  samplefiles[["2014"]] <- "~/bioticsets/v3/biotic_year_2014.xml"
  samplefiles[["2015"]] <- "~/bioticsets/v3/biotic_year_2015.xml"
  samplefiles[["2016"]] <- "~/bioticsets/v3/biotic_year_2016.xml"
  samplefiles[["2017"]] <- "~/bioticsets/v3/biotic_year_2017.xml"
  samplefiles[["2018"]] <- "~/bioticsets/v3/biotic_year_2018.xml"
  samplefiles[["2019"]] <- "~/bioticsets/v3/biotic_year_2019.xml"
  
  landingfiles <- list()
  landingfiles[["2014"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2014_PR_2016-12-08.psv"
  landingfiles[["2015"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2015_PR_2016-12-08.psv"
  landingfiles[["2016"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2016_PR_2017-10-31.psv"
  landingfiles[["2017"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2017_PR_2018-10-05.psv"
  landingfiles[["2018"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2018_PR_2019-10-03.psv"
  landingfiles[["2019"]] <- "~/landingsets/LSS/FDIR_HI_LSS_FANGST_2019_PR_2020-03-03.psv"
  
  missiontypes <- c(1,2,3,9,10,11,12,13,19)
  message(paste("Extracting data for mission types:", paste(missiontypes, collapse = ",")))
  
  message("Extracting data for ICES subarea 1 and 2.")
  message("Excluding area 35,36, and 38 (approximate Jan Mayen zone)")
  defaultareas <- c("00","01","02","03","04","05","06","07","10","11","12","13","14","15","16","17","18","20","21","22","23","24","25","26","27","30","34","37","39","50")
  message("CAP: also excluding area 30,34,50 (approximating ICES 2a, west of 5 deg W.)")  
  capelinareas <- defaultareas[defaultareas != c("30","34","50")]
  
  
  tab <- NULL

  #lodde
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("162035"), speciesLanding = c("CAP"), areas = capelinareas, missiontypes = missiontypes), tab)

  #Breiflabb
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("164501","164498","164497"), speciesLanding = c("MON", "ANF"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #blåkveite
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("172930"), speciesLanding = c("GHL"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #snabeluer
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("166756"), speciesLanding = c("REB"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #Vanlig uer
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("166781"), speciesLanding = c("REG"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #sei
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("164727"), speciesLanding = c("POK"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #hyse
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("164744"), speciesLanding = c("HAD"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  #torsk
  tab <- rbind(makesampleCountTable(samplefiles, landingfiles, speciesSamples = c("164712"), speciesLanding = c("COD"), areas = defaultareas, missiontypes = missiontypes), tab)
  
  print(tab)
  xlsx::write.xlsx2(tab, outfile, sheetName="sample count", row.names = F)
}