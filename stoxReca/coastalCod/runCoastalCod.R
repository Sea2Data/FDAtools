# In order to run coastal cod analysis:
# Set u StoX project with desired spatial strata, and the desired run parameters (including the CC-flag for "coastal cod-analysis").
# This will filter and configure samples as desired, but the configuration will be inclomplete for landings:
# - StoX 2.7 cannot assign the spatial covariate based on other columns than area and location,
# - and cannot adjust landings from logbook records
#
# This script will manipulate StoX-output files after:
# - assign spatial covariate for landings based on main-area (hovedområde) and coastal-zone codes (Kyst-Hav)
# - redistribute landings based on logbook records.
# - run or re-run the estimate with this modified configuration.


library(Rstox)

#' Assigns the given covariate in landings based mainarea and terretorial-line codings (hovedområde & kyst/hav)
#' @param x data.frame with landings
#' @param covariate identifies a column in x
assignSpatialLandingsCoastalCod <- function(x, covariate="spatial"){
  # 8 kyst, 0 hav
  x[x$hovedområdekode %in% c(3,4) & x$kysthavkode == 0,covariate] <- "s300-400"
  x[x$hovedområdekode %in% c(3,4) & x$kysthavkode == 8,covariate] <- "s301-401"
  x[x$hovedområdekode == 5 & x$kysthavkode == 0,covariate] <- "s500"
  x[x$hovedområdekode == 0 | x$hovedområdekode == 5 & x$kysthavkode == 8,covariate] <- "s000-501"
  x[x$hovedområdekode %in% c(6,7) & x$kysthavkode == 8,covariate] <- "s601-701"
  x[x$hovedområdekode %in% c(6,7) & x$kysthavkode == 0,covariate] <- "s600-700"
  if (!all(x$hovedområdekode %in% c(0,3,4,5,6,7))){
    stop("Contains area codes not covered by this covariate assignment.")  
  }

  return(x)
}

makeLogbookAdjuster <- function(projectname, logbookfile){
  bl <- getBaseline(projectname)
  logbook <- Rstox::readErsFile(logbookfile)
  landingsAdjuster <- function(landings){
    return(Rstox::adjustRecaSpatialTemporal(landings, logbook, processDataGear = bl$processData$gearfactor, processDataTemporal = bl$processData$temporal, processDataSpatial = bl$processData$stratumpolygon, gearSelection = "Trawl"))
  }
  return(landingsAdjuster)
}


projectname <- "Kysttorsk_AFWG_2019"
logbookfile <- "~/logbooks/FDIR_HI_ERS_2019_PR_2020-03-04.psv"
# check that sample composition is OK
Rstox::makeSampleHomogeneityReportRECA(projectname)

#
# run coastal cod analysis
# need to specify function for setting spatial strata on landings.
#

#' applies both the assignSpatialLandingsCoastalCod and the logbook-adjustment
adjuster <- function(landings){
  logbookAdjuster <- makeLogbookAdjuster(projectname, logbookfile)
  landings <- assignSpatialLandingsCoastalCod(landings)
  landings <- logbookAdjuster(landings)
  return(landings)
}

# chosen landingsAdjuster must correspond to namings in the stratafile in the StoX-Reca project
Rstox::prepareRECA(projectname, minage = 1, maxage = 20, maxlength = 240, landingAdjuster = adjuster)
saveProjectData(projectname)


#sett feillesingsmatrise for kysttorsk-skrei
CCerrorList <- list(ptype1.CC=1, ptype2.CC=0.7, ptype4.CC=0.3, ptype5.CC=0, ptype1.S=0, ptype2.S=0.3, ptype4.S=0.7, ptype5.S=1)

l<-loadProjectData(projectname)
var <- l$prepareRECA
var$AgeLength$CCerrorList <- CCerrorList
setProjectData(
  projectName = projectname,
  var = var,
  name = "prepareRECA"
)
saveProjectData(projectname)

Rstox::runRECA(projectname, burnin = 5000, thin=1, nSamples = 500, CC = T, CCError = F)
saveProjectData(projectname)

# plots and reports can now be generated in StoX, or with the following lines of code:
Rstox::plotRECA(projectname)
Rstox::reportRECA(projectname)


