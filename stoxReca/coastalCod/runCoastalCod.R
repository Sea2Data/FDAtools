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

# run logbook cleaning 

library(Rstox)

#' Assigns the given covariate in landings based on the following rules:
#' hovedområdekode==0  --> 000
#' hovedområdekode==3 & kysthavkode==0 --> 300
#' hovedområdekode==3 & kysthavkode==1 --> 301
#' ...
#' hovedområdekode==7 & kysthavkode==0 --> 700
#' hovedområdekode==7 & kysthavkode==1 --> 701
#' 
#' @param x data.frame with landings
#' @param covariate identifies a column in x
assignSpatialLandingsCoastalCod <- function(x, covariate="spatial"){
  # 8 kyst, 0 hav
  x[x$hovedområdekode == 0, covariate] <- "s000"
  x[x$hovedområdekode == 3 & x$kysthavkode == 0,covariate] <- "s300"
  x[x$hovedområdekode == 3 & x$kysthavkode == 8,covariate] <- "s301"
  x[x$hovedområdekode == 4 & x$kysthavkode == 0,covariate] <- "s400"
  x[x$hovedområdekode == 4 & x$kysthavkode == 8,covariate] <- "s401"
  x[x$hovedområdekode == 5 & x$kysthavkode == 0,covariate] <- "s500"
  x[x$hovedområdekode == 5 & x$kysthavkode == 8,covariate] <- "s501"
  x[x$hovedområdekode == 6 & x$kysthavkode == 0,covariate] <- "s600"
  x[x$hovedområdekode == 6 & x$kysthavkode == 8,covariate] <- "s601"
  x[x$hovedområdekode == 7 & x$kysthavkode == 0,covariate] <- "s700"
  x[x$hovedområdekode == 7 & x$kysthavkode == 8,covariate] <- "s701"
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



projectname <- "Kysttorsk_AFWG_2018"
logbookfile <- "~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv"
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
Rstox::runRECA(projectname, burnin = 500, thin=1, nSamples = 500, CC = T)
saveProjectData(projectname)

# plots and reports can now be generated in StoX, or with the following lines of code:
Rstox::plotRECA(projectname)
Rstox::reportRECA(projectname)

