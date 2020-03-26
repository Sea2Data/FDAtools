library(Rstox)
projectname <- "~/feilsokECA/Hyse_2018_Elise_pt2"
logbookfile <- "~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv"
logbook <- Rstox::readErsFile(logbookfile)
bl <- getBaseline(projectname)


landingsAdjuster <- function(landings){
  return(Rstox::adjustRecaSpatialTemporal(landings, logbook, processDataGear = bl$processData$gearfactor, processDataTemporal = bl$processData$temporal, processDataSpatial = bl$processData$stratumpolygon, gearSelection = "Trawl"))
}

# check that sample composition is OK
Rstox::makeSampleHomogeneityReportRECA(projectname)

#
# prepareRECA and runRECA will not use parameter values set in the stox-project.
# Set desired values in function calls below.
# See function documentation for default values.
#
Rstox::prepareRECA(projectname, minage = 1, maxage = 20, maxlength = 240, landingAdjuster = landingsAdjuster)
saveProjectData(projectname)
Rstox::runRECA(projectname, burnin = 500, thin=1, nSamples = 500)
saveProjectData(projectname)

#
# Plotting and reporting can be done in Stox
# alternatively plotting and reporting can be run from R by uncommenting lines below.
#
#Rstox::plotRECA(projectname)
#Rstox::reportRECA(projectname)
