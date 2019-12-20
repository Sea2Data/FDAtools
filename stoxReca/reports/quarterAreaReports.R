library(Rstox)

#
# The function Rstox::saveDecomposedCatchMatrix provides estimates for decompositions of landings based on for example quarter and area.
# It can be run after any sucessful eca-run to get estimates tabulated for each of some decomposition variables, for example quarter and area
#
# Decompositions can be made for combinations of columns in landings data.
# In addition some common use cases are supported that are not represented by simple combinations of the columns in landings.
# These are:
# - inclusion of plusgroups in reports, by the integer parameter 'plusgr'
# - inclusion of quarter, by a logical parameter 'addQuarterToDecomp'
# - inclusion of area-combinations, by optional arguments 'customMainAreaGrouping' or 'customLocationGrouping'.
#
# mainarea refer to area coding defined by the Norwegian Directorate of Fisheries (Hovedområde)
# location refer to location coding defined by the Norwegian Directorate of Fisheries (Lokasjon)
#

#
#run project residing in <user home>/workspace/stox/projects
#
projectname <- "ECA_NSSK_sei_2018"
#uncomment to re-run project if necessary
#runRScripts(projectname)

#
#save default decomposition (mainarea and quarter)
#
Rstox:::saveDecomposedCatchMatrix(projectname, "testDefaultReport.csv", addQuarterToDecomp = T, plusgr=6)

#
#save custom decomposition based on grouping of main areas and quarter
#

# define new areas in terms of kodes for mainarea:
customArea <- list(area1=c("08","09", "28"), area2=c("41","42","43"), area4="47")

# run with parameter customMainAreaGrouping, note that the default decomposition is set to NULL
Rstox:::saveDecomposedCatchMatrix(projectname, "testMainAreaGrouping.csv", addQuarterToDecomp = T, plusgr=6, customMainAreaGrouping = customArea, decomposition = NULL)


#
#save custom decomposition based on grouping of locations and quarter
#

# load intermediate calculations to determine which locations are in the dta
l <- loadProjectData(projectname, var="prepareRECA")
ldata <- l$prepareRECA$StoxExport$landing
locationcodes <- sprintf("%02d-%02d", ldata[,"hovedomr\u00e5dekode"], ldata[,"lokasjonkode"])

# make two arbitrarily define areas based on these locations
customlocation <- list(area1=locationcodes[1:100], area2=locationcodes[100:length(locationcodes)])

Rstox:::saveDecomposedCatchMatrix(projectname, "testLocationGrouping.csv", addQuarterToDecomp = T, plusgr=6, customLocationGrouping = customlocation, decomposition = NULL)


# All variables in the argument decomposition is included. One can for instance add gear, here represented by the covariate 'gearfactor' which is appended to the landings data
Rstox:::saveDecomposedCatchMatrix(projectname, "testLocationGroupingWgear.csv", addQuarterToDecomp = T, plusgr=6, customLocationGrouping = customlocation, decomposition = c("gearfactor"))

# One can also run for only gear
Rstox:::saveDecomposedCatchMatrix(projectname, "testGear.csv", addQuarterToDecomp = F, plusgr=6, decomposition = c("gearfactor"))