# Example of a way to extract all eca results for different gear groups using the private function Rstox:::getLandings
# and the eca.predict
# extract StoX-reca predictions for different gears
#

library(Rstox)
library(Reca)
projectname <- "ECA_NSSK_sei_2018"

# If proect has not be run before, uncomment the following line
# Rstox::runRScripts()

#extract eca configuration
l <- loadProjectData(projectname, var="prepareRECA")
p <- loadProjectData(projectname, var="runRECA")

AgeLength <- l$prepareRECA$AgeLength
WeightLength <- l$prepareRECA$WeightLength
GlobalParameters <- p$runRECA$GlobalParameters
projecttempres <- l$prepareRECA$StoxExport$temporalresolution
landings <- l$prepareRECA$StoxExport$landing


gearResults <- list()

for (g in unique(landings$gearfactor)){
  
  #filter landings
  gearLandings <- landings[landings$gearfactor == g,]
  
  #reformat filtered landings object for Reca
  decompLandings <- Rstox:::getLandings(gearLandings, AgeLength, WeightLength, projecttempres)
  
  pred <- Reca::eca.predict(AgeLength, WeightLength, decompLandings, GlobalParameters)
  gearResults[[g]] <- pred
}

# 'gearResults' contains result for each geat. Consult documentation (?Reca::eca.predict) for interpertation.
