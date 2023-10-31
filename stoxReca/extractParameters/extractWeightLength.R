#
# Extract parameters for weight (W) - length (L) regression
# Parameters a and b for W=axL**b
# ECA provides for c=log(a) and b for log(W) = c + bL
# c is denoted Intercept
# d is denoted Slope
# Intercepts must be added across all covariates, including "constant"
#

#Fetch data from Parameterization, run project from R:
RstoxFramework::runModel("~/stoxprosjekter/cod_2023/ECA_Cod_2023", "baseline")
p <- RstoxFramework::runModel("~/stoxprosjekter/cod_2023/ECA_Cod_2023", "analysis")

summarizeWL <- function(WeightGivenLength){
  tab <- merge(WeightGivenLength$constant, WeightGivenLength$LogLikelihood, by="Iteration")
  stopifnot(length(unique(tab$LevelIndex))==1)
  tab <- tab[,c("Age", "Iteration", "AgeIndex", "Slope", "Intercept", "LogLikelihood")]
  tab$Level="All"
  keys <- c("Age", "Iteration", "AgeIndex")
  for (n in names(WeightGivenLength)){
    if (!(n %in% c("fish", "LogLikelihood", "constant")))
    tab <- merge(tab, WeightGivenLength[[n]][,.SD, .SDcol=c(keys, "Level", "Intercept")], by=keys, suffixes = c("", paste(".",n, sep="")), allow.cartesian=TRUE)
  }
  
  tab$Intercept.constant <- tab$Intercept
  covariates <- c()
  for (n in names(WeightGivenLength)){
    if (!(n %in% c("fish", "LogLikelihood", "constant"))){
      tab$Intercept <- tab$Intercept + tab[[paste("Intercept", n, sep=".")]]
      covariates <- c(covariates, paste("Level", n, sep="."))
    }
  }
  
  #aggregate first over catchsamples
  covariates <- covariates[covariates != "Level.catchSample"]
  tab <- tab[,list(Slope=mean(Slope), Intercept=mean(Intercept)), by=c(covariates,"Iteration")]
  tab <- tab[,list(Slope=mean(Slope), Intercept=mean(Intercept), a=mean(exp(Intercept)), b=mean(Slope)), by=c(covariates)]
  
  return(tab)
}
tab <- summarizeWL(p$Parameterise$FitWeightGivenLength)

