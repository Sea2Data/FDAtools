library(Rstox)
library(Reca)

#' make list of gears coded with Fdir system from list fo gear coded with IMR 2-digit code
convertGears <- function(imr2letter){
  fdir <- c()
  for (gear in imr2letter){
    if (gear == 41){
      fdir <- c(fdir, c(20,21,22))
    }
    else{
      stop(paste("Code", gear, "not defined."))
    }
  }
  return(fdir)
}

#' Parse old ECA
#' @description 
#'  Parse output from ECA 3.x / ECA 4.x
#' @param oldEcaOutputFile filename to read data from
#' @return list() with members for estimates and report configuration
parseOldEca <- function(oldEcaOutputFile){
  f <- file(oldEcaOutputFile, open="rU")
  lines <- readLines(f)
  close(f)
  
  output <- list()
  
  i <- 1
  
  # read report parameters
  while (trimws(lines[i]) != "Catch at age is summed over:"){
    if (is.na(lines[i])){
      stop("Parsing error. Could not find header 'Catch at age is summed over'")
    }
    i <- i + 1
  }
  
  # skip to table
  while (trimws(lines[i]) != "Catch at age (millions)"){
    if (is.na(lines[i])){
      stop("Parsing error. Could not find header 'Catch at age (millions)'")
    }
    fields <- strsplit(lines[i], " ")[[1]]
    if (trimws(fields[1]) == "years:"){
      output$year <- as.integer(fields[2])
    }
    if (trimws(fields[1]) == "areas:"){
      output$areas <- as.integer(fields[2:length(fields)])
    }
    if (trimws(fields[1]) == "gears:"){
      output$fdirGears <- convertGears(as.integer(fields[2:length(fields)]))
    }
    if (trimws(fields[1]) == "seasons:"){
      output$seasons <- as.integer(fields[2:length(fields)])
    }
    if (trimws(fields[1]) == "total" & trimws(fields[2]) == "used" & trimws(fields[3]) == "catch:" & trimws(fields[5]) == "tons"){
      output$totaltons <- as.numeric(fields[4])
    }
    
    
    i <- i + 1
  }
  
  #extract table
  i <- i + 1
  header <- strsplit(lines[i], "\t")[[1]]
  i <- i + 1
  datafirst <- i
  while (trimws(lines[i]) != "Mean age in catch"){
    if (is.na(lines[i])){
      stop("Parsing error. Could not find header 'Mean age in catch'")
    }
    datalast <- i
    i <- i + 1
  }
  data <- lines[datafirst:datalast]
  table <- data.frame(do.call(rbind, strsplit(data, "\t")), stringsAsFactors = F)
  names(table)  <- header
  table[["age"]] <- as.character(table[["age"]])
  table[["mean"]] <- as.numeric(table[["mean"]])
  table[["sd"]] <- as.numeric(table[["sd"]])
  table[["5%"]] <- as.numeric(table[["5%"]])
  table[["95%"]] <- as.numeric(table[["95%"]])
  
  if ("+" %in% strsplit(table$age[nrow(table)], "")[[1]]){
    output$plusgr <- as.integer(gsub("+", "", table$age[nrow(table)], fixed = T))
  }
  
  output$table <- table
  
  
  return(output)
}

#' run Stox project with the prediction only for cells defined by the given seasen, gears and ares
runStoxReca <- function(stoxProjectName, seasons, areas, gears, plusgr, force=F){
  
  #run stox and extract parameters and landings
  
  prepdata <- loadProjectData(stoxProjectName, var = "prepareRECA")
  rundata <- loadProjectData(stoxProjectName, var = "runRECA")
  
  if (!is.null(prepdata$prepareRECA) & !is.null(rundata$runRECA) & !force){
    warning(paste("Using previous run of project:", stoxProjectName, "(set force=T, to force new run)"))
  }
  else{
    runRScripts(stoxProjectName)
    prepdata <- loadProjectData(stoxProjectName, var = "prepareRECA")
    rundata <- loadProjectData(stoxProjectName, var = "runRECA")
  }
  
  
  if (is.null(prepdata) | is.null(rundata)) {
    stop("Could not load project data")
  }
  
  prepareRECA <- prepdata$prepareRECA
  projectlandings <- prepareRECA$StoxExport$landing
  
  #annotate quarter as integer 1-4
  projectlandings$Q <- ((as.integer(substr(projectlandings$sistefangstdato, 6,7)) -1 )%/% 3 + 1)
  
  projecttempres <- prepareRECA$StoxExport$temporalresolution
  AgeLength <- prepareRECA$AgeLength
  WeightLength <- prepareRECA$WeightLength
  runRECA <- rundata$runRECA
  GlobalParameters <- runRECA$GlobalParameters
  
  #
  # filter landings, and run new prediction
  #
  filteredlandings <- projectlandings[projectlandings$hovedområdekode %in% areas & 
                                       projectlandings$redskapkode %in% gears &
                                       projectlandings$Q %in% seasons,]
  decompLandings <- Rstox:::getLandings(filteredlandings, AgeLength, WeightLength, projecttempres)
  pred <- eca.predict(AgeLength, WeightLength, decompLandings, GlobalParameters)
  catchmatrix <- Rstox:::getCatchMatrix(pred, plusgr=plusgr, var = "Abundance",
                                        unit = "millions")
  catchmatrix$means$order <- 1:nrow(catchmatrix$means)
  decompmatrix <- merge(catchmatrix$means, catchmatrix$cv)
  decompmatrix <- decompmatrix[order(decompmatrix$order),]
  decompmatrix$age <- as.character(decompmatrix$age)
  decompmatrix$order <- NULL
  
  output <- list()
  output$table <- decompmatrix
  output$totaltons <- sum(filteredlandings$rundvekt) / 1000
  return(output)
}

# Plot estimates from oldEca and stoxReca on top of each other
plotComparison <- function(oldEca, stoxReca){
  comp <- merge(oldEca$table, stoxReca$table, by="age", suffixes=c(".oldEca", ".stoxReca"))
  ymax <- max(oldEca$table$mean + oldEca$table$sd, stoxReca$table$mean + stoxReca$table$sd)
  plot(comp$age, comp$mean.oldEca, ylim=c(0,ymax), xlab="age", ylab="millions +- SD", main=paste(format(oldEca$totaltons, digits=2), "t (old ECA),", format(stoxReca$totaltons, digits=2), "t (StoX-Reca)"))
  segments(as.numeric(comp$age), comp$mean.oldEca + comp$sd.oldEca, y1=comp$mean.oldEca - comp$sd.oldEca)
  points(comp$age, comp$mean.stoxReca, col="red")
  segments(as.numeric(comp$age), comp$mean.stoxReca + comp$sd.stoxReca, y1=comp$mean.stoxReca - comp$sd.stoxReca, col="red")
  legend("topright", legend=c("old ECA", "StoX-Reca"), fill=c("black", "red"))
}


#' Perform comparison
#' @details 
#'  Locate report file from ECA 3.x or ECA 4.x, formatted as the included example (oldEcaReportExample.txt)
#'  As in the example, the report file may be a report for some subset of landings (sepcific area, gear and quarter for instance)
#'  Prepare a Stox project with the same configuration
#'  run this funnction.
#'  
#'  The resulting plot is a a prediction for the same subset of landings that the given ECA 3.x or ECA 4.x report contains.
#' @param oldEcaOutputFile path to the ECA 3.x or ECA 4.x report file
#' @param stoxProjectName name of stox project located in standard directory
#' @param force logical() whether to force re-run of stox project. If false, any existing data preparations in the project will be used.
compare <- function(oldEcaOutputFile, stoxProjectName, force=F){
  oldEca <- parseOldEca(oldEcaOutputFile)
  stoxReca <- runStoxReca(stoxProjectName, oldEca$seasons, oldEca$areas, oldEca$fdirGears, oldEca$plusgr, force=force)
  plotComparison(oldEca, stoxReca)
}

compare("oldEcaReportExample.txt", "ECA_NSSK_sei_2018")