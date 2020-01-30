library(data.table)

#' @noRd
mergeTable <- function(flatBiotic, table, columns, keys, prune=F){
  
  if (any(columns %in% names(table))){
    if (is.null(flatBiotic)){
      return(table[,unique(c(keys, columns[columns %in% names(table)])), with=F])
    }
    else{
      return(merge(table[,unlist(unique(c(keys, columns[columns %in% names(table)]))), with=F], flatBiotic, by=keys, all.x=!prune))
    }
  }
  if (!is.null(flatBiotic)){
    return(merge(table[,keys, with=F], flatBiotic, by=keys))    
  }
  return(NULL)
}

#' Make tabular view of biotic data
#' @description 
#'  produces a single 2D table with biotic data.
#' @details
#'  the columns that should be included in the view is provided in the argument 'columns'.
#'  (field names are unqiue across levels in biotic 3)
#'  Key columns are autmatically included. Do not specify these.
#'  Only levels implied by these columns are included.
#'  
#'  Pruning: pruning results means to exclude higher levels when lower leves are not present.
#'  That is, if individual level is included, should catchsamples with no individuals sampled be kept in the data set.
#' 
#'  relationalBiotic could be obtained by RstoxData::readXmlFile, or any other parser
#'  that produces the following format:
#'  relationalBiotic should be a named list containing data.tables.
#'  each table should be named as corresponding elements in biotic 3 (namespace: http://www.imr.no/formats/nmdbiotic/v3)
#'  all columns should be named as corresponding elements and attributes in biotic 3.
#'  
#'  age parameters are considered part of the individual level,
#'  and views will be constrainted to one age reading for each individual,
#'  although biotic3 supports several.
#'  
#'  views does not allow columns from different branches of the hiearchy in the same view
#'  (apart from age reading, which is constrained to only one per individual).
#'  E.g. can not include columns from both prey and tag, or from both preylengthfrequencytable and copepodedevstagefrequencytable
#' @param relationalBiotic list() of tabular representation of the different levels in biotic
#' @param columns name of columns to keep (in addition to key columns)
#' @param prune logical, if TRUE, whether to prune results.
#' @return data.table with the requested columns, in addition to any key columns.
makeTabularView <- function(relationalBiotic, columns, prune=F){
  
  allColumnNames <- c(names(relationalBiotic$agedetermination), 
                      names(relationalBiotic$catchsample), 
                      names(relationalBiotic$copepodedevstagefrequencytable),
                      names(relationalBiotic$fishstation),
                      names(relationalBiotic$individual),
                      names(relationalBiotic$mission),
                      names(relationalBiotic$prey),
                      names(relationalBiotic$preylengthfrequencytable),
                      names(relationalBiotic$tag))
  
  if (!all(columns %in% allColumnNames)){
    stop("Some specified columns does not exist i format: ", paste(columns[!(columns %in% allColumnNames)], collapse=","))
  }
  
  #biotic 3 key structure
  missionkeys <- c("missiontype", "startyear", "platform", "missionnumber")
  stationkeys <- c(missionkeys, "serialnumber")
  catchkeys <- c(stationkeys, "catchsampleid")
  individualkeys <- c(catchkeys, "specimenid")
  agedetkeys <- c(individualkeys, "agedeterminationid")
  tagkeys <- c(individualkeys, "tagid")
  preykeys <- c(individualkeys, "preysampleid")
  preylengthfrequencykeys <- c(preykeys, "preylengthid")
  copepodedevstagefrequencykeys <- c(preykeys, "copepodedevstage")
  
  #keys are automatically added where appropriate.
  columns <- columns[!(columns %in% c(agedetkeys, tagkeys, preylengthfrequencykeys, copepodedevstagefrequencykeys))]
  
  if (any(columns %in% names(relationalBiotic$agedetermination))){
    #set prefferred age reading where missing
    ageids <- unique(relationalBiotic$agedetermination[,agedetkeys, with=F])
    ageids <- ageids[!duplicated(ageids[,individualkeys, with=F])]
    relationalBiotic$individual <- merge(relationalBiotic$individual, ageids, by=individualkeys, all.x=T)
    relationalBiotic$individual[is.na(relationalBiotic$individual$preferredagereading),"preferredagereading"] <- relationalBiotic$individual[is.na(relationalBiotic$individual$preferredagereading),][["agedeterminationid"]]
    relationalBiotic$individual$agedeterminationid <- NULL
  }
  
  if (any(columns %in% names(relationalBiotic$tag))){
    if (any(columns %in% names(relationalBiotic$copepodedevstagefrequencytable)) |
        any(columns %in% names(relationalBiotic$preylengthfrequencytable)) |
        any(columns %in% names(relationalBiotic$prey))){
      stop("Cannot view several branches of the hiearchy in one view. Columns from tag, may not be comibined with prey-related columns.")
    }
  }
  
  if (any(columns %in% names(relationalBiotic$copepodedevstagefrequencytable)) &
      any(columns %in% names(relationalBiotic$preylengthfrequencytable))){
    stop("Cannot view several branches of the hiearchy in one view. columns from copepodedevstagefrequencytable, may not be comibined with columns from preylengthfrequencytable")
  }
  
  # These can only occur as leaf tables
  flatbiotic <- NULL
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$tag, columns, tagkeys, prune)
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$copepodedevstagefrequencytable, columns, copepodedevstagefrequencykeys, prune)
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$preylengthfrequencytable, columns, preylengthfrequencykeys, prune)
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$prey, columns, preykeys, prune)
  if (!any(columns %in% names(relationalBiotic$agedetermination))){
    flatbiotic <- mergeTable(flatbiotic, relationalBiotic$individual, columns, individualkeys, prune)
  }
  else if (any(columns %in% names(relationalBiotic$agedetermination))){
    agedet <- mergeTable(NULL, relationalBiotic$agedetermination, columns, agedetkeys)
    ind <- relationalBiotic$individual
    ind$agedeterminationid <- ind$preferredagereading
    ind <- merge(relationalBiotic$individual, agedet, all.x=T)
    flatbiotic <- mergeTable(flatbiotic, ind, columns, agedetkeys, prune)
  }
  else{
    stop()
  }

  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$catchsample, columns, catchkeys, prune)
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$fishstation, columns, stationkeys, prune)
  flatbiotic <- mergeTable(flatbiotic, relationalBiotic$mission, columns, missionkeys, prune)

  return(flatbiotic)
}

#' Custom view of individuals, with common additions from catch level and age reading level
individualView <- function(relationalBiotic, additional=c("catchcategory", "catchpartnumber", "commonname", "scientificname", "age", "otolithtype")){
  return(makeTabularView(relationalBiotic, c(names(relationalBiotic$individual), additional), prune=T))
}

#' Custum view with all columns from mission, fishstation, catchsample, indvidual and agedetermination
stdview <- function(relationalBiotic){
  return(makeTabularView(relationalBiotic, c(names(relationalBiotic$agedetermination), names(relationalBiotic$individual), names(relationalBiotic$catchsample), names(relationalBiotic$fishstation), names(relationalBiotic$mission)), prune=F))
}

#' Custum view with all columns from mission, fishstation, and catchsample
catchview <- function(relationalBiotic){
  return(makeTabularView(relationalBiotic, c(names(relationalBiotic$catchsample), names(relationalBiotic$fishstation), names(relationalBiotic$mission)), prune=F))
}


#
# data set examples
# some of these are really fisheries independent data, but derve as useful examples still.
#

#' data set for looking up mission level columns based on serialnumber and year
#' for merging with SPD-dependent scripts
#' See pull.R for getting data files
#' See RstoxData for parsing routines: https://github.com/StoXProject/RstoxData
makeCapelinMissionLookup <- function(datafiles = paste("biotic_year_", 1970:2018, ".xml", sep="")){
  require("RstoxData")
  result <- NULL
  
  if (!all(file.exists(datafiles))){
    stop("Some specified files does not exist: ", paste(datafiles[!file.exists(datafiles)], collapse = ", "))
  }
  
  for (f in datafiles){
    data <- RstoxData::readXmlFile(f)
    flatdata <- makeTabularView(data, c(names(data$mission), "serialnumber", "commonname", "scientificname", "catchcategory"))
    flatdata <- flatdata[!is.na(flatdata$catchcategory) & flatdata$catchcategory == "162035",]
    if (is.null(result)){
      result <- flatdata
    }
    else{
      result <- rbind(result, flatdata)
    }
  }
  return(result)
}

#' Extract catches from a cruise series
#' See pull.R for getting data files
#' See Rstox for routines for looking up cruise info: https://github.com/Sea2Data/Rstox
#' See RstoxData for parsing routines: https://github.com/StoXProject/RstoxData
makeCruiseSet <- function(cruise="Barents Sea NOR-RUS demersal fish cruise in winter", years=2013:2018, datafiles = paste("biotic_year_", years, ".xml", sep="")){
  require("Rstox")
  require("RstoxData")
  require("data.table")
  cruiselist <- getNMDinfo("cs")
  
  survey <- cruiselist[[cruise]][cruiselist[[cruise]]$Year %in% years,]
  survey$Year <- as.integer(survey$Year)
  result <- NULL
  for (f in datafiles){
    if (!file.exists(f)){
      stop(paste("File",f,"does not exist."))
    }
    data <- RstoxData::readXmlFile(f, stream = T)
    flatdata <- catchview(data)
    
    #drop data that is not in any cruise series
    flatdata <- flatdata[!is.na(flatdata$cruise),]
    #keep only the data from 'survey'
    flatdata <- merge(flatdata, as.data.table(survey[,c("Cruise", "Year")]), by.x=c("cruise", "startyear"), by.y=c("Cruise", "Year"))
    
    result <- rbind(flatdata, result)
  }
  return(result)
  
}