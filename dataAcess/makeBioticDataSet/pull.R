library(httr)

#' Download data sets from NMD biotic
#' @description 
#'  Downloads a data set from NMD biotic v3, 
#'  and saves it to a file as XML (namespace: http://www.imr.no/formats/nmdbiotic/v3)
#' @param targetfile filename to save dataset to
#' @param missiontype missiontype name for data set.
#' @param year year for data set
#' @param platform platform path for data set
#' @param delivery delivery identifier for data set
#' @param url address to NMD biotic v.3 (defaults to production address pr. Dec 2019)
#' @param port port to use (defaults 8080)
#' @examples 
#'  \dontrun{pullDataSet("test.xml", "Forskningsfartøy",2018,"Kronprins Haakon_3YYQ","2018707")}
pullDataSet <- function(targetfile, missiontype, year, platform, delivery, url="http://tomcat7.imr.no", port=8080){
  
  if (file.exists(targetfile)){
    stop(paste("File", targetfile, "already exists"))
  }
  
  basepath <- "apis/nmdapi/biotic/v3"
  missiontypepath <- URLencode(paste(basepath, missiontype, sep="/"))
  yearpath <- URLencode(paste(basepath, missiontype, year, sep="/"))
  platformpath <- URLencode(paste(basepath, missiontype, year, platform, sep="/"))
  datasetpath <- URLencode(paste(basepath, missiontype, year, platform, delivery, "dataset", sep="/"))
  
  hasmissiontype <- httr::GET(url, port=port, path=missiontypepath)
  if (is.null(missiontype) | hasmissiontype$status_code != 200){
    if (hasmissiontype$status_code == 404){
      stop(paste("Missiontype", missiontype, "not found in API. Check available missiontypes with:", paste(url, ":", port, "/", basepath, sep="")))
    }
    else{
      stop("Error retrieving data")    
    }
  }
  
  hasyear <- httr::GET(url, port=port, path=yearpath)
  if (is.null(year) | hasyear$status_code != 200){
    if (hasyear$status_code == 404){
      stop(paste("Missiontype:", missiontype, "year:", year, "not found in API. Check available years with:", paste(url, ":", port, "/", missiontypepath, sep="")))
    }
    else{
      stop("Error retrieving data")
    }
  }
  
  hasplatform <- httr::GET(url, port=port, path=platformpath)
  if (is.null(platform) | hasplatform$status_code != 200){
    if (hasplatform$status_code == 404){
      stop(paste("Missiontype:", missiontype, "year:", year, "Platform:", platform, "not found in API. Check available platform paths with:", paste(url, ":", port, "/", yearpath, sep="")))  
    }
    else{
      stop("Error retrieving data")    
    }
  }
  
  mission <- httr::GET(url, port=port, path=datasetpath, query="version=3.0")
  if (is.null(delivery) | mission$status_code != 200){
    if (mission$status_code == 404){
      stop(paste("Missiontype:", missiontype, "year:", year, "Platform:", platform, "delivery:", delivery, "not found in API. Check available platform paths with:", paste(url, ":", port, "/", datasetpath, sep="")))
    }
    else{
      stop("Error retrieving data")    
    }
  }
  
  content <- strsplit(rawToChar(mission$content), "\n")[[1]]
  starttag <- "<missions xmlns=\"http://www.imr.no/formats/nmdbiotic/v3\">"
  endtag <- "</missions>"
  
  write(content[[1]], file=targetfile)
  write(starttag, file=targetfile, append = T)
  if (length(content) > 1){
    for (i in 2:length(content)){
      write(content[[i]], file=targetfile, append = T)
    }
  }
  write(endtag, file=targetfile, append = T)
  
}

#' Download a cached yearfile
#' @description 
#'  Downloads a cached yearfile containing all biotic data for a year
#'  as XML (namespace: http://www.imr.no/formats/nmdbiotic/v3)
#' @param targetfile filename to save year file to
#' @param year year to retrieve data for
#' @param overwrite logical() whether to overwrite any existing file
#' @param url address to NMD biotic v.3 (defaults to production address pr. Dec 2019)
#' @param port port to use (defaults 8080)
#' @examples 
#'  pullYearFile("test1976.txt", 1976)
pullYearFile <- function(targetfile, year, overwrite=F, url="http://tomcat7.imr.no", port=8080){
  
  if (file.exists(targetfile) & !overwrite){
    stop(paste("File", targetfile, "already exists."))
  }
  
  basepath <- "apis/nmdapi/biotic/v3"
  yearpath <- URLencode(paste(basepath, year, "cache", sep="/"))
  
  yeardata <- httr::GET(url, port=port, path=yearpath, query="version=3.0")
  
  if (yeardata$status_code == 200){
    writeBin(yeardata$content, con=targetfile)
  }
  else if (yeardata$status_code == 404){
    stop(paste("Found no cached data for year", year, "in API."))
  }
  else{
    stop(paste("Error when downloading data. Status:", yeardata$status_code))
  }
}

#' Download cached yearfiles
#' @description 
#'  Downloads a cached yearfiles containing all biotic data for a year
#'  as XML (namespace: http://www.imr.no/formats/nmdbiotic/v3)
#' @details 
#'  downloaded files will be stored in the target directory with the name <basename>_year.xml
#' @param targetdirectory directory to save year files to
#' @param years vector with years to retrieve data for
#' @param basename base name for files, see details.
#' @param overwrite logical() whether to overwrite existing files.
#' @param url address to NMD biotic v.3 API (defaults to production address pr. Dec 2019)
#' @param port port to use (defaults 8080)
#' @examples 
#'  pullYears(".", 1978:1980)
pullYears <- function(targetdirectory, years, basename="biotic_year", overwrite=F, url="http://tomcat7.imr.no", port=8080){
  if (!file.exists(targetdirectory)){
    stop(paste("Directory", targetdirectory, "does not exist."))
  }
  if (!file_test("-d", targetdirectory)){
    stop(paste("File", targetdirectory, "is not a directory."))
  }
  filenames <- paste(paste(basename, years, sep="_"), "xml", sep=".")
  if (any(filenames %in% list.files(targetdirectory)) & !overwrite){
    stop(paste("Some files already exists in ", targetdirectory, ": ", paste(filenames[filenames %in% list.files(targetdirectory)], collapse=", "), sep=""))
  }
  
  for (year in years){
    tfile <- file.path(targetdirectory, paste(paste(basename, year, sep="_"), "xml", sep="."))
    write(paste("Downloading:", tfile), stdout())
    pullYearFile(targetfile = tfile, year = year, overwrite = overwrite, url = url, port = port)
  }
  return(file.path(targetdirectory, filenames))
}