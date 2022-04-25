library(RstoxAPI)
projectpath <- "~/workspace/stox3examples/wklifeALK/ECA_CC_South_2017/"

runProject <- function(projectpath){
  results <- list()
  results$baseline <- RstoxAPI::runModel(projectpath, "baseline", force.restart = T)
  results$analysis <- RstoxAPI::runModel(projectpath, "analysis", force.restart = T)
  results$report <- RstoxAPI::runModel(projectpath, "report", force.restart = T)
  return(results)
}

#' Extract ALK for WKLIFE data call in 2022
#' @param ReportRecaCatchAtLengthAndAge report-process that produced the ALK (RstoxFDA::ReportRecaCatchAtLengthAndAge)
#' @param PrepareRecaEstimate analysis-process (RstoxFDA::PrepareRecaEstimate) that prepared input for the eca estimate.
#' @param Stock the stock to extract ALK for
extractALK <- function(ReportRecaCatchAtLengthAndAge, PrepareRecaEstimate, stock="Coastal cod", Psu){
  if ("Stock" %in% names(ReportRecaCatchAtLengthAndAge$NbyLengthAge)){
    if (!(stock %in% ReportRecaCatchAtLengthAndAge$NbyLengthAge$Stock)){
      stop(paste(stock, "is not a valid stock for this estimate. Valid stocks are:", unique(ReportRecaCatchAtLengthAndAge$NbyLengthAge$Stock)))
    }
    stockAlk <- ReportRecaCatchAtLengthAndAge$NbyLengthAge[ReportRecaCatchAtLengthAndAge$NbyLengthAge$Stock==stock]
  }
  else{
    if (!is.null(stock)){
      stop("Argument 'Stock' provided, but ALK is not splitted on Stock.")
    }
    stop("Only imlemented for stock splitted ALKs")
    stockAlk <- ReportRecaCatchAtLengthAndAge$NbyLengthAge
  }

  if (stock == PrepareRecaEstimate$CovariateMaps$CovariateMaps_StockSplitting_StockNameCC){
    stockCodes <- c(1,2)
  }
  else if (stock == PrepareRecaEstimate$CovariateMaps$CovariateMaps_StockSplitting_StockNameS){
    stockCodes <- c(4,5)
  }
  else{
    stop("Could not determine stock codes")
  }
  
  #
  # re-normalising ALK to reflect approximate number of samples of stock
  #
  N <- sum(stockAlk$CatchAtAgeLength)
  n <- sum(!is.na(PrepareRecaEstimate$AgeLength$DataMatrix$otolithtype) & PrepareRecaEstimate$AgeLength$DataMatrix$otolithtype %in% stockCodes)
  scale <- n/N
  stockAlk$CatchAtAgeLength <- stockAlk$CatchAtAgeLength*scale
  stockAlk$SD <- stockAlk$SD*scale
  stockAlk$High <- stockAlk$High*scale
  stockAlk$Low <- stockAlk$Low*scale
  
  return(stockAlk)
}

results <- runProject(projectpath)
alk <- extractALK(results$report$ReportALK, results$analysis$PrepareReca)

# remove cells with very high pr-cell CV.
alk <- alk[alk$CatchAtAgeLength>0,]
alk$CV <- alk$SD/alk$CatchAtAgeLength
alk <- alk[alk$CV < 1]

#inspect alk
ggplot2::ggplot(alk, ggplot2::aes_string(x="Age", y="Length", fill="CatchAtAgeLength")) + ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "white", high = "red")

#format output
alk$assessment_group <- "AFWG"
alk$stock <- "cod.27.2.coastS"
alk$source_of_data <- "commercial"
alk$country <- "NO"
alk$year <- results$baseline$BioticRead$biotic_year_2017_species_164712.xml$mission$startyear[1]

alk$season_type <- "Year"
alk$temporal_stratum <- 1

alk$spatial_stratum <- "27.2.a.2"
alk$fleet_stratum <- "All"
alk$catch_category <- "L"
alk$unit_length <- RstoxData::getUnit(results$report$ReportALK$NbyLengthAge$Length, property = 'symbol')
alk$length_class <- alk$Length
alk$age <- alk$Age
alk$number_of_fish <- format(alk$CatchAtAgeLength, digits=2)

# setting PSU to vessel (reference-fleets)
alk$unit_number_of_samples <- "vessel"
alk$number_of_samples <- length(unique(results$baseline$StoxBioticFinalFilter$Station$CatchPlatform))

alk$info_to_stock_coordinator <- paste(alk$LengthGroup, "CV:", format(alk$CV, digits=2), "Renormalized estimate from mixed multi-stage sampling programmes.")

output <- alk[, c("assessment_group", "stock", "source_of_data", "country", "year", "season_type", "temporal_stratum", "spatial_stratum", "fleet_stratum", "catch_category", "unit_length", "length_class", "age", "number_of_fish", "unit_number_of_samples", "number_of_samples", "info_to_stock_coordinator")]
data.table::fwrite(output, "~/temp/alk.csv", sep = ";")
