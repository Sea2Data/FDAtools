#' Adjust conversion factor
#' @description
#'  Recalculates live weight (round weight) in landings for processed (gutted) fish-products
#' @details
#'  Live weight is obtained by applying a scalar conversion factor to weights of partially proceesed product,
#'  such as 'gutted fish with head' or 'gutted fish without head'.
#'
#'  This function supports applying a conversion factor to only part of the landings based on a range of
#'  selection criteria specified as arguments to this function.
#'
#'  Missing lengths typically reflect unregistered vessels, which are typically small vessels.
#'  Conversion factors can be applied to these with the option 'missingLengths'
#'
#' @param landings sales notes to be adjusted
#' @param conversionfactor conversion factor to apply.
#' @param productTypes product types (Produkttilstand NS9400) that the conversion should be applied
#' @param FAOspecies specification of the species to apply factor to. FAO 3-alpha species code
#' @param vesselRange vector of two numeric values denoting the lower range (inclusive) and the upper range (exclusive) of vessel lengths that factor should be applied for. If NULL the factor is applied to all vessel lengths
#' @param missingLengths if TRUE, apply conversion for vessels with missing lengths
#' @param months vector of month indecies (1 for january) that the conversion factor should be applied for. If NULL the factor is applied to all months
#' @param onlyNorth62 if TRUE, conversion is only applied north of 62nd longitude
#' @param vesselNationalities vector of 3-alpha nationality codes specifying which vessels the conversion should be applied for. If NULL it is applied to all vessels
#' @return landings with the column 'RUNDVEKT' modified for selected lines, so that RUNDVEKT = PRODUKTVEKT * conversionfactor
#' @examples
#'  landingsfile <- system.file("testresources", "codLandings.rda", package="stox2.7preprocessing")
#'  landings <- readRDS(landingsfile)
#'
#'  # convert gutted without head (SLUH), codes 211 and 214
#'  adjustedLandingsSLUH <- adjustConversionFactor(landings, 1.671, c(211, 214))
#'  # convert also for gutted with head (SLMH), code 210
#'  adjustedLandingsSL <- adjustConversionFactor(adjustedLandingsSLUH, 1.311, 210)
#' @export
adjustConversionFactor <- function(landings, conversionfactor, productTypes, FAOspecies="COD", vesselRange=c(0,28), missingLengths=T, months=c(1,2,3,4), onlyNorth62=T, vesselNationalities=c("NOR")){

  filter <- rep(T, nrow(landings))
  if (!missingLengths){
    filter <- filter & !is.na(landings[["St\u00F8rsteLengde"]])
  }
  if (!is.null(vesselRange)){
    filter[!is.na(landings[["St\u00F8rsteLengde"]])] <- filter[!is.na(landings[["St\u00F8rsteLengde"]])] &
                              landings[["St\u00F8rsteLengde"]][!is.na(landings[["St\u00F8rsteLengde"]])] >= min(vesselRange) &
                              landings[["St\u00F8rsteLengde"]][!is.na(landings[["St\u00F8rsteLengde"]])] < max(vesselRange)
  }
  if (!is.null(months)){
    dates <- as.POSIXct(landings$SisteFangstdato, format="%d.%m.%Y")
    filter <- filter & month(dates) %in% months
  }
  if (!is.null(vesselNationalities)){
    filter <- filter & landings[["Fart\u00F8ynasjonalitet_kode"]] %in% vesselNationalities
  }
  if (onlyNorth62){
    filter <- filter & landings[["NordS\u00F8rFor62GraderNord"]] == "Nord for 62\u00B0N"
  }
  if (is.null(productTypes)){
    stop("ProductTypes must be provided")
  }
  else{
    filter <- filter & (landings$Produkttilstand_kode %in% productTypes)
  }

  if (!any(filter)){
    warning("No landings matches criteria for adjusting conversion factor.")
  }

  landings$Rundvekt[filter] <- landings$Produktvekt[filter]*conversionfactor

  return(landings)
}
