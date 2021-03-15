# adjust factor for roundweight based on producttype, gear, size etc.

#' Recalculates live weight (round weight) for processed products
#' @details
#'  Live weight is obtained by applying a scalar conversion factor to weights of partially proceesed product
#' @param landings sales notes to be adjusted
#' @param conversionfactor conversion factor to apply.
#' @param FAOspecies specification of the species to apply factor to. FAO 3-alpha species code
#' @param vesselRange vector of two numeric values denoting the lower range (inclusive) and the upper range (exclusive) of vessel lengths that factor should be applied for. If NULL the factor is applied to all vessel lengths
#' @param months vecotr of month indecies (1 for january) that the conversion factor should be applied for. If NULL the factor is applied to all months
#' @export
adjustConversionFactor <- function(landings, conversionfactor, FAOspecies="COD", vesselRange=c(0,28), months=c(1,2,3,4)){

  if (!is.null(vesselRange)){
    stop("Implement")
  }
  if (!is.null(months)){
    stop("Implement")
  }

}
