
#' compare landings
#' @description
#'  Make table comparing two sets of sales notes by gear and area
#' @param original sales notes to be treatad as original
#' @param modified sales notes to be treated as modifed
#' @param includeGear if True, comparisons are done by gear as well as area
#' @return table
#' @export
compareLandings <- function(original, modified, includeGear=T){
  if (includeGear){
    originalTab <- original[,.(original.weight=sum(get("Rundvekt"))), by=.(get("Redskap_bokm\u00E5l"), get("Redskap_kode"), get("Hovedomr\u00E5de_kode"))]
    modifiedTab <- modified[,.(modified.weight=sum(get("Rundvekt"))), by=.(get("Redskap_bokm\u00E5l"), get("Redskap_kode"), get("Hovedomr\u00E5de_kode"))]
  }
  else{
    originalTab <- original[,.(original.weight=sum(get("Rundvekt"))), by=.(get("Hovedomr\u00E5de_kode"))]
    modifiedTab <- modified[,.(modified.weight=sum(get("Rundvekt"))), by=.(get("Hovedomr\u00E5de_kode"))]
  }
  tab <- merge(originalTab, modifiedTab)
  tab$difference <- tab$modified.weight - tab$original.weight
  tab$rel.difference <- tab$difference / tab$original.weight
  tab <- tab[order(abs(tab$difference), decreasing = T)]
  return(tab)
}
