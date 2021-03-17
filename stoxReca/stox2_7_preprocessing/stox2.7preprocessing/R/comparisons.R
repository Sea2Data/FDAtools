
#' compare landings
#' @description
#'  Make table comparing two sets of sales notes by gear and area
#' @param original sales notes to be treatad as original
#' @param modified sales notes to be treated as modifed
#' @param incldueGear if True, comparisons are done by gear as well as area
#' @return table
#' @export
compareLandings <- function(original, modified, includeGear=T){
  if (includeGear){
    originalTab <- original[,.(original.weight=sum(Rundvekt)), by=.(Redskap_bokmål, Redskap_kode, Hovedområde_kode)]
    modifiedTab <- modified[,.(modified.weight=sum(Rundvekt)), by=.(Redskap_bokmål, Redskap_kode, Hovedområde_kode)]
  }
  else{
    originalTab <- original[,.(original.weight=sum(Rundvekt)), by=.(Hovedområde_kode)]
    modifiedTab <- modified[,.(modified.weight=sum(Rundvekt)), by=.(Hovedområde_kode)]
  }
  tab <- merge(originalTab, modifiedTab)
  tab$difference <- tab$modified.weight - tab$original.weight
  tab$rel.difference <- tab$difference / tab$original.weight
  tab <- tab[order(abs(tab$difference), decreasing = T)]
  return(tab)
}
