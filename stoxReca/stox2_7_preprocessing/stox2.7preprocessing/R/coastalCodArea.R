# recode area based on mainarea and coastal/offshore code

#' encode costal cod areas
#' @description
#'  encode costal cod areas in the column for main area
#' @details
#'  Coastal cod areas are defined as in \code{\link[stox2.7preprocessing]{coastalCodAreas}}
#'  They are inserted into the column 'Hovedområde_kode' based on its value and the value for the field
#'  column 'KystHav_kode' which indicate whether landings is from the coastal or off-shore side of the
#'  terretorial line (12 nm of coast).
#'
#'  If positions are available for some landings (see \code{\link[stox2.7preprocessing]{adjustWithLogbook}})
#'  these will be used for setting the area, rather than the columns 'Hovedområde_kode' and 'KystHav_kode'
#' @param landings landings as parsed by \code{\link[stox2.7preprocessing]{readLandings}}
#' @return landings with codes for coastal cod areas in the colunm 'Hovedområde_kode' where relevant
#' @export
encodeCostalCodArea <- function(landings){

  if (!is.null(landings$FOlat)){
    landingsWpos <- landings[!is.na(landings$FOlat) & !is.na(landings$FOlon),]
    landings <- landings[is.na(landings$FOlat) | is.na(landings$FOlon),]
  }

  #set area based on KystHav_kode when position is not available
  landings[landings[["Hovedomr\u00E5de_kode"]]=="00","Hovedomr\u00E5de_kode"] <- "s000"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="03" & landings$KystHav_kode == 8,"Hovedomr\u00E5de_kode"] <- "s301"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="03" & landings$KystHav_kode == 0,"Hovedomr\u00E5de_kode"] <- "s300"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="04" & landings$KystHav_kode == 8,"Hovedomr\u00E5de_kode"] <- "s401"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="04" & landings$KystHav_kode == 0,"Hovedomr\u00E5de_kode"] <- "s400"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="05" & landings$KystHav_kode == 8,"Hovedomr\u00E5de_kode"] <- "s501"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="05" & landings$KystHav_kode == 0,"Hovedomr\u00E5de_kode"] <- "s500"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="06" & landings$KystHav_kode == 8,"Hovedomr\u00E5de_kode"] <- "s601"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="06" & landings$KystHav_kode == 0,"Hovedomr\u00E5de_kode"] <- "s600"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="07" & landings$KystHav_kode == 8,"Hovedomr\u00E5de_kode"] <- "s701"
  landings[landings[["Hovedomr\u00E5de_kode"]]=="07" & landings$KystHav_kode == 0,"Hovedomr\u00E5de_kode"] <- "s700"

  if (!is.null(landings$FOlat)){
    # set area from position when available
    landingsWpos <- RstoxFDA::appendAreaCode(landingsWpos, stox2.7preprocessing::coastalCodAreas, "FOlat", "FOlon", "areaFromPos", StratumName = "polygonName")
    landingsWpos[["Hovedomr\u00E5de_kode"]] <- landingsWpos$areaFromPos
    landingsWpos$areaFromPos <- NULL
    landings <- rbind(landings, landingsWpos)
  }
  return(landings)
}
