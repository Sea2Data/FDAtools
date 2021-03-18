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
  landings$Hovedområde_kode[landings$Hovedområde_kode=="00"] <- "s000"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="03" & landings$KystHav_kode == 8] <- "s301"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="03" & landings$KystHav_kode == 0] <- "s300"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="04" & landings$KystHav_kode == 8] <- "s401"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="04" & landings$KystHav_kode == 0] <- "s400"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="05" & landings$KystHav_kode == 8] <- "s501"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="05" & landings$KystHav_kode == 0] <- "s500"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="06" & landings$KystHav_kode == 8] <- "s601"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="06" & landings$KystHav_kode == 0] <- "s600"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="07" & landings$KystHav_kode == 8] <- "s701"
  landings$Hovedområde_kode[landings$Hovedområde_kode=="07" & landings$KystHav_kode == 0] <- "s700"

  if (!is.null(landings$FOlat)){
    # set area from position when available
    landingsWpos <- RstoxFDA::appendAreaCode(landingsWpos, stox2.7preprocessing::coastalCodAreas, "FOlat", "FOlon", "areaFromPos", polygonName = "polygonName")
    landingsWpos$Hovedområde_kode <- landingsWpos$areaFromPos
    landingsWpos$areaFromPos <- NULL

    landings <- rbind(landings, landingsWpos)
  }
  return(landings)
}
