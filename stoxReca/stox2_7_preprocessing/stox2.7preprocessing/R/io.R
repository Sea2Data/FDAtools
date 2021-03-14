#' Read landings as Stox 2.7 does it
#' Rename variables to Stox 3.0 conventions
#' Reformat sistefangstdato and landingsdato to StoX 3.0 convention
#' Keep flat format
#' @param filename for xml-formatted landings
#' @import Rstox
#' @import data.table
#' @export
readLandings <- function(landingsXML){
  landings <- data.table::data.table(Rstox::readXMLfiles(files=list(landing=landingsXML)))
  landings$updatedTime <- NULL
  landings$updatedBy <- NULL
  landings$insertedTime <- NULL
  landings$insertedBy <- NULL
  landings$fartøynasjonalitetKode.1 <- NULL
  landings$sisteFangstdato <- strftime(landings$sisteFangstdato, format="%d.%m.%Y")
  landings$landingsdato[landings$landingsdato!=""] <- strftime(landings$landingsdato[landings$landingsdato!=""], format="%d.%m.%Y")
  names(landings) <- c("Dokumentnummer", "Linjenummer",
                       "Art_kode", "Registreringsmerke_seddel",
                       "Fangstår", "SisteFangstdato",
                       "Redskap_kode", "HovedgruppeRedskap_kode",
                       "Hovedområde_kode", "Lokasjon_kode",
                       "StørsteLengde", "Fartøynasjonalitet_kode",
                       "KystHav_kode", "id",
                       "Dokumenttype_Kode", "Dokumenttype_Bokmål",
                       "DokumentVersjonsnummer", "DokumentFormulardato",
                       "DokumentElektroniskDato", "SalgslagID",
                       "Salgslag", "Salgslag_kode",
                       "Mottakernasjonalitet_kode", "Mottakernasjonalitet_bokmål",
                       "Mottaksstasjon", "Landingsdato",
                       "Landingsklokkeslett", "Landingskommune_kode",
                       "Landingskommune", "Landingsfylke_kode",
                       "Landingsfylke", "Landingsnasjon_kode",
                       "Landingsnasjon_bokmål", "Produksjonsanlegg",
                       "Produksjonskommune_kode", "Produksjonskommune",
                       "Fangstfelt_kode", "Hovedområde_bokmål",
                       "Sone_kode", "Sone_bokmål",
                       "Områdegruppering_bokmål", "HovedområdeFAO_kode",
                       "HovedområdeFAO_bokmål", "NordSørFor62GraderNord",
                       "Fangstdagbok_nummer", "Fangstdagbok_turnummer",
                       "Fiskerkommune_kode", "Fiskerkommune",
                       "Fiskernasjonalitet_kode", "Fiskernasjonalitet_bokmål",
                       "Fartøynavn", "FartøyID",
                       "Radiokallesignal_seddel", "Lengdegruppe_kode",
                       "Lengdegruppe_bokmål", "Bruttotonnasje1969",
                       "BruttotonnasjeAnnen", "Byggeår",
                       "Ombyggingsår", "Motorkraft",
                       "Motorbyggeår", "FartøyGjelderFraDato",
                       "FartøyGjelderTilDato", "Fartøytype_kode",
                       "Fartøytype_bokmål", "Fartøykommune_kode",
                       "Fartøykommune", "Fartøyfylke_kode",
                       "Fartøyfylke",
                       "Fartøynasjonalitet_bokmål", "MottakendeFartøyRegMerke",
                       "MottakendeFartøyRKAL", "MottakendeFartøytype_kode",
                       "MottakendeFartøytype_bokmål", "MottakendeFartøynasj_kode",
                       "MottakendeFartøynasj_bokmål", "KvotefartøyRegMerke",
                       "Kvotetype_kode", "Kvotetype_bokmål",
                       "Redskap_bokmål", "HovedgruppeRedskap_bokmål",
                       "Dellanding_signal", "NesteMottaksstasjon",
                       "ForrigeMottakstasjon", "Produkttilstand_kode",
                       "Produkttilstand_bokmål", "Konserveringsmåte_kode",
                       "Konserveringsmåte_bokmål", "Landingsmåte_kode",
                       "Landingsmåte_bokmål", "Kvalitet_kode",
                       "Kvalitet_bokmål", "Størrelsesgruppering_kode",
                       "Anvendelse_kode", "Anvendelse_bokmål",
                       "HovedgruppeAnvendelse_kode", "HovedgruppeAnvendelse_bokmål",
                       "AntallStykk", "Bruttovekt",
                       "Produktvekt", "Rundvekt",
                       "Art_bokmål", "ArtsgruppeHistorisk_kode",
                       "ArtsgruppeHistorisk_bokmål", "HovedgruppeArt_kode",
                       "HovedgruppeArt_bokmål", "ArtFAO_kode",
                       "ArtFAO_bokmål")
  return(landings)
}


#' @noRd
writeXmlDeclaration <- function(stream, version, encoding, standalone){

  if (standalone){
    standalone = "yes"
  }
  else{
    standalone = "no"
  }

  cat(paste0("<?xml version=\"", version, "\" encoding=\"", encoding, "\" standalone=\"", standalone, "\"?>\n"), file=stream)
}

#' @noRd
openTag <- function(stream, tagname, attributes=NULL, indent=""){

  tagstring <- paste0(indent, "<",tagname)
  if (!is.null(attributes)){
    #stopifnot(nrow(attributes)==1)

    for (n in names(attributes)){
      if (!is.na(attributes[[n]][[1]])){
        tagstring <- paste(tagstring, paste0(n,"=\"",attributes[[n]][[1]],"\""))
      }
    }
  }
  tagstring <- paste0(tagstring, ">")
  writeLines(tagstring, con=stream)
}

#' @noRd
closeTag <- function(stream, tagname, indent=""){
  tagstring <- paste0(indent, "</",tagname, ">")
  writeLines(tagstring, con=stream)
}

#' @noRd
writeSimpleTags <- function(stream, tags, indent=""){
  string <- ""
  for (n in names(tags)){
    if (!is.na(tags[[n]][[1]])){
      string <- paste0(string, "<",n,">",tags[[n]][[1]],"</",n,">")
    }
  }
  writeLines(paste0(indent, string), con=stream)
}


#' @noRd
writeLevel <- function(stream, data, level, parentKeys, xsdObject, indent="", namespace=""){

  # handle root
  if (level == xsdObject$root){
    #stopifnot(is.null(parentKeys))
    openTag(stream, level, data.table::data.table(xmlns=namespace), indent)
    for (sub in xsdObject$treeStruct[[level]]){
      writeLevel(stream, data, sub, NULL, xsdObject, paste0(indent, "\t"))
    }
    closeTag(stream, level, indent)
    return()
  }

  # handle non root
  leveldata <- data[[level]]
  if (!is.null(parentKeys)){
    #stopifnot(all(names(parentKeys) %in% names(leveldata)))
    #filter <- apply(leveldata[names(leveldata) %in% names(parentKeys)], 1, function(x){paste(x, collapse=" ")}) == apply(parentKeys, 1, function(x){paste(x, collapse=" ")})
    #leveldata <- leveldata[filter, ]
    leveldata <- leveldata[as.list(parentKeys), nomatch = NULL]

  }
  if (nrow(leveldata) == 0){
    return()
  }

  children <- xsdObject$treeStruct[[level]]

  # get keys
  keys <- names(leveldata)[1:xsdObject$prefixLens[[level]]]

  #stopifnot(!any(duplicated(Reduce(paste, data[[level]][,keys, with=F]))))
  #stopifnot(all(names(parentKeys) %in% keys))

  # write opening tag and attributes
  for (i in 1:nrow(leveldata)){
    openTag(stream, level, leveldata[i,keys,with=F], indent)

    # write simple element tags
    simpletags <- xsdObject$tableHeaders[[level]][!(xsdObject$tableHeaders[[level]] %in% keys)]
    writeSimpleTags(stream, leveldata[i,simpletags,with=F], paste0(indent, "\t"))

    # write complex element tags
    for (ch in children){
      writeLevel(stream, data, ch, leveldata[i,keys,with=F], xsdObject, paste0(indent, "\t"))
    }

    # write closing tag
    closeTag(stream, level, indent)
  }

}

#' @noRd
typeConvert <- function(dataTables, xsdObject){
  for (n in names(xsdObject$tableTypes)){
    stopifnot(n %in% names(dataTables))
    if (nrow(dataTables[[n]])>0){
      for (i in 1:length(xsdObject$tableHeaders[[n]])){
        name <- xsdObject$tableHeaders[[n]][[i]]
        xsdType <- xsdObject$tableTypes[[n]][[i]]

        if(!(name %in% names(dataTables[[n]]))){
          stop(paste("Column", name, "not found in data tables. Possible mismatch with xsdObject."))
        }

        if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:string"){

        }
        else if (is.integer(dataTables[[n]][[name]]) & xsdType == "xs:long"){
          stopifnot(all(is.na(dataTables[[n]][[name]]) | dataTables[[n]][[name]] >= -9223372036854775808))
          stopifnot(all(is.na(dataTables[[n]][[name]]) | dataTables[[n]][[name]] <= 9223372036854775807))
          dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
        }
        else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:string"){
          dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
        }
        else if (is.logical(dataTables[[n]][[name]]) & xsdType == "xs:string"){
          dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
        }
        else if (is.logical(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
          dataTables[[n]][[name]] <- as.character(as.integer(dataTables[[n]][[name]]))
        }
        else if (is.integer(dataTables[[n]][[name]]) & xsdType == "xs:integer"){
          dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
        }
        else if (is.numeric(dataTables[[n]][[name]]) & xsdType == "xs:decimal"){
          dataTables[[n]][[name]] <- as.character(dataTables[[n]][[name]])
        }
        else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:date"){
          ok <- is.na(dataTables[[n]][[name]])
          ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%Y-%m-%d"))
          if (!all(ok)){
            stop(paste("Data type conversion from character to xs:date is not configured for some date formats in data."))
          }
        }
        else if (is.character(dataTables[[n]][[name]]) & xsdType == "xs:time"){
          ok <- is.na(dataTables[[n]][[name]])
          ok <- ok | !is.na(as.POSIXct(dataTables[[n]][[name]], format="%H:%M:%S"))
          if (!all(ok)){
            stop(paste("Data type conversion from character to xs:time is not configured for some time formats in data."))
          }
        }
        else{
          browser()
          stop(paste("Data type conversion from", class(dataTables[[n]][[name]]), "to", xsdType, "is not configured"))
        }
      }
    }
    else{
      for (coln in names(dataTables[[n]])){
        dataTables[[n]][[coln]] <- as.character(dataTables[[n]][[coln]])
      }
    }
  }

  return(dataTables)
}

#' @noRd
setKeysDataTables <- function(dataTables, xsdObject){
  for (dt in names(dataTables)){
    if (length(xsdObject$tableHeaders[[dt]])>0){
      data.table::setkeyv(dataTables[[dt]], xsdObject$tableHeaders[[dt]][1:xsdObject$prefixLens[[dt]]])
    }
  }
  return(dataTables)
}

#' Generic xml writer
#' @description
#'  Support generic writing of xml formats parsed by RstoxData
#' @details
#'  The file is written without namespace prefixes, and requires all names to specified by the same namespace.
#'  This function is applicable when the relational input and the xml format adheres to certain conditions.
#'
#'  Conditions for relational input 'dataTables'
#'  \itemize{
#'   \item{xsdobject specifies the keys for each table by the 'n' leftmost columns, where 'n' is given in 'prefixLens'}
#'   \item{NAs does only occur for optional elements}
#'  }
#'
#'  Conditions for hierarchical XML format:
#'  \itemize{
#'   \item{The root node has no attributes}
#'   \item{All node names are unique across all levels}
#'   \item{All keys are attributes}
#'   \item{Only keys are attributes}
#'   \item{The xsdobject specifies names of attributes and elements, and any constraint on their order in the xml format in 'tableHeaders'}
#'   \item{The xsdobject specifies names of complex type elements, and any constraints on their order in 'tableOrder'}
#'   \item{The xml-format either does not constrain the order of elements or simple types always preceed complex types.}
#'  }
#'
#' @param fileName filename to write xml to
#' @param dataTable relational structure to write as XML, as parsed by readXmlFile
#' @param xsdObject specification for xml format, e.g xsdObjects$nmdbioticv3.1.xsd
#' @param namespace namespace for the xml format
#' @param encoding specifices the encoding (charset)
#' @param xmlStandard specifies the xml version used
#' @noRd
writeXmlFile <- function(fileName, dataTables, xsdObject, namespace, encoding="UTF-8", xmlStandard="1.0"){

  # Notes for development:
  # consider adding namespace name to xsdObjects
  # consider adding XML version to xsdObjects
  # consider adding information about which columns are attributes / elements in xsdObjects
  # consider adding ordering information about all elements in xsdObjects
  # (including the relative ordering of complex and simple elements)
  # consider adding information about key structure in xsdObjects

  dataTables <- typeConvert(dataTables, xsdObject)
  dataTables <- setKeysDataTables(dataTables, xsdObject)

  stream = file(fileName, open="w", encoding=encoding)
  writeXmlDeclaration(stream, version=xmlStandard, encoding=encoding, standalone=T)
  writeLevel(stream, dataTables, xsdObject$root, NULL, xsdObject, "", namespace)
  close(stream)

}

#' Write Stox 3.0 landings to XML
#' @param fileName path to file to write to
#' @param LandingData \code{\link[RstoxData]{LandingData}}
#' @param namespace namespace to use for XML output
#' @noRd
writeLandingXml <- function(fileName, LandingData, namespace="http://www.imr.no/formats/landinger/v2"){

  if (namespace == "http://www.imr.no/formats/landinger/v2"){
    writeXmlFile(fileName, LandingData, RstoxData::xsdObjects$landingerv2.xsd, namespace)
  }
  else {
    stop(paste("Namespace", namespace, "not supported."))
  }
}

#' @noRd
convert2Stox3 <- function(landings){
  dataTables <- list()
  for (n in names(RstoxData::xsdObjects$landingerv2.xsd$tableHeaders)){
    dataTables[[n]] <- landings[,RstoxData::xsdObjects$landingerv2.xsd$tableHeaders[[n]],with=F]
  }
  return(dataTables)
}

#' save landings as XML (namespace: "http://www.imr.no/formats/landinger/v2")
#' @param fileName filename to save to
#' @param landings landings as read by readLandings
#' @export
writeStox27LandingXML <- function(fileName, landings){
  landings <- convert2Stox3(landings)
  writeLandingXml(fileName, landings)
}