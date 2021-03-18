#stox2.7preprosessing

## Preprocessing for AWFG
Some estimates relies on logbook-adjustment of landings. In particular the coastal cod estimate.
It will take some time before logbook-adjustments can be included in StoX. As a workaround, this package offers functions that adjusts the landings (sales-notes) and saves them to the XML-format that is accepted as input to StoX, so that the StoX-workflow can be applied once necessary changes are done. A few other workaround functions are also offered to prepare data for Stox2.7-Reca runs, some of them will not be necessary for Stox 3.0. The important functions are:
* adjustConversionFactor: adjust the conversion factors used when calculating live-weight in landings.
* encodeCostalCodArea: encodes area definiton for coastal cod analysis in the column for area in sales notes (hovedområde)
* adjustWithLogbook: adjusts landings with spatial and temporal information from logbooks.

An example of how these functions can be applied is provided in stox2.7preprocessing/inst/examplescript/example.R

## coastal cod strata files
The coastal-cod estimate has been using a custom area-definition. These area definitions are stored in this package and functions for export Stox-stratafiles and Stox2.7 area-position resources are offered.

## File writing
This package offers a function (writeStox27LandingXML) that writes landings data to the XML-format accepted by StoX. For NEA cod and coastal cod, this is a large volume and writeStox27LandingXML performs poorly. A full year of adjusted landings may need several hours to complete and produce files that are several gigabytes large.

## Installing and dependencies
The package may be installed with: devtools::install_github("https://github.com/Sea2Data/FDAtools", subdir = "stoxReca/stox2_7_preprocessing")


