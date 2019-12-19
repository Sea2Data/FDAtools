# makeBioticDataSet

Examples for fetching biotic data via NMD biotic API, and convertion to more convientent tabular views. XML files were read using the RstoxData library, under development at: https://github.com/StoXProject/RstoxData (commit d753e1d, the function RstoxData::readXmlFile).

Install RstoxData with:
devtools::install_github("StoXProject/RstoxData")

file | content
-----|--------
pull.R | functions for downloading data from NMDbiotic v3 API
tabularize.R | functions for converting XML to customized tabular views.

For documentation on how to use the NMDbiotic v3 API (only available internally on IMR net): https://confluence.imr.no/display/API/Biotic+V3+API+documentation#BioticV3APIdocumentation-Getcachedyeardata

For documentation of the biotic v3 data format: http://www.imr.no/formats/nmdbiotic/v3/

For documentation of codes used in biotic v3 (only available internally on IMR net): https://referenceeditor.hi.no