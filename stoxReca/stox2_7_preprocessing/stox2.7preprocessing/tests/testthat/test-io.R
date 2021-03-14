context("Test xml read landings")

landingsfile <- system.file("testresources", "landing.xml", package="stox2.7preprocessing")
landings <- readLandings(landingsfile)


context("Test xml writing landings")
example <- readLandings(landingsfile)

tmp <- tempfile()
writeStox27LandingXML(tmp, example)
backIn <- readLandings(tmp)
unlink(tmp)

example <- example[order(example$Dokumentnummer, example$Linjenummer),]
backIn <- backIn[order(backIn$Dokumentnummer, backIn$Linjenummer),]

expect_equal(example, backIn)


context("Test xml writing generic")
example <- RstoxData::readXmlFile(system.file("testresources","biotic3.1_example.xml", package="RstoxData"))
tmp <- tempfile()
writeXmlFile(tmp, example, RstoxData::xsdObjects$nmdbioticv3.1.xsd, "http://www.imr.no/formats/nmdbiotic/v3.1")
backIn <- RstoxData::readXmlFile(tmp)
unlink(tmp)

backIn$metadata <- example$metadata
expect_equal(example, backIn)
