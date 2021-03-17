context("Test encodeCostalCodArea")

landings <- readLandings(system.file("testresources", "landings_trimmed_2018.xml", package="stox2.7preprocessing"))
logbook <- RstoxData::readErsFile(system.file("testresources", "logbooks_mock_2018.psv", package="stox2.7preprocessing"))
adjusted <- adjustWithLogbook(landings, logbook, "HER", 11)

recoded <- encodeCostalCodArea(adjusted)
expect_equal(sum(recoded$Hovedområde_kode=="s501"),2)
expect_equal(sum(recoded$Hovedområde_kode=="s500"),1)

