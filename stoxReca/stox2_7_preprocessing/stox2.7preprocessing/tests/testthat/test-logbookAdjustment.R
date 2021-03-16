context("Test logbook adjustment")
logbook <- RstoxData::readErsFile(system.file("testresources", "logbooks_mock_2018.psv", package="stox2.7preprocessing"))
landings <- readLandings(system.file("testresources", "landings_trimmed_2018.xml", package="stox2.7preprocessing"))

adjusted <- adjustLogbook(landings, logbook, "HER", 11)
expect_lt(nrow(landings), nrow(adjusted))
expect_equal(sum(adjusted$Rundvekt), sum(landings$Rundvekt))

