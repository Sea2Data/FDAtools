context("Test logbook adjustment")
logbook <- RstoxData::readErsFile(system.file("testresources", "logbooks_mock_2018.psv", package="stox2.7preprocessing"))
landings <- readLandings(system.file("testresources", "landings_trimmed_2018.xml", package="stox2.7preprocessing"))

adjusted <- adjustWithLogbook(landings, logbook, "HER", 11)
expect_lt(nrow(landings), nrow(adjusted))
expect_equal(sum(adjusted$Rundvekt), sum(landings$Rundvekt))
expect_equal(sum(adjusted$Produktvekt), sum(landings$Produktvekt))
expect_equal(sum(adjusted$Bruttovekt), sum(landings$Bruttovekt))
expect_equal(sum(adjusted$Radiokallesignal_seddel=="LK5707"), 4)
expect_equal(sum(landings$Radiokallesignal_seddel=="LK5707"), 2)
expect_true(is.numeric(adjusted$FOlat))
