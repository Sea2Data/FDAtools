context("Test adjustConversionFactor")
landingsfile <- system.file("testresources", "codLandings.rda", package="stox2.7preprocessing")
landings <- readRDS(landingsfile)

context("Test adjustConversionFactor with no records matching criteria")
expect_warning(adjustConversionFactor(landings, 1.671, c(211, 214)))
landings$Fartøynasjonalitet_kode <- "NOR"

context("Test adjustConversionFactor. Check when all records are converted")
# convert SLUH
adjustedLandingsSLUH <- adjustConversionFactor(landings, 1.671, c(211, 214), vesselRange = NULL,
                                                                            months = NULL,
                                                                            onlyNorth62 = F,
                                                                            vesselNationalities = NULL)
#convert SLMH
adjustedLandings <- adjustConversionFactor(adjustedLandingsSLUH, 1.311, 210, vesselRange = NULL,
                                                                              months = NULL,
                                                                              onlyNorth62 = F,
                                                                              vesselNationalities = NULL)

#check calculation SLUH
expect_true(all(adjustedLandingsSLUH$Rundvekt[adjustedLandingsSLUH$Produkttilstand_kode %in% c(211,214)] - (landings$Produktvekt[landings$Produkttilstand_kode %in% c(211,214)]*1.671)==0))
#check calculation SLMH
expect_true(all(adjustedLandings$Rundvekt[adjustedLandings$Produkttilstand_kode == 210] - (landings$Produktvekt[landings$Produkttilstand_kode == 210]*1.311)==0))
#check that other product types are left untouched
expect_equal(adjustedLandings$Rundvekt[!(adjustedLandings$Produkttilstand_kode %in% c(211, 214, 210))], landings$Rundvekt[!(landings$Produkttilstand_kode %in% c(211, 214, 210))])

expect_gt(sum(adjustedLandingsSLUH$Rundvekt), sum(landings$Rundvekt))
expect_gt(sum(adjustedLandings$Rundvekt), sum(adjustedLandingsSLUH$Rundvekt))
expect_equal(adjustedLandingsSLUH$Produktvekt, landings$Produktvekt)
expect_equal(adjustedLandings$Produktvekt, landings$Produktvekt)



context("Test adjustConversionFactor. Check default criteria")
# convert SLUH
adjustedLandingsSLUH <- adjustConversionFactor(landings, 1.671, c(211, 214))
#convert SLMH
expect_warning(adjustedLandings <- adjustConversionFactor(adjustedLandingsSLUH, 1.311, 210))

expect_gt(sum(adjustedLandingsSLUH$Rundvekt), sum(landings$Rundvekt))
expect_gte(sum(adjustedLandings$Rundvekt), sum(adjustedLandingsSLUH$Rundvekt))
expect_equal(adjustedLandingsSLUH$Produktvekt, landings$Produktvekt)
expect_equal(adjustedLandings$Produktvekt, landings$Produktvekt)



#
# check each filter individually
#

landingsfile <- system.file("testresources", "codLandings.rda", package="stox2.7preprocessing")
landings <- readRDS(landingsfile)
landings$StørsteLengde[is.na(landings$StørsteLengde)] <- 12

context("check vessel length filter")
expect_warning(adjustedLandingsSLlt10 <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselRange = c(0,10), vesselNationalities = NULL, months = NULL))
adjustedLandingsSLlt20 <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselRange = c(0,20), vesselNationalities = NULL, months = NULL)
expect_gt(sum(adjustedLandingsSLlt20$Rundvekt), sum(adjustedLandingsSLlt10$Rundvekt))


context("check missing lengths filter")
landings$StørsteLengde[landings$StørsteLengde==12] <- NA
adjustedLandingsSLmissing <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL)
expect_warning(adjustConversionFactor(landings, 1.671, c(211, 214), missingLengths = F, vesselNationalities = NULL))
expect_gt(sum(adjustedLandingsSLmissing$Rundvekt), sum(landings$Rundvekt))


context("check months  filter")
landings$StørsteLengde[is.na(landings$StørsteLengde)] <- 12
adjustedLandingsDefault <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL)
adjustedLandingsAllMonths <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL, months = NULL)
expect_gt(sum(adjustedLandingsDefault$Rundvekt), sum(landings$Rundvekt))
expect_gt(sum(adjustedLandingsAllMonths$Rundvekt), sum(adjustedLandingsDefault$Rundvekt))

context("check N62  filter")
adjustedLandingsDefault <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL)
adjustedLandingsS62 <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL, onlyNorth62 = F)
expect_gt(sum(adjustedLandingsDefault$Rundvekt), sum(landings$Rundvekt))
expect_gt(sum(adjustedLandingsS62$Rundvekt), sum(adjustedLandingsDefault$Rundvekt))

context("check nationality filter")
adjustedLandingsDefault <- adjustConversionFactor(landings, 1.671, c(211, 214,210), months=NULL, onlyNorth62 = F)
adjustedLandingsAllVessels <- adjustConversionFactor(landings, 1.671, c(211, 214,210), vesselNationalities = NULL, month=NULL, onlyNorth62 = F)
expect_gt(sum(adjustedLandingsDefault$Rundvekt), sum(landings$Rundvekt))
expect_gt(sum(adjustedLandingsAllVessels$Rundvekt), sum(adjustedLandingsDefault$Rundvekt))


