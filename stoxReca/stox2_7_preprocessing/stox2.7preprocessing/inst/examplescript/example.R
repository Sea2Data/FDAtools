library(stox2.7preprocessing)

# prepare data for coastal cod estimate 2021 (seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/torsk_2021_logbook_adjusted.xml",
                                  "~/landingsets/xml/filtered/landing-year-2021-species-1022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2021_PR_2021-12-02.psv", coastalCod=T)

# prepare data for NEA cod estimate in coastal region (without seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/kysttorsk_2021_logbook_and_factor_adjusted.xml",
                                  "~/landingsets/xml/filtered/landing-year-2021-species-1022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2021_PR_2021-12-02.psv",
                                  seasonalConversionFactor=F, coastalCod=T)

# prepare data for NEA cod estimate in offshore region (without seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/kysttorsk_2021_logbook_and_factor_adjusted.xml",
                                  "~/landingsets/xml/filtered/landing-year-2021-species-1022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2021_PR_2021-12-02.psv",
                                  seasonalConversionFactor=F, coastalCod=F)

# get strata files for Stox2.7 DefineStrata
#writeSpDataFrameAsWKT(stox2.7preprocessing::coastalCodAreasAFWG, "~/temp/coastalCodStrata.txt")
# get postion files for Stox2.7 ApplyPosToData (ApplyPosToLanding)
#writePolygonPositions(stox2.7preprocessing::coastalCodAreas, "~/temp/coastalCodAreaPositions.txt")
