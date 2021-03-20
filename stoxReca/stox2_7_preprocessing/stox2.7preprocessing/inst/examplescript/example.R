library(stox2.7preprocessing)

# prepare data for coastal cod estimate 2021 (seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/coastalAreasCodSeasonalCF2018.xml",
                                  "~/workspace/stox/ECA_prosjekter/Kysttorsk_AFWG_2018/input/landing/torsk_2018_2020_03_23.xml",
                                  "~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv")

# prepare data for NEA cod estimate in coastal region (without seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/coastalAreasCodCF2018.xml",
                                  "~/workspace/stox/ECA_prosjekter/Kysttorsk_AFWG_2018/input/landing/torsk_2018_2020_03_23.xml",
                                  "~/logbooks/FDIR_HI_ERS_2018_PR_2019-03-04.psv",
                                  seasonalConversionFactor=F)

# get strata files for Stox2.7 DefineStrata
writeSpDataFrameAsWKT(stox2.7preprocessing::coastalCodAreasAFWG, "~/temp/coastalCodStrata.txt")
# get postion files for Stox2.7 ApplyPosToData (ApplyPosToLanding)
writePolygonPositions(stox2.7preprocessing::coastalCodAreas, "~/temp/coastalCodAreaPositions.txt")
