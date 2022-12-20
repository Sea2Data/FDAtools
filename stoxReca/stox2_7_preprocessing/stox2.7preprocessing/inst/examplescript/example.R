library(stox2.7preprocessing)

# prepare data for coastal cod estimate 2021 (seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/torsk_kyst_2022_logbook_and_factor_adjusted_pr_des_2022.xml",
                                  "~/landingsets/xml/filtered/torsk_2022_pr_des_2022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2022_PR_2022-12-12.psv",
                                  seasonalConversionFactor=T, coastalCod=T)

# prepare data for NEA cod estimate in coastal region (without seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/torsk_kyst_2022_logbook_adjusted_pr_des_2022.xml",
                                  "~/landingsets/xml/filtered/torsk_2022_pr_des_2022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2022_PR_2022-12-12.psv",
                                  seasonalConversionFactor=F, coastalCod=T)

# prepare data for NEA cod estimate in offshore region (without seasonal conversion factors):
processLandingsAllAdjustmentsAFWG("~/temp/torsk_hav_2022_logbook_adjusted.xml",
                                  "~/landingsets/xml/filtered/torsk_2022_pr_des_2022.xml",
                                  "~/logbooks/FDIR_HI_ERS_2022_PR_2022-12-12.psv",
                                  seasonalConversionFactor=F, coastalCod=F)

# get strata files for Stox2.7 DefineStrata
#writeSpDataFrameAsWKT(stox2.7preprocessing::coastalCodAreasAFWG, "~/temp/coastalCodStrata.txt")
# get postion files for Stox2.7 ApplyPosToData (ApplyPosToLanding)
#writePolygonPositions(stox2.7preprocessing::coastalCodAreas, "~/temp/coastalCodAreaPositions.txt")
