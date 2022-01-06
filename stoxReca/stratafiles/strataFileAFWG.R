source("stratafiles.R")

hovedomr <- readStoxWKT("~/workspace/stox/reference/stratum/mainarea_fdir_fom2018_strata.txt")

# Fra ECA-config fil
#ngroups
#8
#groups
#3 2 10 11 13 14 15 16 17 18 24 1
#12
#4
#5 37 36 39 38
#0
#20 21 22 23 25 26 27
#6
#7 30 34 35 50
AFWGgroups <- list()
AFWGgroups[["03-02-10-11-13-14-15-16-17-18-24-01"]] <- c("03", "02", "10", "11", "13", "14", "15", "16", "17", "18", "24", "01")
AFWGgroups[["12"]] <- c("12")
AFWGgroups[["04"]] <- c("04")
AFWGgroups[["5-37-36-39-38"]] <- c("05", "37", "36", "39", "38")
AFWGgroups[["00"]] <- c("00")
AFWGgroups[["20-21-22-23-25-26-27"]] <- c("20", "21", "22", "23", "25", "26", "27")
AFWGgroups[["06"]] <- c("06")
AFWGgroups[["07-30-34-35-50"]] <- c("07", "30", "34", "35", "50")


newstrata <- mergeStrata(hovedomr, AFWGgroups)
neighbours <- extractNeighbours(newstrata)
writeSpAsWKT(newstrata, "AFWGstrata.txt")
saveNeighbours(newstrata, "AFWGneighbours.txt")
