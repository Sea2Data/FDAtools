source("stratafiles.R")

hovedomr <- readStoxWKT("~/workspace/stox/reference/stratum/mainarea.txt")

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
AFWGgroups[["3 2 10 11 13 14 15 16 17 18 24 1"]] <- c("3", "2", "10", "11", "13", "14", "15", "16", "17", "18", "24", "1")
AFWGgroups[["12"]] <- c("12")
AFWGgroups[["4"]] <- c("4")
AFWGgroups[["5 37 36 39 38"]] <- c("5", "37", "36", "39", "38")
AFWGgroups[["0"]] <- c("0")
AFWGgroups[["20 21 22 23 25 26 27"]] <- c("20", "21", "22", "23", "25", "26", "27")
AFWGgroups[["6"]] <- c("6")
AFWGgroups[["7 30 34 35 50"]] <- c("7", "30", "34", "35", "50")


newstrata <- mergeStrata(hovedomr, AFWGgroups)
neighbours <- extractNeighbours(newstrata)
writeSpAsWKT(newstrata, "AFWGstrata.txt")
saveNeighbours(newstrata, "AFWGneighbours.txt")
