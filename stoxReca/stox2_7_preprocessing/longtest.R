library(stox2.7preprocessing)
ts0 <- Sys.time()
landings <- readLandings("~/landingsets/xml/filtered/2015_torsk.xml")
ts1 <- Sys.time()
print("File read")
print(ts1-ts0)
print("Writing file")
writeStox27LandingXML("~/temp/testland.xml", landings)
ts2 <- Sys.time()
print(ts2-ts1)
