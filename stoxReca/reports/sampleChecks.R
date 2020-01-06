library(Rstox)

#
# the function makeSampleHomogeneityReportRECA runs a project and checks for issues
# that might affect interpretation on Reca results. See ?makeSampleHomogeneityReportRECA for details
#

# Run checks on project residing in <user home>/workspace/stox/projects
projectname <- "ECA_kolmule_2018"

# Default run checks output from process named FilterBiotic and writes to stdout
Rstox::makeSampleHomogeneityReportRECA(projectname)

# write to file in stead
Rstox::makeSampleHomogeneityReportRECA(projectname, issuefile = "~/temp/kolmuleissues.txt")

# check output from different process in stead
Rstox::makeSampleHomogeneityReportRECA(projectname, processName = "ReadBioticXML")
