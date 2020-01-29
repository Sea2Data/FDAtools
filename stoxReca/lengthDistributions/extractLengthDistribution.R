library(Rstox)
projectname <- "ECA_NSSK_sei_2018"
runRScripts(projectname)
l<-loadProjectData(projectname, var="prepareRECA")


# get distribution of mean length pr age group
meanLengthPrAgeGroup <- l$runRECA$pred$MeanLength
rownames(meanLengthPrAgeGroup) <- l$runRECA$pred$AgeCategories
colnames(meanLengthPrAgeGroup) <- paste("Iteration", 1:ncol(meanLengthPrAgeGroup))

# condense to a profile of mean length pr age group and a stadard deviation, calculated over iterations
# SD depends primarily on the number of iterations runs
profileMeanLengthPrAgeGroup <- rowMeans(meanLengthPrAgeGroup)
sdMeanLengthPrAgeGroup <- apply(meanLengthPrAgeGroup, FUN=sd, MARGIN=1)

# get distribution of mean number of fish pr length and age group
meanNumberPrLengthAndAgeGroup <- t(apply(l$runRECA$pred$TotalCount, FUN=mean, MARGIN=c(1,2)))
lengths <- exp(l$runRECA$pred$LengthIntervalsLog)
rownames(meanNumberPrLengthAndAgeGroup) <- l$runRECA$pred$AgeCategories
colnames(meanNumberPrLengthAndAgeGroup) <- paste(format(lengths, digits=2, nsmall=1), "cm")

# condense to mean length pr age group and a stadard deviation, calculated over the length distribution
# SD depends primarily on the number of length groups and the natural variation in length
profileMeanNumberPrLengthAndAgeGroup <- apply(meanNumberPrLengthAndAgeGroup, FUN=function(x){mean(rep(lengths,x))}, MARGIN=1)
sdMeanNumberPrLengthAndAgeGroup <- apply(meanNumberPrLengthAndAgeGroup, FUN=function(x){sd(rep(lengths,x))}, MARGIN=1)

#
# Note the row and column names, while the structure of these two results are similar, their interpretation is quite different.
#
par(mfrow=c(1,2))
plot(1:length(profileMeanLengthPrAgeGroup), profileMeanLengthPrAgeGroup, xlab="age", ylab="length", main="iterations")
segments(1:length(profileMeanLengthPrAgeGroup), profileMeanLengthPrAgeGroup + sdMeanLengthPrAgeGroup, y1=profileMeanLengthPrAgeGroup - sdMeanLengthPrAgeGroup)

plot(1:length(profileMeanNumberPrLengthAndAgeGroup), profileMeanNumberPrLengthAndAgeGroup, xlab="age", ylab="length", main="length distributions")
segments(1:length(profileMeanNumberPrLengthAndAgeGroup), profileMeanNumberPrLengthAndAgeGroup + sdMeanNumberPrLengthAndAgeGroup, y1=profileMeanNumberPrLengthAndAgeGroup - sdMeanNumberPrLengthAndAgeGroup)

par(mfrow=c(1,1))
plot(1:length(profileMeanNumberPrLengthAndAgeGroup), sdMeanNumberPrLengthAndAgeGroup, xlab="age", ylab="sd", ylim=c(0,max(sdMeanNumberPrLengthAndAgeGroup)), col="black")
points(1:length(profileMeanLengthPrAgeGroup), sdMeanLengthPrAgeGroup, col="red")
legend("bottomright", fill=c("black", "red"), legend=c("length (length distributions)", "mean length (iterations)"))
