install.packages("aplpack")

library(aplpack)
library(MASS, lib.loc = "/usr/lib/R/library")
library(dplyr)

# Q1
percents<-c(10, 30, 5, 35, 20)
homeType<-c("on campus", "with parents", "alone", "with roomates", "with spouse")
colors<-c("green", "yellow", "red", "blue", "cyan")
pie(percents, homeType, col=colors)

# Q2
hands<-survey$W.Hnd
tab<-table(hands)
lbls<-paste(rownames(tab), sep="\n", tab)
colors<-c("pink", "turquoise")
title<-paste("Writing hands of", sum(tab), "students", sep=" ")
pie(tab, lbls, col=colors, main=title)

# Q3
tab<-table(hands)
title<-paste("Writing hands of", sum(tab), "students", sep=" ")
barplot(tab, horiz=TRUE, main=title)


# Q4
stem(survey$Height, scale=2)

# Q5
mensHeight<-filter(survey, Sex=="Male")$Height
womensHeight<-filter(survey, Sex=="Female")$Height
stem.leaf.backback(mensHeight, womensHeight, depths=FALSE)

# Q7
maleAges<-filter(survey, Sex=="Male")$Age
femaleAges<-filter(survey, Sex=="Female")$Age
ageRange<-range(survey$Age)
interval<-5
breaks<-seq(min(ageRange), max(ageRange) + interval, by=interval)
maleAges.cut<-cut(maleAges, breaks, right=FALSE)
femaleAges.cut<-cut(femaleAges, breaks, right=FALSE)
maleAges.freq<-table(maleAges.cut)
femaleAges.freq<-table(femaleAges.cut)
cbind(maleAges.freq)
cbind(femaleAges.freq)

# Q8
numHeights<-survey$Height[!is.na(survey$Height)]
breaks<-seq(min(numHeights), max(numHeights), by=2.5)
title<-paste("Heights of", nrow(survey), "students", sep=" ")
hist(numHeights, breaks=breaks, main=title, xlab="Heights (cm)")

# Q9
numHeights<-survey$Height[!is.na(survey$Height)]
heightRange<-range(numHeights)
interval<-5
breaks<-seq(min(heightRange), max(heightRange) + interval, by=interval)
numHeights.cut<-cut(numHeights, breaks, right=FALSE)
numHeights.freq<-table(numHeights.cut)
numHeights.cumfreq=cumsum(numHeights.freq)
heightFreqs<-c(0, numHeights.cumfreq)
title<-paste("Ogive of", length(numHeights), "student heights", sep=" ")
xtitle<-"student heights in cm"
ytitle<-"number of students"
plot(breaks, heightFreqs, type="b", main=title, xlab=xtitle, ylab=ytitle)

# Q10
title<-paste("Student height vs hand span of", nrow(survey), "students", sep=" ")
xtitle<-"student heights in cm"
ytitle<-"span of writing hand in cm"
xyplot(survey$Wr.Hnd ~ survey$Height, main=title, xlab=xtitle, ylab=ytitle)

# Q11
title<-paste("Writing hand vs Non-Writing hand spans of", nrow(survey), "students", sep=" ")
xtitle<-"Writing hand span in cm"
ytitle<-"Non-Writing hand span in cm"
xyplot(survey$NW.Hnd ~ survey$Wr.Hnd, main=title, xlab=xtitle, ylab=ytitle)

title<-paste("Height vs Pulse of", nrow(survey), "students", sep=" ")
xtitle<-"Student height in cm"
ytitle<-"Student Pulse in BPM"
xyplot(survey$Pulse ~ survey$Height, main=title, xlab=xtitle, ylab=ytitle)

