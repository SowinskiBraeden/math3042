library(mosaic)
library(MASS)

# Q1
mean_days<-mean(~Days, data=quine)
median_days<-median(~Days, data=quine)
sd_days<-sd(~Days, data=quine)
sk_days<-3 * (mean_days - median_days) / sd_days

# Q2
title<-"Days Absent Histogram"
xtitle<-"Days Absent"
ytitle<-"Frequency"
hist(quine$Days, main=title, xlab=xtitle, ylab=ytitle)

# Q3
title<-"Box Plot Days Absent"
xtitle<-"Days Absent"
bwplot(~Days, data=quine, main=title, xlab=xtitle)

# Q4
median(quine$Days)

# Q5
title<-"Days Absent Grouped by Age"
xtitle<-"Days Absent"
ytitle<-"Age"
bwplot(quine$Age ~ quine$Days, main=title, xlab=xtitle, ylab=ytitle)

# Q6
favstats(Days ~ Age + Sex, data=quine)

# Q7
quantile(Days ~ Lrn, data=quine, probs = c (.20, .40, .60, .80))
quantile(Days ~ Eth, data=quine, probs = c (.20, .40, .60, .80))
quantile(Days ~ Sex, data=quine, probs = c (.20, .40, .60, .80))

# Q8
f0_male<-filter(quine, Age=="F0" & Sex=="M")
days_f0_male<-f0_male$Days
percentiles<-rank(days_f0_male) / length(days_f0_male) * 100
percentiles<-round(percentiles, 2)
percentiles

# Q9
studentsOneMean<-quine |> filter(abs(scale(Days)) >= 1)
studentsTwoMean<-quine |> filter(abs(scale(Days)) >= 2)
studentsThreeMean<-quine |> filter(abs(scale(Days)) >= 3)
nrow(studentsOneMean)
nrow(studentsTwoMean)
nrow(studentsThreeMean)