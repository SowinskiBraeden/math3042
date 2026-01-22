install.packages("mosaic")

library(mosaic)
library(mosaicData)
library(dplyr)
library(stringi)

# Q1
fastrunners=filter(TenMileRace, net<4000)

# Q2
femalerunners=filter(TenMileRace, sex=="F")
foreignrunners=filter(TenMileRace, stri_length(state) >= 3)

# Q3
table(TenMileRace$state)

# Q4
fastestfemale=min(femalerunners$net)
malerunners=filter(TenMileRace, sex=="M")
fastermales=filter(malerunners, net < fastestfemale)
nrow(fastermales)

# Q5
numrunners=nrow(TenMileRace)
fifthpercentile=0.05 * numrunners
typcialrunners=filter(TenMileRace, rank(net) > fifthpercentile & rank(net) < (numrunners - fifthpercentile))
hist(typcialrunners$net)

