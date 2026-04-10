library(mosaicData)
library(MASS)

# Q1
set.seed(123)

truemean=mean(TenMileRace$net)

lowerbounds=rep(0, 20)
upperbounds=rep(0, 20)
containsmean=rep(FALSE, 20)

for(i in 1:20)
{
  racetimes25=TenMileRace$net[sample(1:nrow(TenMileRace), 25)]
  
  raceci=t.test(racetimes25)
  
  lowerbounds[i]=raceci$conf.int[1]
  upperbounds[i]=raceci$conf.int[2]
  containsmean[i]=truemean >= lowerbounds[i] & truemean <= upperbounds[i]
}

ciresults=data.frame(
  sample=1:20,
  lower=lowerbounds,
  upper=upperbounds,
  containstruemean=containsmean
)

ciresults
sum(containsmean)

# Q2
femalecats=subset(cats, Sex=="F")
malecats=subset(cats, Sex=="M")

femalebodyci=t.test(femalecats$Bwt)
femaleheartci=t.test(femalecats$Hwt)
malebodyci=t.test(malecats$Bwt)
maleheartci=t.test(malecats$Hwt)

femalebodyci
femaleheartci
malebodyci
maleheartci

# Q3
set.seed(123)

racetimes30=TenMileRace$net[sample(1:nrow(TenMileRace), 30)]
racetimes230=TenMileRace$net[sample(1:nrow(TenMileRace), 230)]
racetimes430=TenMileRace$net[sample(1:nrow(TenMileRace), 430)]

ci30=t.test(racetimes30)
ci230=t.test(racetimes230)
ci430=t.test(racetimes430)

ci30
ci230
ci430

ci30$conf.int
ci230$conf.int
ci430$conf.int

# Q4
confidence=function(data, conflevel)
{
  ci=t.test(data, conf.level=conflevel)
  
  lowerbound=ci$conf.int[1]
  upperbound=ci$conf.int[2]
  confpercent=attr(ci$conf.int, "conf.level") * 100
  
  cat("We are", confpercent, 
      "percent sure that the mean is between", 
      lowerbound, "and", upperbound, ".")
}

confidence(survey$Age, 0.99)

# Q5
confidence=function(data, conflevel, meaninfo, units)
{
  CI=t.test(data, conf.level=conflevel)
  
  lowerbound=CI$conf.int[1]
  upperbound=CI$conf.int[2]
  confpercent=attr(CI$conf.int, "conf.level") * 100
  
  cat("We are", confpercent, "percent sure that the mean", 
      meaninfo, "is between", lowerbound, "and", upperbound, 
      units, ".")
}

confidence(survey$Age, 0.99, "student age", "years")

# Q6
confidence=function(data, conflevel=0.95, meaninfo, units)
{
  CI=t.test(data, conf.level=conflevel)
  
  lowerbound=CI$conf.int[1]
  upperbound=CI$conf.int[2]
  confpercent=attr(CI$conf.int, "conf.level") * 100
  
  cat("We are", confpercent, "percent sure that the mean", 
      meaninfo, "is between", lowerbound, "and", upperbound, 
      units, ".")
}

confidence(survey$Age, 0.90, "student age", "years")
confidence(survey$Age, 0.95, "student age", "years")
confidence(survey$Age, quantity="student age", units="years")