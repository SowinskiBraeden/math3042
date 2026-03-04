# 1a.
SimDiceSample = function(m,n)
{
  threes<-c(1:n)
  
  for (i in 1:n)
  {
    rolls<-sample(c(1,2,3,4,5,6), m, replace=T)
    threes[i]<-sum(rolls==3)
  }
  
  freq<-table(threes)/n
  print(freq)
  
  barplot(freq,
          main=paste("Probability histogram for", m, "dice - Sample()"),
          xlab="Number of 3's",
          ylab="Probabilities")
  
  return(freq)
}

# 1b.
SimDiceBinom = function(m,n)
{
  threes<-c(1:n)
  
  for (i in 1:n)
  {
    threes[i]<-rbinom(1,m,1/6)
  }
  
  freq<-table(threes)/n
  
  print(freq)
  
  barplot(freq,
          main=paste("Probability histogram for", m, "dice - rbinom()"),
          xlab="Number of 3's",
          ylab="Probabilities")
  
  return(freq)
}

SimDiceSample(10, 10000)
SimDiceBinom(10, 10000)


# 3.
dbinom(100,300,1/3)
pbinom(100,300,1/3)
pbinom(99,300,1/3)
1-pbinom(109,300,1/3)
pbinom(110,300,1/3)-pbinom(89,300,1/3)

# 4.
x<-0:4
p<-dhyper(x,4,48,8)
cbind(x,p)

barplot(p,
        names.arg=x,
        ylab="Probabilities",
        main="Probability histogram: # of jacks in 8 cards")

# 5a.
SimJacksSample = function(m,n)
{
  deck<-c(rep(1,4),rep(0,48))
  jacks<-c(1:n)
  
  for (i in 1:n)
  {
    draws<-sample(deck,m,replace=F)
    jacks[i]<-sum(draws==1)
  }
  
  freq<-table(jacks)/n
  print(freq)
  
  barplot(freq,
          ylab="Relative frequency",
          main=paste("Simulation (sample): # of jacks in",m,"cards"))
  
  return(freq)
}

SimJacksSample(8,10000)

# 5b.
SimJacksHyper = function(m,n)
{
  jacks<-c(1:n)
  
  for (i in 1:n)
  {
    jacks[i]<-rhyper(1,4,48,m)
  }
  
  freq<-table(jacks)/n
  print(freq)
  
  barplot(freq,
          ylab="Relative frequency",
          main=paste("Simulation (rhyper): # of jacks in",m,"cards"))
  
  return(freq)
}

SimJacksHyper(8,10000)

# 6.
dhyper(100,333,666,300)
phyper(100,333,666,300)
phyper(99,333,666,300)
1-phyper(109,333,666,300)
phyper(110,333,666,300)-phyper(89,333,666,300)

# 8.
p<-1/5
x<-0:100
prob<-dgeom(x,p)

keep<-prob>=0.0001
x2<-x[keep]
prob2<-prob[keep]

cbind(x2,prob2)

barplot(prob2,
        names.arg=x2,
        ylab="Probabilities",
        main="Exact distribution: # losing tickets before first win (p=1/5)")


# 9a.
SimLotterySample = function(n,p)
{
  losses<-c(1:n)
  
  for (i in 1:n)
  {
    count<-0
    repeat
    {
      draw<-sample(c("W","L"),1,replace=T,prob=c(p,1-p))
      if (draw=="W") break
      count<-count+1
    }
    losses[i]<-count
  }
  
  freq<-table(losses)/n
  print(freq)
  
  barplot(freq,
          ylab="Relative frequency",
          main="Simulation (sample): # losing tickets before first win")
  
  return(freq)
}

SimLotterySample(10000,1/5)

# 9b.
SimLotteryGeom = function(n,p)
{
  losses<-c(1:n)
  
  for (i in 1:n)
  {
    losses[i]<-rgeom(1,p)
  }
  
  freq<-table(losses)/n
  print(freq)
  
  barplot(freq,
          ylab="Relative frequency",
          main="Simulation (rgeom): # losing tickets before first win")
  
  return(freq)
}

SimLotteryGeom(10000,1/5)

# 11
p<-0.001

pgeom(499,p)
1-pgeom(1198,p)
pgeom(1999,p)-pgeom(998,p)

# 12.
lambda<-0.75
x<-0:30
p<-dpois(x,lambda)

keep<-p>=0.0001
x2<-x[keep]
p2<-p[keep]

cbind(x2,p2)

barplot(p2,
        names.arg=x2,
        ylab="Probabilities",
        main="Exact distribution: # flaws per meter (lambda=0.75)")

# 13.
SimFlaws = function(n,lambda)
{
  flaws<-c(1:n)
  
  for (i in 1:n)
  {
    flaws[i]<-rpois(1,lambda)
  }
  
  freq<-table(flaws)/n
  print(freq)
  
  barplot(freq,
          ylab="Relative frequency",
          main=paste("Simulation: # flaws per meter (lambda =",lambda,")"))
  
  return(freq)
}

SimFlaws(10000,0.75)

# 15.
lambda<-34

dpois(34,lambda)
ppois(30,lambda)
ppois(29,lambda)
1-ppois(38,lambda)
1-ppois(37,lambda)
ppois(40,lambda)-ppois(29,lambda)

(ppois(34,lambda))^2
ppois(68,2*lambda)

