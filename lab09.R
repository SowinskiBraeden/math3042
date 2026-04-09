set.seed(123)

# Q1
NormalMeans = function(n, m)
{
  means <- c(1:n)
  
  for (i in 1:n)
  {
    values <- rnorm(m, mean = 0, sd = 1)
    means[i] <- mean(values)
  }
  
  title <- paste(n, "sample means from N(0,1) with sample size", m)
  hist(means, main = title, xlab = "Sample mean", ylab = "Frequency")
  
  return(c(mean(means), sd(means)))
}

normal_m1 <- NormalMeans(10000, 1)
normal_m2 <- NormalMeans(10000, 2)
normal_m10 <- NormalMeans(10000, 10)
normal_m50 <- NormalMeans(10000, 50)
normal_m100 <- NormalMeans(10000, 100)

normal_m1
normal_m2
normal_m10
normal_m50
normal_m100

# Q2
ExponentialMeans = function(n, m)
{
  means <- c(1:n)
  
  for (i in 1:n)
  {
    values <- rexp(m, rate = 1)
    means[i] <- mean(values)
  }
  
  title <- paste(n, "sample means from Exp(1) with sample size", m)
  hist(means, main = title, xlab = "Sample mean", ylab = "Frequency")
  
  return(c(mean(means), sd(means)))
}

exp_m1 <- ExponentialMeans(10000, 1)
exp_m2 <- ExponentialMeans(10000, 2)
exp_m10 <- ExponentialMeans(10000, 10)
exp_m50 <- ExponentialMeans(10000, 50)
exp_m100 <- ExponentialMeans(10000, 100)

exp_m1
exp_m2
exp_m10
exp_m50
exp_m100

# Q3
ExponentialMeansProb = function(n, m, z)
{
  means <- c(1:n)
  
  for (i in 1:n)
  {
    values <- rexp(m, rate = 1)
    means[i] <- mean(values)
  }
  
  probability <- mean(means >= (1 - z) & means <= (1 + z))
  return(probability)
}

prob_2_1 <- ExponentialMeansProb(10000, 2, 1)
prob_2_05 <- ExponentialMeansProb(10000, 2, 0.5)
prob_2_02 <- ExponentialMeansProb(10000, 2, 0.2)

prob_5_1 <- ExponentialMeansProb(10000, 5, 1)
prob_5_05 <- ExponentialMeansProb(10000, 5, 0.5)
prob_5_02 <- ExponentialMeansProb(10000, 5, 0.2)

prob_10_1 <- ExponentialMeansProb(10000, 10, 1)
prob_10_05 <- ExponentialMeansProb(10000, 10, 0.5)
prob_10_02 <- ExponentialMeansProb(10000, 10, 0.2)

prob_2_1
prob_2_05
prob_2_02
prob_5_1
prob_5_05
prob_5_02
prob_10_1
prob_10_05
prob_10_02

# Q4
library(MASS)

data(genotype)

groupA <- subset(genotype, Litter == "A")
groupB <- subset(genotype, Litter == "B")
groupI <- subset(genotype, Litter == "I")
groupJ <- subset(genotype, Litter == "J")

qqnorm(groupA$Wt, main = "QQ Plot of Weight for Litter Genotype A")
qqline(groupA$Wt)

qqnorm(groupB$Wt, main = "QQ Plot of Weight for Litter Genotype B")
qqline(groupB$Wt)

qqnorm(groupI$Wt, main = "QQ Plot of Weight for Litter Genotype I")
qqline(groupI$Wt)

qqnorm(groupJ$Wt, main = "QQ Plot of Weight for Litter Genotype J")
qqline(groupJ$Wt)

# Optional support if you want a numerical check as well
shapiro.test(groupA$Wt)
shapiro.test(groupB$Wt)
shapiro.test(groupI$Wt)
shapiro.test(groupJ$Wt)
