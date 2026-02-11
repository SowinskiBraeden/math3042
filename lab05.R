# Q1 Write a function, hasAntibodies(n) that simulates checking n people and 
# returns the number with antibodies. Assume people being checked are 
# independent and that person has a 5% chance of having antibodies.
hasAntibodies = function(n)
{
  hasAntibody    <- sample(c(0, 1), n, replace=TRUE, prob=c(0.5, 0.5))
  sumHasAntibody <- sum(hasAntibody == 1)

    return(sumHasAntibody)
}

# Q2 Write a function called truePositives(n) that simulates testing n people 
# who have COVID-19 antibodies, and returns the number that test positive. Run 
# your function for n=100000 and give your output.
truePositives = function(n)
{
  isPositive    <- sample(c(0, 1), n, replace=TRUE, prob=c(0.1, 0.9))
  sumIsPositive <- sum(isPositive == 1)
  
  return(sumIsPositive)
}

# Q4 Write a function called falsePositives(n) that simulates testing n 
# people who do not have COVID-19 antibodies, and returns the number who test 
# positive
falsePositives = function(n)
{
  isNegative       <- sample(c(0, 1), n, replace=TRUE, prob=c(0.08, 0.92))
  sumFalsePositive <- sum(isNegative == 0)

  return(sumFalsePositive)
}

# Q7 Write a function called probNegativeGivenTestsNegative(n) that
# simulates testing n samples and returns the following proportion:
# num people without covid-19 antibodies who test negative /
# total number of people who test negative
probNegativeGivenTestsNegative = function(n)
{
  numWith    <- hasAntibodies(n)
  numWithout <- n - numWith
  
  trueNumWith <- truePositives(numWith)
  falseNeg    <- numWith - trueNumWith
  
  falsePos    <- falsePositives(numWithout)
  trueNeg     <- numWithout - falsePos
  
  return(trueNeg / (trueNeg + falseNeg)) 
}

# Q8 write a function called probPositiveGivenTestsPositive(n) to get an 
# answer to the following question: suppose our test is positive – that is, 
# our test tells us that the person has COVID-19 antibodies. What is the 
# probability that our test is correct – that is, the person actually has 
# COVID-19 antibodies?
probPositiveGivenTestsPositive = function(n)
{
  numWith    <- hasAntibodies(n)
  numWithout <- n - numWith
  
  truePos <- truePositives(numWith)
  falsePos <- falsePositives(numWithout)
  
  return(truePos / (truePos + falsePos))
}

