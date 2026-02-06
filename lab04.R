# Q1 - Run 10 times and count heads vs tails
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)
sample(c("Heads", "Tails"), 1)

# *******************************
FlipOnce = function()
{
  headOrTail<-sample(c("Heads", "Tails"), 1)
  return(headOrTail)
}

CoinResults = function(n)
{
  coinList<-c(1:n)
  for (i in 1:n)
  {
    coinList[i]=FlipOnce()
  }
  return(coinList)
}

ProbHeads=function(n)
{
  coinList<-CoinResults(n)
  numHeads<-sum(coinList=="Heads")
  return(numHeads / n)
}

# Q2 Rewrite your CoinResults(n) function in the script window without using 
# loops
CoinResults = function(n)
{
  coinList<-sample(c("Heads", "Tails"), n, repl=T)
  return(coinList)
}

ProbHeads = function(n)
{
  coinList<-CoinResults(n)
  numHeads<-sum(coinList=="Heads")
  return(numHeads / n)
}

# Q3 Write a function called MaxAndMinHeads(m,n) that calls ProbsHeads(m) n 
# times, and returns the maximum and minimum probability of obtaining heads.

MaxAndMinHeads = function(m, n)
{
  results<-c(1:n)
  for (i in 1:n)
  {
    results[i] = ProbHeads(m)
  }
  return(c(min(results), max(results)))
}

# Q4 Write a function called RollDie(n) that simulates rolling a die n times, 
# and returns a bar plot
RollDie = function(n)
{
  rolls<-sample(c(1,2,3,4,5,6), n, repl=T)
  title<-paste("Distribution of outcomes of", n, "die rolls", sep=" ")
  plot<-barplot(table(rolls), main=title)
  return(plot)
}

# Q5 Write a function called RollSomeDice(n,m) that simulates rolling m dice n 
# times, and each time counts the number of 3’s obtained in the m dice.
RollSomeDice = function(n, m)
{
  threes<-c(1:n)
  for (i in 1:n)
  {
    rolls<-sample(c(1,2,3,4,5,6), m, repl=T)
    threes[i]<-sum(rolls==3)
  }
  title<-paste("Number of 3's obtained in rolling", m, "dice", sep=" ")
  plot<-barplot(table(threes), main=title)
  return(plot)
}

# Q6 Write a function DrawCardsWithReplacement(n,m) that simulates drawing m 
# cards from a 52-card deck with replacement, n times.
DrawCardsWithReplacement = function(n, m)
{
  red_counts <- c(1:n)
  for (i in 1:n)
  {
    draws <- sample(c(0,1), m, replace = TRUE)
    red_counts[i] <- sum(draws== 1)
  }
  
  title<-paste("Number of red cards in", m,
                 "draws (with replacement)", sep=" ")
  
  plot<-barplot(table(red_counts), main = title,
                  xlab = "Number of red cards",
                  ylab = "Frequency")
  return(plot)
}

# Q7 Now write a function DrawCardsWithoutReplacement(n,m) 
# that simulates drawing m cards from a 52-card deck without replacement, 
# n times. For each draw, record the number of red cards. 
DrawCardsWithoutReplacement = function(n, m)
{
  deck <- c(rep(1,26), rep(0,26))
  red_counts <- c(1:n)
  
  for (i in 1:n)
  {
    draws <- sample(deck, m, replace = FALSE)
    red_counts[i] <- sum(draws == 1)
  }
  
  title <- paste("Number of red cards in", m,
                 "draws (without replacement)")
  
  plot <- barplot(table(red_counts),
                  main = title,
                  xlab = "Number of red cards",
                  ylab = "Frequency")
  return(plot)
}

