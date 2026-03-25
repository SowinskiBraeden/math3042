# 2
r <- 1 / 20
qexp(0.5, rate = r)

# 3a
r <- 1 / 20
pexp(10, rate = r)

#3b
r <- 1 / 20
1 - pexp(15, rate = r)

#3c
r <- 1 / 20
pexp(10, rate = r) - pexp(5, rate = r)

# 4
set.seed(123)

r <- 1 / 20

wait100 <- rexp(100, rate = r)
wait1000 <- rexp(1000, rate = r)
wait10000 <- rexp(10000, rate = r)

par(mfrow = c(1, 3))

hist(wait100,
     breaks = 20,
     main = "Bus Waits (n = 100)",
     xlab = "Waiting time (minutes)",
     ylab = "Frequency")

hist(wait1000,
     breaks = 20,
     main = "Bus Waits (n = 1000)",
     xlab = "Waiting time (minutes)",
     ylab = "Frequency")

hist(wait10000,
     breaks = 20,
     main = "Bus Waits (n = 10000)",
     xlab = "Waiting time (minutes)",
     ylab = "Frequency")

# 5
set.seed(123)

r <- 1 / 20
waits <- rexp(10000, rate = r)
mean(waits < 10)

#6a
pnorm(9.03, mean = 9.01, sd = 0.05)

#6b
1 - pnorm(9.02, mean = 9.01, sd = 0.05)

#6c
pnorm(9.1, mean = 9.01, sd = 0.05) - pnorm(8.9, mean = 9.01, sd = 0.05)

#7a
qnorm(0.95, mean = 9.01, sd = 0.05)

#7b
qnorm(0.05, mean = 9.01, sd = 0.05)

#7c
qnorm(0.25, mean = 9.01, sd = 0.05)

#8
set.seed(123)

mu <- 9.01
sigma <- 0.05

b100 <- rnorm(100, mean = mu, sd = sigma)
mean(b100 >= 8.9 & b100 <= 9.1)

b1000 <- rnorm(1000, mean = mu, sd = sigma)
mean(b1000 >= 8.9 & b1000 <= 9.1)

b10000 <- rnorm(10000, mean = mu, sd = sigma)
mean(b10000 >= 8.9 & b10000 <= 9.1)

pnorm(9.1, mean = 9.01, sd = 0.05) - pnorm(8.9, mean = 9.01, sd = 0.05)