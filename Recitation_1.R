# Lecture 1 example:
1-2*pnorm(1.8, lower.tail = FALSE)

qnorm(0.95)
# Exercise 7.3
dice <- 1:6

## Roll the dice for one time
sample(dice, size=1, prob = rep(1/6,6))
# Check the help file for sample() using the following line 11,
# and choose "Random Samples and Permutations" option.
?sample
## To get a random sample of size 3, 
## we need to roll the dice for 3 times (repeat the above line 9 3 times)

# 1. Use do() function in package "mosaic". do(n) will repeat the code n times.
install.packages("mosaic")
library(mosaic)
do(3)*sample(dice, size=1, prob = rep(1/6,6))
# 2. Or use sample function and set size=3, replace =TRUE
sample(dice, size=3, replace=TRUE, prob = rep(1/6,6))

## Sample mean of a random sample of size n=3. 
## That is, roll the dice 3 times and calculate the mean of 3 tosses
mean(sample(dice, size=3, replace=TRUE, prob = rep(1/6,6)))


# (a) a single random sample of size n=3, or 12
## n=12
sample_mean12 <- do(100)*mean(sample(dice, size=12, replace=TRUE, prob = rep(1/6,6)))
hist(sample_mean12$mean,breaks = 10) # similar shape, smaller spread

## n=3
sample_mean3 <- do(100)*mean(sample(dice, size=3, replace=TRUE, prob = rep(1/6,6)))
hist(sample_mean3$mean,breaks = 10)

# (b)
## n=12
sample_mean12 <- do(100)*mean(sample(dice, size=12, replace=TRUE, prob = rep(1/6,6)))
mean(sample_mean12$mean)
sd(sample_mean12$mean)

## n=3
sample_mean3 <- do(100)*mean(sample(dice, size=3, replace=TRUE, prob = rep(1/6,6)))
mean(sample_mean3$mean)
sd(sample_mean3$mean)

sd(sample_mean12$mean)/sd(sample_mean3$mean)

# Theoretically,
sqrt(2.9167/12)/sqrt(2.9167/3)

# (c)
par(mfrow=c(3,1))

sample_mean3 <- do(10000)*mean(sample(dice, size=3, replace=TRUE, prob = rep(1/6,6)))
hist(sample_mean3$mean, probability = TRUE, main = "n=3") 
lines(x=seq(1,6,0.05), y = dnorm(seq(1,6,0.05), mean = 3.5, sd = sqrt(2.9167/3)))

sample_mean12 <- do(10000)*mean(sample(dice, size=12, replace=TRUE, prob = rep(1/6,6)))

hist(sample_mean12$mean, probability = TRUE,main = "n=12") 
lines(x=seq(1,6,0.05), y = dnorm(seq(1,6,0.05), mean = 3.5, sd = sqrt(2.9167/12)))

sample_mean30 <- do(10000)*mean(sample(dice, size=30, replace=TRUE, prob = rep(1/6,6)))
hist(sample_mean30$mean, probability = TRUE,main = "n=30") 
lines(x=seq(1,6,0.05), y = dnorm(seq(1,6,0.05), mean = 3.5, sd = sqrt(2.9167/30)))

