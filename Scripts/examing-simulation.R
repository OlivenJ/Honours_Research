set.seed(19970628)

t <- 120
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 500
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 30

alpha <- 0.55
#The inital factor strengh setting, alpha equals to 0.55 (indicates weak factor)

epsilon <- rnorm(t, 0, 1)
mu <- runif(t, epsilon-0.2, epsilon + 0.2)
hist(mu)
#Generate the error, for the baseline setting, the error term epsilon will be iid normal
#0 mean and 1 variance.

return <- runif(t,-0.5,0.5)
hist(return)
