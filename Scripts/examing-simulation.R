library(mvtnorm)

set.seed(19970628)

t <- 120
#the time periods, i.e. the observations amount: we mimic the real stock's monthely
#return, 12 months a year and 10 years, so we will generate 120 different returns 
n <- 500
#How many independent cross-section observation we have. Since in the real data scenario
#we will use S&P 500, so we set the sample size to 500 to mimic it.
k <- 30
# the number of factors
alpha <- c(1, rep(0.55, k))
#the factor strength, with the first term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.
rf <- 0.04
#randomly assigned risk free rate as 4%
Sigma <- diag(1, k)
Y <- c(0.1, rmvnorm(1,rep(0, nrow(Sigma)), Sigma))
#random generate factor list, the first one is the market facotr which equals to 
#the difference between makret return and risk free return

#assign the factor strengh, for all the factor, the strengh is 0.55 (weak factor), and
#for the only market factor, is strong (alpha = 1)

sig_fact <- as.integer(k^alpha)
#Indicates under the factor strength setting, how many factor laodings are significnatly
#different from 0. We only take the integer part.

mu_var = 0.71
mu <- matrix(runif(t*(k+1),mu_var-0.2, mu_var + 0.2),nrow = t, ncol = k+1)
hist(mu)
#Generate the error, for the baseline setting, the error term epsilon will be iid normal
#0 mean and 1 variance.

constant <- runif(t, -0.5, 0.5)
