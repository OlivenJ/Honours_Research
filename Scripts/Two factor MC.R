library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)
set.seed(19890604)

regression <- function(data, equation){
  data %>% 
    mutate(coefficient = map(data, equation)) %>% 
    unnest(coefficient)
}
strength_calc <- function(regress_result){
  
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    filter(term != "(Intercept)") %>% 
    group_by(factor, term) %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
  regress_result <- regress_result %>% 
    mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
    mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
  
  
}
market_calc <-function(regress_result){
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    filter(term == "market_factor_val") %>% 
    group_by(factor) %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
  regress_result <- regress_result %>% 
    mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
    mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
}
loading_generate<- function(nonzero_amount){
  loading <- matrix(0, n, k+1)
  for(i in 1:(k+1)){
    col <- runif(n,mu_var-0.2,  mu_var + 0.2)
    ind <- which(col %in% sample(col,n-nonzero_amount[i]))
    col[ind] <- 0
    loading[,i] <- col
  }
  return(loading)
}

result_table <- tibble()
cont = 0

times <- c(120, 240, 360)
unit <- c(100, 300, 500)
strength_seq <- seq(0.7, 1 ,0.05)

for(var1 in times){
  for(var2 in unit){
    for(var3 in strength_seq){
      
      cont<- cont +1
      
      
      
      rep = 1
      t <- var1
      n <- var2
      alpha <- var3
      k <- 1
      


repeat{

constant <- matrix(rep(runif(n, -0.5, 0.5), t), nrow = n, ncol = t)
#Assume the constant term ( the alpha term from CAPM model) follows uniform distribution
error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
#Assume the error term follows a standard normal distribution
strength <- c( rep(alpha, k),1)
#the factor strength, with the last term as market factor strength, which equals to 1
#represents that market factor influence all the strocks, the rest 30 factors have 
#strength 0.55, which is pretty low.
sig_count <- as.integer((n)^strength)

Sigma <- cbind(c(1, 0.3), c(0.3, 1))
factor <- t(rmvnorm(t,mean = rep(0, k+1), Sigma))

p = 0.05
sigma = 0.5
logn = log(n)

#Generate the factor vector with k factor and one market factor.
#Each facotr foolows a multinomial distribution with zeor mean and Sigma vairance


market_factor <- t(matrix(factor[nrow(factor),]))
colnames(market_factor) <- paste0("m", 1:t)
market_factor <- as_tibble(market_factor)%>% 
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "market_factor_val")
#Take out the market factor, and pack it into a tibble format.


#Indicates under the factor strength setting, how many factor laodings are significnatly
#different from 0. We only take the integer part. From the result we can see that 

adj_stat <- pnorm(1-(0.05/(2*n^1.5)))
###Notice that here has been changed into 1.5


mu_var = 0.71
loading <- loading_generate(sig_count)
#==============+==============+==============+==============+==============+==============+==============+#
##Wrap the generate factor and return into tibble
return <-  constant + loading%*%factor + error
colnames(return) <- paste0("m", 1:t)
return <- as_tibble(return) %>% mutate(stock = 1:n) %>% dplyr::select(stock, everything())
return <- return %>%
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "return")

colnames(factor) <- paste0("m", 1:t)
factor <- as_tibble(factor) %>% mutate(factor = 1:(k+1)) %>% dplyr::select(factor,everything())
factor <- factor %>% 
  pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "factor_val")
combine_table <- right_join(return, factor, by = "month")


#==============+==============+==============+==============+==============+==============+==============+#
##The setting of market factor and one other factor.

#add market factor as a separate variable of the data frame.
two_factor_group <- combine_table %>% 
  filter(factor != k+1) %>% 
  right_join(market_factor, by = 'month') %>% 
  dplyr::select(stock, factor,return, market_factor_val, factor_val) %>% 
  group_by(stock,factor) %>% nest()


#Set the regression function
two_factor_regress <-function(data){
  tidy(lm(return ~ factor_val + market_factor_val, data))
}
#Run the regression and unnest the results
two_factor <- two_factor_group %>% 
  regression(two_factor_regress)

#Caculate the factor strength
two_factor_result <- two_factor %>% 
  strength_calc() %>% 
  rename(model = factor)



two_factor_result <- two_factor_result %>% 
  mutate(time = t, unit = n, alpha = alpha) %>% 
  dplyr::select(time, unit, alpha, everything()) %>% 
  mutate(bias = if_else(alpha_hat == 1,0, alpha_hat - alpha)) %>% 
  mutate( part1 = alpha_hat - alpha,
          part2 = n - n^alpha_hat,
          power_part1 = -sigma-alpha_hat,
          power_part2 = -sigma-2*alpha_hat,
          part3 = (1 - p/n^sigma),
          numerator = logn*part1 - p*part2*n^power_part1,
          denominator = sqrt(p*part2*n^power_part2*part3),
          z = numerator/denominator) %>% 
  mutate(size_count = if_else(alpha_hat == 1,0,if_else(abs(z) > 1.96, 1, 0)) )
  

result_table <- result_table %>% bind_rows(two_factor_result)



print(nrow(result_table)/2)
if(nrow(result_table)/2 == cont * rep ){
  print("Repeat finish")
  break
}
}

  }
 }
}

summary_result <-  result_table %>% group_by(time, unit, alpha) %>% 
  filter(term == "factor_val") %>% 
  mutate(sqr = bias^2) %>% 
  summarize(alpha_hat = mean(alpha_hat),
            bias = mean(bias),
            RMSE = sqrt(sum(sqr)/n),
            size = sum(size_count)/rep)


view(summary_result)
view(result_table)