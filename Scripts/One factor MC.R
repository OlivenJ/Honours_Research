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
    #select(-data) %>% 
    filter(term == "factor_val") %>% 
    group_by(factor) %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
  regress_result <- regress_result %>% 
    mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
    mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
}
market_calc <-function(regress_result){
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    dplyr::select(-data) %>% 
    filter(term == "market_factor_val") %>% 
    group_by(factor) %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
  regress_result <- regress_result %>% 
    mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
    mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
}
loading_generate<- function(nonzero_amount){
  loading <- matrix(0, n, k)
  for(i in 1:(k)){
    col <- runif(n,mu_var-0.2,  mu_var + 0.2)
    ind <- which(col %in% sample(col,n-nonzero_amount[i]))
    col[ind] <- 0
    loading[,i] <- col
  }
  return(loading)
}

result_table <- tibble()

times <- c(120, 240, 360)
unit <- c(100, 300, 500)
strength <- c(0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 1)


  

rep = 200
  t <- 120
  n <- 500
  alpha = 0.7
  k <- 1
  
repeat{
  #constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
  constant <- matrix(rep(runif(n, -0.5, 0.5), t), nrow = n, ncol = t)
  #Assume the constant term ( the alpha term from CAPM model) follows uniform distribution
  error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
  #Assume the error term follows a standard normal distribution
  strength <- c(rep(alpha, k),1)
  #the factor strength, with the last term as market factor strength, which equals to 1
  #represents that market factor influence all the strocks, the rest 30 factors have 
  #strength 0.55, which is pretty low.
  sig_count <- as.integer((n)^strength)
  
  Sigma <- diag(1,k)
  factor <- t(rmvnorm(t,mean = rep(0, k), Sigma))

  
  adj_stat <- pnorm(1-(0.05/(2*n^0.25)))
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
  factor <- as_tibble(factor) %>% mutate(factor = 1:(k)) %>% dplyr::select(factor,everything())
  factor <- factor %>% 
    pivot_longer(c(paste0("m", 1:t)), names_to = "month", values_to = "factor_val")
  combine_table <- right_join(return, factor, by = "month")
  
  #==============+==============+==============+==============+==============+==============+==============+#
  
  one_factor_group <- combine_table %>% 
    group_by(stock,factor) %>% nest()
  
  one_factor_regress <- function(data){
    tidy(lm(return ~ factor_val, data))
  }
  
  one_factor <- one_factor_group %>% 
    regression(one_factor_regress)
  

  one_factor_result <- one_factor %>% 
    strength_calc()
  
  one_factor_result <- one_factor_result %>% 
    mutate(bias = alpha_hat - alpha) %>% 
          mutate( z = (log(n)*(alpha_hat-alpha)-0.05*(n-n^alpha_hat)*n^(-0.5-alpha_hat))/
             sqrt(0.05*(n-n^alpha_hat)*(n^(-0.5-2*alpha_hat))*(1-(0.05/(n^0.5)))) ) %>% 
    mutate(size_count = if_else(abs(z) > 1.96, 1, 0)) %>% 
    rename(repe = factor)
  
  result_table <- result_table %>% bind_rows(one_factor_result)
  
#==============+==============+==============+==============+==============+==============+==============+#
  
  print(nrow(result_table))
  if(nrow(result_table) == rep ){
    print("Repeat finish")
    break
  }
}

result_table %>% 
  mutate(sqr = bias^2) %>% 
  summarize(alpha_hat = mean(alpha_hat),
            bias = mean(bias),
            RMSE = sqrt(sum(sqr)/n),
            size = sum(size_count)/rep)