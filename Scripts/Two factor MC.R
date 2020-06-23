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

factor_strength_calc <- function(regress_result){
  
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    filter(term == "factor") %>% 
    summarize(pi_hat =sum(sig_indi)/n) 
  
  regress_result <- regress_result %>% 
    mutate(alpha_hat = 1 +log(pi_hat)/log(n)) %>% 
    mutate(alpha_hat = replace(alpha_hat, alpha_hat == -Inf, 0))
  return(regress_result)
  
  
}
market_strength_calc <- function(regress_result){
  
  regress_result$sig_indi <-  if_else(regress_result$statistic > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    filter(term == "market_factor") %>% 
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

factor_result_table <- tibble()
market_result_table <- tibble()
cont = 0

times <- c(120,240,360)
unit <- c(100,300,500)
strength_seq <- seq(0.7, 1 ,0.05)

for(var1 in times){
  for(var2 in unit){
    for(var3 in strength_seq){
      
      cont<- cont +1
      rep = 2000
      t <- var1
      n <- var2
      alpha <- var3
      k <- 1
      repeat{
        
        constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
        error <- matrix(rnorm(n*t, 0,1), nrow = n, ncol = t)
        strength <- c( rep(alpha, k),1)
        sig_count <- as.integer((n)^strength)
        
        Sigma <- cbind(c(1, 0), c(0, 1))
        factor <- t(rmvnorm(t,mean = rep(0, k+1), Sigma))
      
        p = 0.05
        delta = 0.5
        logn = log(n)
        
        market_factor <-factor[2,]
        factor <- factor[1,]
        
        
        adj_stat <- qnorm(1-(p/(2*n^delta)))
        mu_var = 0.71
        loading <- loading_generate(sig_count)
        market_loading <- loading[,2]
        factor_loading <- loading[,1]
        #==============+==============+==============+==============+==============+==============+==============+#
        return <-  constant +market_loading%*%t(market_factor) + factor_loading%*%t(factor) + error
  
        fit <- lm(t(return) ~ market_factor +factor+1) %>% tidy()

        factor_estimate <- fit %>% factor_strength_calc()
        market_estimate <- fit %>% market_strength_calc()
        
        factor_estimate <- factor_estimate %>% 
          mutate(time = t, unit = n, alpha = alpha) %>% 
          dplyr::select(time, unit, alpha, everything()) %>% 
          mutate(bias = if_else(alpha_hat == 1,0, alpha_hat - alpha)) %>% 
          mutate( bias = alpha_hat - alpha,
                  part2 = unit - unit^alpha_hat,
                  power_part1 = -delta-alpha_hat,
                  power_part2 = -delta-2*alpha_hat,
                  part3 = (1 - p/(n^delta)),
                  numerator = log(unit)*bias - p*part2*n^power_part1,
                  denominator = sqrt(p*part2*(unit^power_part2)*part3),
                  z = numerator/denominator) %>% 
          mutate(size_count = if_else(abs(z) > 1.96, 1, 0)) %>% 
          select(time, unit,alpha,pi_hat, alpha_hat, bias,size_count)
        
        market_estimate <- market_estimate %>% 
          mutate(time = t, unit = n, alpha = alpha) %>% 
          dplyr::select(time, unit, alpha, everything()) %>% 
          mutate(bias = if_else(alpha_hat == 1,0, alpha_hat - alpha)) %>% 
          mutate( bias = alpha_hat - alpha,
                  part2 = unit - unit^alpha_hat,
                  power_part1 = -delta-alpha_hat,
                  power_part2 = -delta-2*alpha_hat,
                  part3 = (1 - p/(n^delta)),
                  numerator = log(unit)*bias - p*part2*n^power_part1,
                  denominator = sqrt(p*part2*(unit^power_part2)*part3),
                  z = numerator/denominator) %>% 
          mutate(size_count = if_else(abs(z) > 1.96, 1, 0)) %>% 
          select(time, unit,alpha,pi_hat, alpha_hat, bias,size_count)
        
        
        factor_result_table <- factor_result_table %>% bind_rows(factor_estimate)
        market_result_table <- market_result_table %>% bind_rows(market_estimate)
        
        
        
        print(nrow(factor_result_table))
        if(nrow(factor_result_table) == cont * rep ){
          print("Repeat finish")
          break
        }
      }
      
    }
  }
}

factor_summary_result <-  factor_result_table %>% 
  group_by(time, unit, alpha) %>% 
  mutate(sqr = bias^2) %>% 
  summarize(alpha_hat = mean(alpha_hat),
            bias = mean(bias),
            RMSE = sqrt(sum(sqr)/rep),
            size = sum(size_count)/rep)

market_summary_result <-  market_result_table %>% group_by(time, unit, alpha) %>% 
  mutate(sqr = bias^2) %>% 
  summarize(alpha_hat = mean(alpha_hat),
            bias = mean(bias),
            RMSE = sqrt(sum(sqr)/rep),
            size = sum(size_count)/rep)

view(factor_summary_result)
view(market_summary_result)