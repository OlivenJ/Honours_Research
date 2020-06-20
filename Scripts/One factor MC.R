library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)
set.seed(20202020)

strength_calc <- function(regress_result){
  regress_result$sig_indi <-  if_else(abs(regress_result$statistic) > adj_stat, 1, 0)
  regress_result <- regress_result %>% 
    summarize(pi_hat =sum(sig_indi)/n) %>% 
    mutate(alpha_hat =  1+log(pi_hat)/log(n))
  #notice the change formula 
  return(regress_result)
}

loading_generate<- function(nonzero_amount){
    loading <- matrix(0, n)
  
    col <- runif(n,mu_var-0.2,  mu_var + 0.2)
    ind <- which(col %in% sample(col,n-nonzero_amount[1]))
    col[ind] <- 0
    loading[,1] <- col
  
  return(loading)
}

#Three functions for running regression, calculate the strength and generate loadings

times <- c(120, 240,360)
unit <- c(100, 300,500)
strength_seq <- seq(0.7, 1, 0.05)
result_table <- tibble()
cont = 0  
for(var1 in times){
  for(var2 in unit){
    for(var3 in strength_seq){
      cont<- cont +1
      rep = 50
      t <- var1
      n <- var2
      alpha <- var3
      p = 0.05
      delta = 0.5
      logn = log(n)
      mu_var = 0.71
      adj_stat <- qnorm(1-(p/(2*n^delta)))
repeat{
  constant <- matrix(runif(n*t, -0.5, 0.5), nrow = n, ncol = t)
  error <- matrix(rnorm(t*n, 0,(1+rchisq(1,2))/3), nrow = n, ncol = t)
  sig_count <- as.integer((n)^alpha)
  Sigma <- diag(1)
  factor <-t(matrix(rnorm(t, 0,1)))
  #factor <-rep(0,t+50)
  #for(i in 2:(t+50)){
  #   factor[i]<- 0.5*factor[i-1]+sqrt(1-0.5^2)*rnorm(1)
  #}
  #factor<- factor[c(51:length(factor))]
  #factor <- t(factor)
  loading <- loading_generate(sig_count)
  return <-  constant + loading%*%factor + error
  #==============+==============+==============+==============+==============+==============+==============+#
  
  fit <- lm(t(return) ~ t(factor)+1) %>% 
    tidy() %>% filter(term == "t(factor)") 
  
  estimate <- fit %>% 
    strength_calc()

  estimate <- estimate %>% 
    mutate(time = t, unit = n, alpha = alpha) %>% 
    dplyr::select(time, unit, alpha, everything()) %>% 
    mutate( bias = alpha_hat - alpha,
                  part2 = unit - unit^alpha_hat,
                  power_part1 = -delta-alpha_hat,
                  power_part2 = -delta-2*alpha_hat,
                  part3 = (1 - p/(unit^delta)),
                  numerator = (log(unit)*bias),#- p*part2*unit^power_part1,
                  denominator = sqrt(p*part2*(unit^power_part2)*part3),
                  z = numerator/denominator) %>% 
    mutate(size_count = if_else(abs(z) > 1.96, 1, 0)) %>% 
    select(time, unit,alpha,pi_hat, alpha_hat, bias,size_count)
  
  result_table <- result_table %>% bind_rows(estimate)
  
#==============+==============+==============+==============+==============+==============+==============+#
  
  print(nrow(result_table))
  if(nrow(result_table) == cont * rep ){
    print("Repeat finish")
    break}
   }
  }
 }
}

summary_table <-  result_table %>%
    group_by(time, unit, alpha) %>% 
    summarize(alpha_hat = mean(alpha_hat),
            bias_report = mean(bias),
            RMSE = sqrt(sum(bias^2)/rep),
            size = sum(size_count)/rep)

view(summary_table)
view(result_table)
