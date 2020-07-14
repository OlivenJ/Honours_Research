library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)

ten_return<- inner_join(ten_return, risk_factors, by = "Date") %>% group_by(Factor) %>% 
  nest()
twenty_return<- inner_join(twenty_return, risk_factors, by = "Date") %>% group_by(Factor) %>% 
  nest()
thirty_return<- inner_join(thirty_return, risk_factors, by = "Date") %>% group_by(Factor) %>% 
  nest()



model <- function(data){
  lm(Excess ~ Value + 1, data = data)
}

strength_calc <- function(return_data, n){
  result_table <- tibble(Factor = character(), pi = double(), strength = double())
  for (i in 1:length(unique(risk_factors$Factor))) {
    group_lm <- return_data[i,] %>% unnest(cols = data) %>% group_by(Ticker) %>% nest() %>% 
      mutate(model = map(data,model)) 
    
    result <- group_lm %>% 
      mutate(summary = map(model, broom::glance)) %>% 
      unnest(summary)
    
    result$sig_indi <-  if_else(abs(result$statistic) > qnorm(1-(0.05/(2*n^0.5))), 1, 0)
    result_table <- result_table %>%  add_row( tibble_row(Factor = as.character(return_data[1][i,]), 
                                                          pi = sum(result$sig_indi)/n, 
                                                          strength = 1+log(sum(result$sig_indi)/n )/log(n)))
    
    print(i)
  }
  return(result_table)
}

test_result <-  ten_return[1,] %>% unnest(cols = data) %>% group_by(Ticker) %>% nest() %>% 
  mutate(model = map(data, model)) %>% 
   mutate(summary = map(model, broom::tidy)) %>% 
   unnest(summary)

test_result$sig_indi <- if_else(abs(test_result$statistic)>3, 1,0)
result_table %>% add_row(tibble_row(Factor = as.character(ten_return[1][1,]),
                                    pi = sum(test_result$sig_indi)/n,
                                    strength = 1 + log(sum(test_result$sig_indi)/n)/log(n)))

ten_result <- strength_calc(ten_return, n=419)
twenty_result <- strength_calc(twenty_return,n=342)
thirty_result <- strength_calc(thirty_return, n=242)

ten_result %>% arrange(strength)
twenty_result %>% arrange(strength)
thirty_result %>% arrange(strength)

ten_result %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

twenty_result %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

ten_result %>% 
  rename(ten_strength = strength) %>% 
  right_join(twenty_result, by = "Factor") %>% 
  rename(twenty_strength = strength) %>% 
  right_join(thirty_result, by = "Factor") %>% 
  rename(thirty_strength = strength) %>% 
  select(-pi.x, -pi, -pi.y) %>% 
  mutate(mean = (ten_strength+twenty_strength+thirty_strength)/3,
         var = ((ten_strength-mean)^2 +(twenty_strength - mean)^2 +(thirty_strength - mean)^2)/3,
         std = sqrt(var)) %>% 
  arrange(desc(std), mean) %>% 
  view()

ten_result %>% filter(strength >= 0.7)  %>% arrange(desc(strength))
twenty_result %>% filter(strength >= 0.7)  %>% arrange(desc(strength))
thirty_result %>% filter(strength >= 0.7)  %>% arrange(desc(strength))
