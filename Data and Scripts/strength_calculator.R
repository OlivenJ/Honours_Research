library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)



nested_ten_return<- ten_return %>% 
  group_by(Factor) %>% 
  nest()

nested_fifty_return <- fifty_return %>% 
  group_by(Factor) %>% 
  nest()
  
nested_twenty_return<- twenty_return %>% 
  group_by(Factor) %>% 
  nest()

nested_thirty_return<- thirty_return %>% 
  group_by(Factor) %>% 
  nest()

nested_post_GFC_return<- post_GFC_return %>% 
  group_by(Factor) %>% 
  nest()

nested_first_boom_return <- first_boom_return %>% 
  group_by(Factor) %>% 
  nest()

nested_pre_GFC_thrity_return<- pre_GFC_thirty_return %>% 
  group_by(Factor) %>% 
  nest()

model <- function(data){
  lm(Excess ~ Market + Value + 1, data = data)
}

strength_calc <- function(return_data){
  result_table <- tibble(Factor = character(), Market_strength = double(), strength = double(), pi = double())
  n <- length((nested_ten_return[1,] %>% unnest(cols = c(data)))$Ticker %>% unique())
  for (i in 1:length(unique(risk_factor$Factor))) {
    result <- return_data[i,] %>% unnest(cols = data) %>% group_by(Ticker) %>% nest() %>% 
      mutate(model = map(data,model)) %>% 
      mutate(summary = map(model, broom::tidy)) %>% 
      unnest(summary) %>% 
      mutate(sig_indi = if_else(abs(statistic) > qnorm(1-(0.05/(2*n^0.5))), 1, 0)) %>% 
      group_by(term) %>% 
      nest()

    result_table <- result_table %>%  add_row( tibble_row(Factor = as.character(return_data[1][i,]), 
                                                          pi = sum((result[3,] %>% 
                                                                      unnest(cols = c(data)))$sig_indi)/n, 
                                                          strength = if_else(pi == 0,0, 1 + log(pi)/log(n)),
                                               Market_strength = 1 + log(sum((result[2,] %>% 
                                                                                unnest(cols = c(data)))$sig_indi)/n)/log(n))
    )
    print(i)
  }
  return(result_table)
}

ten_result <- strength_calc(nested_ten_return)
fifty_result <- strength_calc(nested_fifty_return)
twenty_result <- strength_calc(nested_twenty_return)
thirty_result <- strength_calc(nested_thirty_return)
post_GFC_result <- strength_calc(nested_post_GFC_return)
first_boom_result <- strength_calc(nested_first_boom_return)
pre_GFC_thirty_result <- strength_calc(nested_pre_GFC_thrity_return) 

ten_result %>% arrange(desc(strength)) 
fifty_result %>% arrange(strength,pi,Market_strength)
twenty_result %>% arrange(desc(strength),pi,Market_strength)
thirty_result %>% arrange(desc(strength), pi, Market_strength) %>% view()
post_GFC_result %>% arrange(desc(strength), pi, Market_strength) %>% 
  view()
first_boom_result %>% arrange(desc(strength), pi, Market_strength)
pre_GFC_thirty_result %>% arrange(desc(strength))


ten_result %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

twenty_result %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

three_combine <- ten_result %>% 
  rename(ten_strength = strength) %>% 
  right_join(twenty_result, by = "Factor") %>% 
  rename(twenty_strength = strength) %>% 
  right_join(thirty_result, by = "Factor") %>% 
  rename(thirty_strength = strength) %>% 
  select(-pi.x, -pi, -pi.y) %>% 
  mutate(mean = (ten_strength+twenty_strength+thirty_strength)/3,
         var = ((ten_strength-mean)^2 +(twenty_strength - mean)^2 +(thirty_strength - mean)^2)/3,
         std = sqrt(var)) %>% 
  inner_join(factor_ancillary, by = "Factor") %>% select(-Name)

  three_combine %>% arrange(desc(std), mean) %>% 
  select(-Market_strength.x, -Market_strength.y,-Market_strength)

  
  ten_result %>% inner_join(thirty_result, by = "Factor") %>% 
    rename(Market_ten = Market_strength.x,
           strength_ten = strength.x,
           Market_thirty = Market_strength.y,
           strength_thirty = strength.y) %>% 
    select(-pi.x,-pi.y,-Market_ten, -Market_thirty) %>% 
    mutate(diff = abs(strength_ten - strength_thirty)) %>% 
    select(Factor,diff, everything()) %>% 
    view()
    
    
    #mutate(mean = (strength_ten + strength_thirty)/2,
     #      var = ((strength_ten - mean)^2+ (strength_thirty - mean)^2)/2,
      #     std = sqrt(var)) %>% view()
    
  
ten_result %>% filter(strength >= 0.5)  %>% arrange(desc(strength)) 
fifty_result %>% filter(strength >= 0.5) %>% arrange(desc(strength))
twenty_result %>% filter(strength >= 0.5)  %>% arrange(desc(strength))
thirty_result %>% filter(strength >= 0.5)  %>% arrange(desc(strength))

three_combine %>% filter(ten_strength >= 0.5) %>% 
  arrange(desc(std)) %>% select(-c(Market_strength, Market_strength.x, Market_strength.y)) %>% 
  view()

 


control_group <- semi_join(return, first_boom_name, by = "Ticker") %>% 
  filter(Date < ymd("2018-01-01") & Date > ymd("2009-04-01")) %>% 
  select(-Rf, -Adj.Close)

control_group$Date <- as.Date(control_group$Date)


control_group <- inner_join(control_group, risk_factor, by = "Date") %>% inner_join(Market_factor, by = "Date")%>% 
  rename(Factor = Factor.x, Value = Value.x, Market = Value.y) %>% 
  select(-Factor.y) 

nested_control_group <- control_group %>% 
  group_by(Factor) %>% 
  nest()
control_group_result <- strength_calc(nested_control_group)

control_group_result %>% 
  arrange(desc(strength)) %>% view()

control_group_result %>% inner_join(first_boom_result, by = "Factor") %>% 
  rename(early = strength.y, older = strength.x) %>% 
  select(Factor, early, older) %>% pivot_longer(c(early, older), names_to = "period", values_to = "strength") %>% 
  inner_join(factor_ancillary, by = "Factor") %>% 
  ggplot(aes(x = Year, y = strength, color = period))+
  geom_point()
  
