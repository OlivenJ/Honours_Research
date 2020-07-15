library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)

#=========================#======================#=======================#
#=========================#======================#=======================#
one_thirty <- thirty_return %>% filter(Date >= ymd("1988-01-01") & Date < ymd("1998-01-01")) %>% 
  group_by(Factor) %>% 
  nest()

two_thirty <- thirty_return %>% filter(Date >= ymd("1998-01-01") & Date < ymd("2008-01-01")) %>% 
  group_by(Factor) %>% 
  nest()

three_thirty <- thirty_return %>% filter(Date >= ymd("2008-01-01") & Date < ymd("2018-01-01")) %>% 
  group_by(Factor) %>% 
  nest()

one_thirty_result <- strength_calc(one_thirty)
two_thirty_result <- strength_calc(two_thirty)
three_thirty_result <- strength_calc(three_thirty)

thirty_combine <- one_thirty_result %>% 
  inner_join(two_thirty_result, by = "Factor") %>% 
  rename(Market_strength.one = Market_strength.x,
         Strength.one = strength.x,
         Proportion.one = pi.x,
         Market_strength.two = Market_strength.y,
         Strength.two = strength.y,
         Proportion.two = pi.y) %>% 
  inner_join(three_thirty_result, by = "Factor") %>% 
  rename(Market_strength.three = Market_strength,
         Strength.three = strength,
         Proportion.three = pi) %>% 
  mutate(mean = (Strength.one+Strength.two+Strength.three)/3,
                                           var = ((Strength.one-mean)^2 +(Strength.two - mean)^2 +(Strength.three - mean)^2)/3,
                                           std = sqrt(var)) 


  view(thirty_combine %>% arrange(desc(std), mean) %>% 
  select(Factor, mean, std, mean, Strength.one, Strength.two, Strength.three) )

  #=========================#======================#=======================#
  #=========================#======================#=======================#
  one_twenty <- twenty_return %>% filter(Date >= ymd("1998-01-01") & Date < ymd("2008-01-01")) %>% 
    group_by(Factor) %>% 
    nest()  

  two_twenty <- twenty_return %>% filter(Date >= ymd("2008-01-01") & Date < ymd("2018-01-01")) %>% 
    group_by(Factor) %>% 
    nest()
  
  one_twenty_result <- strength_calc(one_twenty)
  two_twenty_result <- strength_calc(two_twenty)
  
  
  twenty_combine <- one_twenty_result %>% inner_join(two_twenty_result, by = "Factor") %>% 
    rename(Market_one = Market_strength.x,
           strength_one = strength.x,
           Market_two = Market_strength.y,
           strength_two = strength.y) %>% 
    select(-pi.x,-pi.y,-Market_one, -Market_two) %>% 
    mutate(diff = abs(strength_one - strength_two)) %>% 
    select(Factor,diff, everything()) 
  
  view(twenty_combine)
  