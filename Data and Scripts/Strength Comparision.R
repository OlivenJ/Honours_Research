library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)
library(ggpubr)


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

half_thirty <- thirty_return %>% filter(Date >= ymd("2003-01-01") & Date < ymd("2018-01-01")) %>% 
  group_by(Factor) %>% 
  nest()

one_thirty_result <- strength_calc(one_thirty)
two_thirty_result <- strength_calc(two_thirty)
three_thirty_result <- strength_calc(three_thirty)
half_thirty_result <- strength_calc(half_thirty)

one_thirty_result
three_ten_result

two_thirty_result
two_ten_result

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
  inner_join(thirty_result, by = "Factor") %>% 
  select(Factor,strength, Strength.one,Strength.two, Strength.three) %>% 
  mutate(mean = (Strength.one+Strength.two+Strength.three)/3,
                                           var = ((Strength.one-mean)^2 +(Strength.two - mean)^2 +(Strength.three - mean)^2)/3,
                                           std = sqrt(var)) 

xtable(thirty_combine %>% arrange(desc(strength)) %>% 
  select(-mean, - var), digits = c(0,0,3,3,3,3,3))

half_thirty_result %>% inner_join(fifty_result, by = "Factor") %>% 
  select(-c(Market_strength.x, pi.x, Market_strength.y, pi.y)) %>% 
  rename(half_thirty = strength.x, fifty = strength.y) %>% 
  mutate(diff = abs(half_thirty - fifty)) %>% 
  arrange(desc(diff)) %>% view()

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
    inner_join(twenty_result,by = "Factor") %>% 
    select(Factor,strength, everything(),-Market_strength, -pi,diff) 
  
  xtable(twenty_combine %>% arrange(desc(diff)), digits = c(0,0,3,3,3,3))
  
  ten_plot <- ten_result %>% inner_join(factor_ancillary, by = "Factor") %>% 
    filter(strength >= 0.5) %>% 
    ggplot(aes(x = Year , y = strength)) +
    geom_point()
  
  
  twenty_plot <- twenty_result %>% inner_join(factor_ancillary, by = "Factor") %>% 
    filter(strength >= 0.5) %>% 
    ggplot(aes(x = Year , y = strength)) +
    geom_point()
  
  thirty_plot <- thirty_result %>% inner_join(factor_ancillary, by = "Factor") %>% 
    filter(strength >= 0.5) %>% 
    ggplot(aes(x = Year , y = strength)) +
    geom_point()
  
  thirty_result %>% inner_join(twenty_result, by = "Factor") %>% 
    inner_join(factor_ancillary, by = "Factor") %>% 
    filter(strength.x >= 0.5 & strength.y >= 0.5) %>% 
    select(Factor, Name, Year, strength.x, strength.y) %>% 
    rename(thirty_strength = strength.x, twenty_stregth = strength.y) %>% 
    pivot_longer(-c(Factor, Name, Year),names_to = "type", values_to = "Value" ) %>% 
    ggplot(aes(x = Year, y = Value, color = type))+
    geom_point()
  
  
  
  ggarrange(ten_plot, twenty_plot, thirty_plot)
  
  ggarrange(twenty_plot, thirty_plot)
  
  twenty_thirty_combine <- twenty_result %>% inner_join  (thirty_result, by = "Factor") %>% 
    rename(twenty_strength = strength.x, thirty_strength = strength.y) %>% 
    select(Factor, twenty_strength, thirty_strength) %>% 
    mutate(diff = abs(twenty_strength - thirty_strength))  
    
  
  ten_twenty_combine <- ten_result %>% inner_join  (twenty_result, by = "Factor") %>% 
    rename(ten_strength = strength.x, twenty_strength = strength.y) %>% 
    select(Factor, ten_strength, twenty_strength) %>% 
    mutate(diff = abs(ten_strength - twenty_strength)) 

  xtable(twenty_thirty_combine %>% arrange(desc(diff)), digits = c(0,0,3,3,3))
  
  ten_thirty_combine <- ten_result %>% inner_join  (thirty_result, by = "Factor") %>% 
    rename(ten_strength = strength.x, thirty_strength = strength.y) %>% 
    select(Factor, ten_strength, thirty_strength) %>% 
    mutate(diff = abs(ten_strength - thirty_strength)) 
  
  xtable(tibble(pair = c("ten_twenty", 'ten_thirty', "twenty_thirty"), 
         RMSE = c(sqrt(sum(ten_twenty_combine$diff^2)/length(ten_twenty_combine$diff)),
                  sqrt(sum(ten_thirty_combine$diff^2)/length(ten_thirty_combine$diff)),
                  sqrt(sum(twenty_thirty_combine$diff^2)/length(twenty_thirty_combine$diff)))))
  
  
  #=========================#======================#=======================#
  #=========================#======================#=======================#
  #Decade Comparsion#
  third_decade <- ten_result %>% inner_join(two_twenty_result, by = "Factor") %>% 
    rename(ten= strength.x,twenty =  strength.y)  %>% 
    inner_join(three_thirty_result, by = "Factor") %>% 
    rename(thirty = strength) %>% 
    select(Factor, ten, twenty, thirty) %>% 
    mutate(mean = (ten+twenty+thirty)/3,
           var = ((ten-mean)^2 +(twenty - mean)^2 +(thirty - mean)^2)/3,
           std = sqrt(var)) 

  third_decade %>% arrange(desc(std))  %>% view()
  
  third_decade %>% 
    ggplot(aes(x = mean, y = std))+
    geom_jitter(width = 0.05)+
    geom_smooth(method = lm)
  
    lm(std ~ mean + 1, third_decade) %>% tidy()
  
  xtable(ten_result %>% arrange(desc(strength)) %>% bind_cols(twenty_result %>% arrange(desc(strength))) %>% 
    bind_cols(thirty_result %>% arrange(desc(strength))))
  twenty_result %>% arrange(desc(strength)) 
  thirty_result %>% arrange(desc(strength)) 
  
  
  sub_ten_twenty_combine <-ten_result %>% inner_join(two_twenty_result, by = "Factor") %>% 
    select(-c(Market_strength.x, Market_strength.y, pi.x, pi.y)) %>% 
    rename(ten = strength.x, twenty = strength.y) %>% 
    mutate(diff = ten -twenty)
    
    sub_ten_thirty_combine <- ten_result %>% inner_join(three_thirty_result, by = "Factor") %>% 
      select(-c(Market_strength.x, Market_strength.y, pi.x, pi.y)) %>% 
      rename(ten = strength.x, thirty = strength.y) %>% 
      mutate(diff = ten -thirty)
    
    sub_twenty_thirty_combine <- two_twenty_result %>% inner_join(three_thirty_result, by = "Factor") %>% 
      select(-c(Market_strength.x, Market_strength.y, pi.x, pi.y)) %>% 
      rename(twenty = strength.x, thirty = strength.y) %>% 
      mutate(diff = twenty -thirty)
  
    tibble(pair = c("ten_twenty", 'ten_thirty', "twenty_thirty"), 
           RMSE = c(sqrt(sum(sub_ten_twenty_combine$diff^2)/length(sub_ten_twenty_combine$diff)),
                    sqrt(sum(sub_ten_thirty_combine$diff^2)/length(sub_ten_thirty_combine$diff)),
                    sqrt(sum(sub_twenty_thirty_combine$diff^2)/length(sub_twenty_thirty_combine$diff))))
    
    first_boom_result %>% inner_join(post_GFC_result, by  = "Factor") %>% 
      select(-c(Market_strength.x, Market_strength.y, pi.x, pi.y)) %>% 
      rename(boom_strength = strength.x, post_GFC_strength = strength.y) %>% 
      mutate(diff = abs(boom_strength - post_GFC_strength)) %>% arrange(desc(diff)) %>% 
      view()

    first_boom_result %>% inner_join(control_group_result, by  = "Factor") %>% 
      select(-c(Market_strength.x, Market_strength.y, pi.x, pi.y)) %>% 
      rename(boom_strength = strength.x, control_group_strength = strength.y) %>% 
      mutate(diff = abs(boom_strength - control_group_strength)) %>% arrange(desc(diff)) %>% 
      view()
    
    three_combine %>% 
      ggplot(aes(x = mean, y = std))+
      geom_jitter(width = 0.01)+
      geom_smooth(method = lm)
    
    lm(std~mean, three_combine) %>% tidy()
    
    #=========================#======================#=======================#
    #=========================#======================#=======================#
    one_pre_GFC <- pre_GFC_thirty_return %>% filter(Date >= ymd("1986-01-01") & Date < ymd("1996-01-01")) %>% 
      group_by(Factor) %>% 
      nest()
    
    two_pre_GFC <- pre_GFC_thirty_return %>% filter(Date >= ymd("1997-01-01") & Date < ymd("2007-01-01")) %>% 
      group_by(Factor) %>% 
      nest()
    
    three_pre_GFC <- pre_GFC_thirty_return %>% filter(Date >= ymd("2007-01-01") & Date < ymd("2017-01-01")) %>% 
      group_by(Factor) %>% 
      nest()
    
    one_pre_GFC_result <- strength_calc(one_pre_GFC)
    two_pre_GFC_result <- strength_calc(two_pre_GFC)
    three_pre_GFC_result <- strength_calc(three_pre_GFC)
    
    pre_GFC_combine <- one_pre_GFC_result %>% 
      inner_join(two_pre_GFC_result, by = "Factor") %>% 
      select(-c(Market_strength.x,pi.x,Market_strength.y, pi.y)) %>% 
      rename(one = strength.x,
             two = strength.y) %>% 
      inner_join(three_pre_GFC_result, by = "Factor") %>% 
        select(-Market_strength, -pi) %>% 
      rename(three = strength) %>% 
      inner_join(pre_GFC_thirty_result, by = "Factor") %>% 
      select(Factor,strength, one,two, three) %>% 
      mutate(mean = (one+two+three)/3,
             var = ((one-mean)^2 +(two - mean)^2 +(three - mean)^2)/3,
             std = sqrt(var)) 

  label_strength<- function(result){
    result <- result %>% add_column(level = rep("", nrow(result)))
    for(i in 1:nrow(result)){
    if(result[i,]$strength >= 0.9){
      result[i,]$level = "above09"
    }else if(result[i,]$strength < 0.9 & result[i,]$strength >= 0.85){
      result[i,]$level = "085to09"
    }else if(result[i,]$strength < 0.85 & result[i,]$strength >= 0.8){
      result[i,]$level = "08to085"
    }else if(result[i,]$strength < 0.8 & result[i,]$strength >= 0.75){
      result[i,]$level = "075to08"
    }else if(result[i,]$strength < 0.75 & result[i,]$strength >= 0.7){
      result[i,]$level = "07to075"
    }else if(result[i,]$strength < 0.7 & result[i,]$strength >= 0.65){
      result[i,]$level = "065to07"
    }else if(result[i,]$strength < 0.65 & result[i,]$strength >= 0.6){
      result[i,]$level = "06to065"
    }else if(result[i,]$strength < 0.6 & result[i,]$strength >= 0.55){
      result[i,]$level = "055to06"
    }else if(result[i,]$strength < 0.55 & result[i,]$strength >= 0.5){
      result[i,]$level = "05to055"
    }else{
      result[i,]$level = "below05"
    }
    }
    return(result)
  }

  
  
ten_result<- label_strength(ten_result) 
twenty_result<- label_strength(twenty_result)
thirty_result<- label_strength(thirty_result)

ten_result$level <- ten_result$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
twenty_result$level <- twenty_result$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
thirty_result$level <- thirty_result$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))

ten_result %>% ggplot(aes(x = level))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)

twenty_result  %>% ggplot(aes(x = level))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)
 
thirty_result %>% ggplot(aes(x = level))+
  geom_bar()+
  geom_text(stat='count', aes(label=..count..), vjust=-1)
      
ten_result %>% group_by(level) %>% 
  summarise(proportion = n()/145*100)

twenty_result %>% group_by(level) %>% 
  summarise( proportion = n()/145*100)

thirty_result %>% group_by(level) %>% 
  summarise(proportion = n()/145*100)

