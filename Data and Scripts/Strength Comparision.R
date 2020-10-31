library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)
library(ggpubr)
#=========================#======================#=======================#
#=========================#======================#=======================#
three_combine <- ten_strength %>% 
  rename(ten_strength = strength) %>% 
  right_join(twenty_strength, by = "Factor") %>% 
  rename(twenty_strength = strength) %>% 
  right_join(thirty_strength, by = "Factor") %>% 
  rename(thirty_strength = strength) %>% 
  select(-pi.x, -pi, -pi.y) %>% 
  mutate(mean = (ten_strength+twenty_strength+thirty_strength)/3,
         var = ((ten_strength-mean)^2 +(twenty_strength - mean)^2 +(thirty_strength - mean)^2)/3,
         std = sqrt(var)) %>% 
  inner_join(factor_ancillary, by = "Factor") %>% select(-Name)

xtable(three_combine %>% arrange(desc(std), mean) %>% 
         select(-Market_strength.x, -Market_strength.y,-Market_strength, -var, -Year), digits = c(0,0,3,3,3,3,3))

xtable(ten_strength %>% arrange(desc(strength)) %>% 
         rename(Market_ten = Market_strength, ten_strength = strength) %>% 
         select(-pi, -level)%>% 
         bind_cols(twenty_strength %>% arrange(desc(strength))) %>% 
         rename(Market_twenty = Market_strength) %>% 
         select(-pi, -level) %>% 
         bind_cols(thirty_strength %>% arrange(desc(strength))) %>% 
         rename(Market_thirty = Market_strength) %>% 
         select(-pi, -level), digits = c(0,0,3,3,0,3,3,0,3,3))


thirty_combine  <- one_thirty_strength %>% 
  inner_join(two_thirty_strength, by = "Factor") %>% 
  rename(Market_strength.one = Market_strength.x,
         Strength.one = strength.x,
         Proportion.one = pi.x,
         Market_strength.two = Market_strength.y,
         Strength.two = strength.y,
         Proportion.two = pi.y) %>% 
  inner_join(three_thirty_strength, by = "Factor") %>% 
  rename(Market_strength.three = Market_strength,
         Strength.three = strength,
         Proportion.three = pi) %>% 
  inner_join(thirty_strength, by = "Factor") %>% 
  select(Factor,strength, Strength.one,Strength.two, Strength.three) %>% 
  mutate(mean = (Strength.one+Strength.two+Strength.three)/3,
                                           var = ((Strength.one-mean)^2 +(Strength.two - mean)^2 +(Strength.three - mean)^2)/3,
                                           std = sqrt(var)) 

xtable(thirty_combine %>% arrange(desc(strength)) %>% 
  select(-mean, - var), digits = c(0,0,3,3,3,3,3))

xtable(one_thirty_strength %>% arrange(desc(strength)) %>% 
         rename( ten_strength = strength) %>% 
         select(-pi, -level,-Market_strength)%>% 
         bind_cols(twenty_strength %>% arrange(desc(strength))) %>% 
         rename(twenty_strength = strength) %>% 
         select(-pi, -level, -Market_strength) %>% 
         bind_cols(thirty_strength %>% arrange(desc(strength))) %>% 
         rename(thirty_strength = strength) %>% 
         select(-pi, -level, -Market_strength), digits = c(0,0,3,0,3,0,3))

  #=========================#======================#=======================#
  #=========================#======================#=======================#
  
  
  twenty_thirty_combine <- twenty_strength %>% inner_join  (thirty_strength, by = "Factor") %>% 
    rename(twenty_strength = strength.x, thirty_strength = strength.y) %>% 
    select(Factor, twenty_strength, thirty_strength) %>% 
    mutate(diff = abs(twenty_strength - thirty_strength))  
    
  
  ten_twenty_combine <- ten_strength %>% inner_join  (twenty_strength, by = "Factor") %>% 
    rename(ten_strength = strength.x, twenty_strength = strength.y) %>% 
    select(Factor, ten_strength, twenty_strength) %>% 
    mutate(diff = abs(ten_strength - twenty_strength)) 

  xtable(twenty_thirty_combine %>% arrange(desc(diff)), digits = c(0,0,3,3,3))
  
  ten_thirty_combine <- ten_strength %>% inner_join  (thirty_strength, by = "Factor") %>% 
    rename(ten_strength = strength.x, thirty_strength = strength.y) %>% 
    select(Factor, ten_strength, thirty_strength) %>% 
    mutate(diff = abs(ten_strength - thirty_strength)) 
  
  xtable(tibble(pair = c("ten_twenty", 'ten_thirty', "twenty_thirty"), 
         RMSE = c(sqrt(sum(ten_twenty_combine$diff^2)/length(ten_twenty_combine$diff)),
                  sqrt(sum(ten_thirty_combine$diff^2)/length(ten_thirty_combine$diff)),
                  sqrt(sum(twenty_thirty_combine$diff^2)/length(twenty_thirty_combine$diff)))))
  


ten_strength %>% group_by(level) %>% 
  summarise(proportion = n()/145*100) %>% 
  right_join(twenty_strength %>% group_by(level) %>% 
  summarise( proportion = n()/145*100), by = "level") %>% 
  right_join(thirty_strength %>% group_by(level) %>% 
               summarise( proportion = n()/145*100), by = "level") %>% 
  dplyr::select(level, ten = proportion.x ,  twenty = proportion.y, thirty = proportion) %>% 
  pivot_longer(-level, names_to = "length", values_to = "proportion") %>% 
  ggplot(aes(x = level, y = proportion, fill = factor(level)))+
  geom_bar(position = "dodge", stat = "identity")

thirty_strength %>% group_by(level) %>% 
  summarise(proportion = n()/145*100)

thirty_combine %>% mutate(one_two = Strength.one - Strength.two, two_three =Strength.two - Strength.three ) %>% 
  filter(one_two < 0 & two_three >0)
 
ten_strength %>% group_by(level) %>% 
  summarise(proportion = n()/145*100) %>% 
  right_join(twenty_strength %>% group_by(level) %>% 
               summarise( proportion = n()/145*100), by = "level") %>% 
  right_join(thirty_strength %>% group_by(level) %>% 
               summarise( proportion = n()/145*100), by = "level") %>% 
  dplyr::select(level, ten = proportion.x ,  twenty = proportion.y, thirty = proportion) %>% 
  pivot_longer(-level, names_to = "length", values_to = "proportion") %>% 
  ggplot(aes(x=level,y=proportion,fill=factor(length)))+
  geom_bar(stat="identity",position="dodge")+
  theme_minimal()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  theme(panel.background = element_blank())+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  guides(fill = guide_legend(title = "Data Time Length"))+
  xlab("Strength Group")+
  ylab("Proportion")

one_thirty_strength %>%
  select(Factor, one = strength) %>% 
  left_join(two_thirty_strength %>% select(Factor, two = strength), by = "Factor") %>% 
left_join(three_thirty_strength %>% select(Factor, three = strength), by = "Factor") %>% 
  mutate(changes = if_else(two > one, if_else(two > three, 1,0), 0)) %>% 
  summarise(sum(changes)/145)



