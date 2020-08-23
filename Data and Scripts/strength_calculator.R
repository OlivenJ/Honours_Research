library(tidyverse)
library(broom)
library(mvtnorm)
library(xtable)

#=============================================================================================#
#===========================#Calculate the factor strength#===================================#

ten_strength <- strength_calc(ten_factor_return)
twenty_strength <- strength_calc(twenty_factor_return)
thirty_strength <- strength_calc(thirty_factor_return)
one_thirty_strength <- strength_calc(one_thirty_factor_return)
two_thirty_strength <- strength_calc(two_thirty_factor_return)
three_thirty_strength <- strength_calc(three_thirty_factor_return)

ten_strength %>% arrange(desc(strength))
twenty_strength %>% arrange(desc(strength)) %>% view()
thirty_strength %>% arrange(desc(strength)) %>% view()
one_thirty_strength %>% arrange(desc(strength)) %>% view()
two_thirty_strength %>% arrange(desc(strength)) %>% view()
three_thirty_strength %>% arrange(desc(strength)) %>% view()

#=============================================================================================#
#===========================#Classify the factor strength#====================================#

ten_strength<- label_strength(ten_strength) 
twenty_strength<- label_strength(twenty_strength)
thirty_strength<- label_strength(thirty_strength)
one_thirty_strength<- label_strength(one_thirty_strength)
two_thirty_strength<- label_strength(two_thirty_strength)
three_thirty_strength<- label_strength(three_thirty_strength)

ten_strength$level <- ten_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
twenty_strength$level <- twenty_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
thirty_strength$level <- thirty_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
one_thirty_strength$level <- one_thirty_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
two_thirty_strength$level <- two_thirty_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))
three_thirty_strength$level <- three_thirty_strength$level %>% factor(levels= c("above09","085to09","08to085","075to08","07to075","065to07","06to065", "055to06", "05to055", "below05"))

ten_strength %>% arrange(desc(strength)) %>% select(Factor, Market_strength,strength) %>% 
  add_column(twenty_strength %>% arrange(desc(strength)) %>% select(Factor, Market_strength,strength))
thirty_strength %>% arrange(desc(strength)) %>% view()


