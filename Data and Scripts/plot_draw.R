long_three_combine <- three_combine %>% 
  rename(ten = ten_strength, twenty = twenty_strength, thirty = thirty_strength) %>% 
  select(Factor, ten, twenty, thirty, mean, std)  %>% 
  pivot_longer(c(ten, twenty, thirty), names_to = "period", values_to = "strength") %>% 
  inner_join(factor_ancillary, by = "Factor")

long_three_combine$period <- factor(long_three_combine$period, levels = c("thirty", "twenty", "ten"))

long_three_combine %>% ggplot(aes(x  = Year, y = strength, color = period))+
  geom_jitter()

long_three_combine %>% ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.7,linetype = "dashed", color = "red")+
  facet_wrap(vars(Factor))

long_thirty_combine<- thirty_combine %>% 
  inner_join(factor_ancillary, by = "Factor") %>% 
  rename(one = Strength.one, two = Strength.two, three = Strength.three) %>% 
  select(Factor, one, two, three, Year) %>% 
  pivot_longer(c(one, two, three), names_to = "period", values_to = "strength")

long_thirty_combine$period <- factor(long_thirty_combine$period, levels = c("three", "two", "one"))

long_thirty_combine %>% ggplot(aes(x  = Year, y = strength, color = period))+
  geom_point()

long_thirty_combine %>% ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.7,linetype = "dashed", color = "red")+
  facet_wrap(vars(Factor))

long_pre_GFC <- pre_GFC_combine%>% 
  select(Factor, one, two, three) %>% 
  pivot_longer(c(one, two, three), names_to = "period", values_to = "strength")

long_pre_GFC$period<- factor(long_pre_GFC$period, levels = c("one", "two", "three"))

long_pre_GFC %>%  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.7,linetype = "dashed", color = "red")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% inner_join(long_pre_GFC, by = c("Factor", "period")) %>% 
  rename(incl.GFC = strength.x, excl.GFC = strength.y) %>% 
  mutate(diff = abs(incl.GFC - excl.GFC)) %>% 
  #filter(period == "three") %>% 
  filter(diff >= 0.1) %>% 
  ggplot(aes(period))+
  geom_bar()


long_twenty_combine <-twenty_combine %>% 
  rename(older = strength_one, younger = strength_two) %>% 
  inner_join(factor_ancillary, by = "Factor") %>% 
  pivot_longer(c(older, younger), names_to = "period", values_to = "strength") 


long_twenty_combine %>% ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.7,linetype = "dashed", color = "red")+
  facet_wrap(vars(Factor))
