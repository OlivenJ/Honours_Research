library(corrplot)

ten_strength %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

twenty_strength %>% 
  ggplot(aes(x = strength)) +
  geom_histogram()

long_three_combine <- three_combine %>% 
  rename(ten = ten_strength, twenty = twenty_strength, thirty = thirty_strength) %>% 
  select(Factor, ten, twenty, thirty, mean, std)  %>% 
  pivot_longer(c(ten, twenty, thirty), names_to = "period", values_to = "strength") %>% 
  inner_join(factor_ancillary, by = "Factor")

long_three_combine$period <- factor(long_three_combine$period, levels = c("thirty", "twenty", "ten"))
long_three_combine <- long_three_combine %>% arrange(Factor)
long_three_combine$num <- c(1:nrow(long_three_combine))

long_three_combine %>% ggplot(aes(x  = Year, y = strength, color = period))+
  geom_jitter()+
  geom_smooth(method = "lm")

long_three_combine %>% ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_three_combine %>% filter(num <=75)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))



long_three_combine %>% filter(num >75 & num <= 150)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_three_combine %>% filter(num >150 & num <= 225)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_three_combine %>% filter(num >225 & num <= 300)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_three_combine %>% filter(num >300 & num <= 375)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_three_combine %>% filter(num >375 )%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))


long_thirty_combine<- thirty_combine %>% 
  inner_join(factor_ancillary, by = "Factor") %>% 
  rename(one = Strength.one, two = Strength.two, three = Strength.three) %>% 
  select(Factor, one, two, three, Year) %>% 
  pivot_longer(c(one, two, three), names_to = "period", values_to = "strength")

long_thirty_combine$period <- factor(long_thirty_combine$period, levels = c("three", "two", "one"))

long_thirty_combine %>% ggplot(aes(x  = Year, y = strength, color = period))+
  geom_point()

long_thirty_combine %>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  facet_wrap(vars(Factor))

long_thirty_combine$num <- c(1:nrow(long_thirty_combine))

long_thirty_combine<- long_thirty_combine %>% arrange(Factor)

long_thirty_combine %>% filter(num <=75)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% filter(num >75 & num <= 150)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% filter(num >150 & num <= 225)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% filter(num >225 & num <= 300)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% filter(num >300 & num <= 375)%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))

long_thirty_combine %>% filter(num >375 )%>% 
  ggplot(aes(x = period, y = strength))+
  geom_col()+
  geom_hline(yintercept = 0.5,linetype = "dashed", color = "red")+
  geom_hline(yintercept = 0.75,linetype = "dashed", color = "blue")+
  facet_wrap(vars(Factor))



 ten_strength %>% ggplot(aes(x = level))+
   geom_bar()+
   geom_text(stat='count', aes(label=..count..), vjust=-1)
 
 twenty_strength  %>% ggplot(aes(x = level))+
   geom_bar()+
   geom_text(stat='count', aes(label=..count..), vjust=-1)
 
 thirty_strength %>% ggplot(aes(x = level))+
   geom_bar()+
   geom_text(stat='count', aes(label=..count..), vjust=-1)
 
 
 ten_plot <- ten_strength %>% inner_join(factor_ancillary, by = "Factor") %>% 
   filter(strength >= 0.5) %>% 
   ggplot(aes(x = Year , y = strength)) +
   geom_point()
 
 
 twenty_plot <- twenty_strength %>% inner_join(factor_ancillary, by = "Factor") %>% 
   filter(strength >= 0.5) %>% 
   ggplot(aes(x = Year , y = strength)) +
   geom_point()
 
 thirty_plot <- thirty_strength %>% inner_join(factor_ancillary, by = "Factor") %>% 
   filter(strength >= 0.5) %>% 
   ggplot(aes(x = Year , y = strength)) +
   geom_point()
 
 thirty_strength %>% inner_join(twenty_strength, by = "Factor") %>% 
   inner_join(factor_ancillary, by = "Factor") %>% 
   filter(strength.x >= 0.5 & strength.y >= 0.5) %>% e
   select(Factor, Name, Year, strength.x, strength.y) %>% 
   rename(thirty_strength = strength.x, twenty_stregth = strength.y) %>% 
   pivot_longer(-c(Factor, Name, Year),names_to = "type", values_to = "Value" ) %>% 
   ggplot(aes(x = Year, y = Value, color = type))+
   geom_point()
 
 ggarrange(ten_plot, twenty_plot, thirty_plot)
 
 ggarrange(twenty_plot, thirty_plot)
 
 thirty_strength %>% group_by(level) %>% summarise(Proportion = n()/145) %>%
   ggplot(aes(x=level,y=Proportion))+
   geom_bar(stat="identity",position="dodge")+
   theme_minimal()+
   #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
   theme(panel.background = element_blank())+
   theme(axis.text.x = element_text(angle = 60, hjust = 1))+
   guides(fill = guide_legend(title = "Data Time Length"))+
   xlab("Strength Group")+
   ylab("Proportion")
   
 corrplot(cor(risk_factors %>% left_join(thirty_strength %>% select(Factor, strength), by = "Factor") %>% 
              arrange(strength)  %>% pivot_wider(names_from = Factor, values_from = Value) %>% select(-Date, -strength)), method = "color" )
   
 