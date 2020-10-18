library(reshape2)
library(tseries)
library(tsibble)
library(broom)

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

risk_factors_strength<- risk_factors %>% right_join(thirty_strength, strngth, by = "Factor") %>% 
  select(Date, Factor, Value, strength, level)



correlation <-cor(risk_factors_strength %>% group_by(Factor) %>% 
             arrange(desc(strength)) %>% 
             select(-c(strength, level)) %>% 
             pivot_wider(names_from = Factor, values_from = Value) %>% 
            select(-Date),
                     use = "pairwise.complete.obs",
                     method = "pearson") 

melt(correlation) %>% 
  ggplot( aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()+
  theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  theme(panel.background = element_blank())+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())+
 # theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
  #                                                     ends = "last")),
  #      axis.title.y = element_text(angle = 0)) +
  labs(y = "")+
#  theme(axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
  #                                                    ends = "last")))+
  labs(x = "")


risk_factors_wider <- risk_factors %>% pivot_wider(names_from = Factor, values_from = Value) %>% as_tsibble()
thirty_return_wider <- thirty_return %>% dplyr::select(-Return) %>% pivot_wider(names_from = Ticker, values_from = Excess)
twenty_return_wider <- twenty_return %>% dplyr::select(-Return) %>% pivot_wider(names_from = Ticker, values_from = Excess)
ten_return_wider <- ten_return %>% dplyr::select(-Return) %>% pivot_wider(names_from = Ticker, values_from = Excess)

stationarity_test(ten_return_wider, "KPSS")
stationarity_test(ten_return_wider, "ADF")
stationarity_test(ten_return_wider, "PP")

stationarity_test(twenty_return_wider, "KPSS")
stationarity_test(twenty_return_wider, "ADF")
stationarity_test(twenty_return_wider, "PP")

stationarity_test(thirty_return_wider, "KPSS")
stationarity_test(thirty_return_wider, "ADF")
stationarity_test(thirty_return_wider, "PP")

stationarity_test(risk_factors_wider, "PP")
stationarity_test(risk_factors_wider, "ADF")
stationarity_test(risk_factors_wider, "KPSS")

kpss_table %>% dplyr::select(Factor, p.value) %>% 
  left_join(adf_table %>% dplyr::select(Factor, p.value), by = "Factor" ) %>% inner_join(
pp_table %>% dplyr::select(Factor, p.value, method) , by = "Factor")

unstationry_factor<- risk_factors %>% inner_join( kpss_table %>% arrange(p.value) %>% filter(p.value <= 0.05) %>% dplyr::select(Factor), by = "Factor")

unstationry_factor %>% 
  ggplot(aes(x = Date, y = Value))+
  geom_line()+
  facet_wrap(.~Factor)

weak_factors

risk_factors %>% 
  #inner_join( weak_factors, by = "Factor") %>% 
  ggplot(aes(x = Date, y = Value))+
  geom_line()+
  facet_wrap(.~Factor)


test_table <- pp.test(unlist(twenty_return_wider[2])) %>% tidy() %>% add_column(unit = (twenty_return_wider %>% variable.names())[2])  
for (i in 3:ncol(twenty_return_wider)) {
  test_table<- test_table %>% add_row(pp.test(unlist(twenty_return_wider[i])) %>% tidy() %>% add_column(unit = (twenty_return_wider %>% variable.names())[i])  )
}

kpss.test(unlist(twenty_return_wider[101])) %>% tidy() %>% add_column(unit = (twenty_return_wider %>% variable.names())[101])


