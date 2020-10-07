library(corrr)


### All Weak factors: Strength lower than 0.5, alpha = 0.377
en_result_0005       ; lasso_result_0005       
en_model_0005        ; lasso_model_0005        
en_coef_0005         ; lasso_coef_0005         
en_matrix_0005       ; lasso_matrix_0005       
en_company_count_0005; lasso_company_count_0005

mean(model_factor_count(en_matrix_0005)$count)
mean(model_factor_count(en_matrix_0005)$prop)
mean(model_factor_count(lasso_matrix_0005)$count)
mean(model_factor_count(lasso_matrix_0005)$prop)



en_result_0005 %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()


en_company_count_0005 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak Elastic Net Result")
lasso_company_count_0005 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak Lasso Result")
quasi_ridge_company_count_0005%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak ridge-like EN Result")

en_company_count_0005 %>% 
  left_join(lasso_company_count_0005, by = "Ticker") %>%
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("weak factor comparison")+
  #geom_bar()+
  #facet_wrap(.~name)
  geom_density(aes(color = name))

matrix_compare(en_matrix_0005, lasso_matrix_0005)

comp_0005
sum(comp_0005$toler)/nrow(comp_0005)
sum(comp_0005$exact)/nrow(comp_0005)

### All semi strong factors: Strength between 0.7 and 0.8 alpha = 0.411
en_result_0607    ; lasso_result_0607
en_model_0607     ; lasso_model_0607
en_coef_0607      ; lasso_coef_0607   
en_matrix_0607       ; lasso_matrix_0607       
en_company_count_0607; lasso_company_count_0607

mean(model_factor_count(en_matrix_0607)$count)
mean(model_factor_count(en_matrix_0607)$prop)
mean(model_factor_count(lasso_matrix_0607)$count)
mean(model_factor_count(lasso_matrix_0607)$prop)

en_company_count_0607 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong Elastic Net Result")
lasso_company_count_0607 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong Lasso Result")
quasi_ridge_company_count_0607%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong ridge-like EN Result")

semi_en_company_count %>% 
  left_join(semi_lasso_company_count, by = "Ticker") %>%
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("semi-strong factor comparison")+
  geom_bar()+
  facet_wrap(.~name)
geom_density(aes(color = name))

comp_0708
sum(comp_0708$toler)/nrow(comp_0708)
sum(comp_0708$exact)/nrow(comp_0708)


### All strong factors: Strength higher than 0.9,  alpha = 0.413
en_result_0910; lasso_result_0910
en_model_0910; lasso_model_0910
en_coef_0910; lasso_coef_0910
en_company_count_0910; lasso_company_count_0910

mean(model_factor_count(en_matrix_0910)$count)
mean(model_factor_count(en_matrix_0910)$prop)
mean(model_factor_count(lasso_matrix_0910)$count)
mean(model_factor_count(lasso_matrix_0910)$prop)

(en_matrix_0910 %>%
  pivot_longer(cols = -Origin, names_to = "Ticker", values_to = "Loadings") %>% 
  filter(Origin != "(Intercept)") %>% 
  mutate(none_zero = ifelse(Loadings == 0, 0, 1)) %>% 
  group_by(Ticker) %>% 
  summarise(select_fact = mean(none_zero)))$select_fact %>% mean()

(lasso_matrix_0910 %>%
  pivot_longer(cols = -Origin, names_to = "Ticker", values_to = "Loadings") %>% 
  filter(Origin != "(Intercept)") %>% 
  mutate(none_zero = ifelse(Loadings == 0, 0, 1)) %>% 
  group_by(Ticker) %>% 
  summarise(select_fact = mean(none_zero)) )$select_fact %>% mean()


en_company_count_0910 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong Elastic Net Result")
lasso_company_count_0910 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong Lasso Result")
quasi_ridge_company_count_0910%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong ridge-like EN Result")

en_company_count_0910 %>% 
  left_join(strong_lasso_company_count, by = "Ticker") %>%
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("strong factor comparison")+
#  geom_bar()+
#  facet_wrap(.~name)
geom_density(aes(color = name))

comp_0910
sum(comp_0910$toler)/nrow(comp_0910)
sum(comp_0910$exact)/nrow(comp_0910)

### mix factors: half facotr Strength higher than 0.9, half lower than 0.5 alpha = 0.448
en_result_mix; lasso_result_mix
en_model_mix; lasso_model_mix
en_coef_mix; lasso_coef_mix
en_matrix_mix; lasso_matrix_mix
en_company_count_mix; lasso_company_count_mix
mean(model_factor_count(mix_en_matrix)$count)

mean(model_factor_count(en_matrix_mix)$count)
mean(model_factor_count(en_matrix_mix)$prop)
mean(model_factor_count(lasso_matrix_mix)$count)
mean(model_factor_count(lasso_matrix_mix)$prop)

en_company_count_mix %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Elastic Net Result")
lasso_company_count_mix %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Lasso Result")
quasi_ridge_company_count_mix%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed ridge-like EN Result")

en_company_count_mix %>% 
  left_join(mix_lasso_company_count, by = "Ticker") %>%
  left_join(mix_quasi_ridge_company_count, by = "Ticker") %>% 
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("mixed factor comparison")+
  geom_bar()+
  facet_wrap(.~name)
geom_density(aes(color = name))

sum(comp_mix$toler)/nrow(comp_mix)
sum(comp_mix$exact)/nrow(comp_mix)



en_result_0506       ; lasso_result_0506       
en_model_0506        ; lasso_model_0506        
en_coef_0506         ; lasso_coef_0506         
en_matrix_0506       ; lasso_matrix_0506       
en_company_count_0506; lasso_company_count_0506

mean(model_factor_count(en_matrix_0809)$count)
mean(model_factor_count(en_matrix_0809)$prop)
mean(model_factor_count(lasso_matrix_0809)$count)
mean(model_factor_count(lasso_matrix_0809)$prop)

en_company_count_0506 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Elastic Net Result")
lasso_company_count_0506 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Lasso Result")
quasi_ridge_company_count_0506%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed ridge-like EN Result")

en_company_count_0506 %>% 
  left_join(lasso_company_count_0506, by = "Ticker") %>%
  left_join(quasi_ridge_company_count_0506, by = "Ticker") %>% 
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("mixed factor comparison")+
  geom_bar()+
  facet_wrap(.~name)
geom_density(aes(color = name))

sum(comp_0506$toler)/nrow(comp_0506)
sum(comp_0506$exact)/nrow(comp_0506)




en_result_0607       ; lasso_result_0607       
en_model_0607        ; lasso_model_0607        
en_coef_0607         ; lasso_coef_0607         
en_matrix_0607       ; lasso_matrix_0607       
en_company_count_0607; lasso_company_count_0607

mean(model_factor_count(en_matrix_0708)$count)
mean(model_factor_count(en_matrix_0708)$prop)
mean(model_factor_count(lasso_matrix_0708)$count)
mean(model_factor_count(lasso_matrix_0708)$prop)

en_company_count_0607 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Elastic Net Result")
lasso_company_count_0607 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Lasso Result")
quasi_ridge_company_count_0607%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed ridge-like EN Result")

en_company_count_0607 %>% 
  left_join(lasso_company_count_0607, by = "Ticker") %>%
  left_join(quasi_ridge_company_count_0607, by = "Ticker") %>% 
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("mixed factor comparison")+
  geom_bar()+
  facet_wrap(.~name)
geom_density(aes(color = name))

sum(comp_0607$toler)/nrow(comp_0607)
sum(comp_0607$exact)/nrow(comp_0607)




en_result_0809       ; lasso_result_0809       
en_model_0809        ; lasso_model_0809        
en_coef_0809         ; lasso_coef_0809         
en_matrix_0809       ; lasso_matrix_0809       
en_company_count_0809; lasso_company_count_0809

en_company_count_0809 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Elastic Net Result")
lasso_company_count_0809 %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Lasso Result")
quasi_ridge_company_count_0809%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed ridge-like EN Result")

en_company_count_0809 %>% 
  left_join(lasso_company_count_0809, by = "Ticker") %>%
  left_join(quasi_ridge_company_count_0809, by = "Ticker") %>% 
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("mixed factor comparison")+
  geom_bar()+
  facet_wrap(.~name)
geom_density(aes(color = name))

sum(comp_0809$toler)/nrow(comp_0809)
sum(comp_0809$exact)/nrow(comp_0809)

#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====
#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====
#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====#=====


(risk_factors %>% filter(Factor == "pchsale_pchrect")) %>% 
  ggplot(aes(x = Date, y = Value))+
  geom_line()

strong_factors<-  thirty_strength %>% dplyr::filter(strength >=0.9& strength <1)
weak_factors<-  thirty_strength %>% dplyr::filter(strength >=0& strength <0.5)
semi_strong <- thirty_strength %>% dplyr::filter(strength >=0.7& strength <0.8)
strengt_0506 <- thirty_strength %>% dplyr::filter(strength >=0.5& strength <0.6)
strengt_0607 <- thirty_strength %>% dplyr::filter(strength >=0.6& strength <0.7)
strengt_0809 <- thirty_strength %>% dplyr::filter(strength >=0.8& strength <0.9)

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[weak_factors$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% 
  summarise(mean(corr))


correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[strong_factors$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% 
  summarise(mean(corr))

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[weak_factors$Factor],
          use = 'pairwise.complete.obs',
          method = "pearson") %>% 
          shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
   mutate(corr = abs(corr)) %>% 
  summarise(mean(corr))

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[semi_strong$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>%
  summarise(mean(corr))

cor((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[strong_factors$Factor]) %>% 
  ggcorrplot(hc.order = TRUE, outline.col = "white", type = "lower", lab = TRUE)

cor((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[weak_factors$Factor]) %>% 
  ggcorrplot(hc.order = TRUE, outline.col = "white", type = "lower", lab  = TRUE)

cor((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[semi_strong$Factor]) %>% 
  ggcorrplot(hc.order = TRUE, outline.col = "white", type = "lower", lab = TRUE)




#=================#=================#=================#=================#=================#=================
correlation_storage <- rep(0,2000)
timer <- 1
repeat{
correlation_data <-thirty_strength %>% dplyr::filter(strength >=0.9& strength <1)  %>% 
  dplyr::filter(Factor %in% sample(unique(Factor),5)) %>% 
  bind_rows(thirty_strength %>% dplyr::filter(strength >=0& strength <0.5) %>% 
               dplyr::filter(Factor %in% sample(unique(Factor),5)))

correlation_storage[timer] <- as.numeric(correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[correlation_data$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% 
  summarise(mean(corr))
)

timer <- timer + 1
print(timer)
if (timer > 2000) {
  break
}
}


weak_en_result %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()
semi_en_result %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()
strong_en_result %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()
en_result_0506 %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()
en_result_0607 %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()
en_result_0809 %>% ggplot(aes(y = prop,x = reorder(Factor, -prop)))+
  geom_col() 


long_en_result <- weak_en_result %>% mutate(group = "0005") %>% 
  bind_rows(en_result_0506 %>% mutate(group = "0506"),
            en_result_0607 %>% mutate(group = "0607"),
            semi_en_result %>% mutate(group = "0708"),
            en_result_0809 %>% mutate(group = "0809"),
            strong_en_result %>% mutate(group = "0910"))

long_en_result$group<- as_factor(long_en_result$group  )

long_en_result %>% 
  #filter(prop >= 0.2)%>% 
  ggplot(aes(x  = reorder(Factor, -prop), y = prop))+
  geom_col(aes( fill = group))+
  theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  theme(panel.background = element_blank())+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  #scale_fill_brewer()+
  facet_wrap(.~group)+
  xlab("Factor")+
  ylab("Select Proportion")

long_lasso_result <- weak_lasso_result %>% mutate(group = "0005") %>% 
  bind_rows(lasso_result_0506 %>% mutate(group = "0506"), 
            lasso_result_0607 %>% mutate(group = "0607"),
            semi_lasso_result %>% mutate(group = "0708"),
            lasso_result_0809 %>% mutate(group = "0809"),
            strong_lasso_result %>% mutate(group = "0910"))

long_lasso_result$group<- as_factor(long_lasso_result$group)

long_lasso_result %>% 
  #filter(prop >= 0.2)%>% 
  ggplot(aes(x  = reorder(Factor, -prop), y = prop))+
  geom_col(aes( fill = group))+
  theme_bw()+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  theme(panel.background = element_blank())+
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())+
  #scale_fill_brewer()+
  facet_wrap(.~group)+
  xlab("Factor")+
  ylab("Select Proportion")
 
head(strong_en_result, 10)
head(weak_en_result, 10)
head(semi_en_result, 10)
head()
