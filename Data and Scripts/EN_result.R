library(corrr)


### All Weak factors: Strength lower than 0.5, alpha = 0.377
weak_en_result       ; weak_lasso_result       ; weak_quasi_ridge_result 
weak_en_model        ; weak_lasso_model        ; weak_quasi_ridge_model
weak_en_coef         ; weak_lasso_coef         ; weak_quasi_ridge_coef 
weak_en_matrix       ; weak_lasso_matrix       ; weak_quasi_ridge_matrix
weak_en_company_count; weak_lasso_company_count; weak_quasi_ridge_company_count 

mean(model_factor_count(weak_en_matrix)$prop)

weak_en_result %>% ggplot(aes(y = prop,x = Factor))+
  geom_col()


weak_en_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak Elastic Net Result")
weak_lasso_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak Lasso Result")
weak_quasi_ridge_company_count%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("weak ridge-like EN Result")

weak_en_company_count %>% 
  left_join(weak_lasso_company_count, by = "Ticker") %>%
  left_join(weak_quasi_ridge_company_count, by = "Ticker") %>% 
  dplyr::select(everything(),en = prop.x, lasso = prop.y, quasi_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "quasi_ridge")) %>% 
  ggplot(aes(x = value))+
  ggtitle("weak factor comparison")+
  #geom_bar()+
  #facet_wrap(.~name)
  geom_density(aes(color = name))

matrix_compare(weak_en_matrix, weak_lasso_matrix)

comp_0005
sum(comp_0005$toler)/nrow(comp_0005)
sum(comp_0005$exact)/nrow(comp_0005)

### All semi strong factors: Strength between 0.7 and 0.8 alpha = 0.411
semi_en_result       ; semi_lasso_result       ; semi_quasi_ridge_result
semi_en_model        ; semi_lasso_model        ; semi_quasi_ridge_model
semi_en_coef         ; semi_lasso_coef         ; semi_quasi_ridge_coef
semi_en_matrix       ; semi_lasso_matrix       ; semi_quasi_ridge_matrix
semi_en_company_count; semi_lasso_company_count; semi_quasi_ridge_company_count

semi_en_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong Elastic Net Result")
semi_lasso_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong Lasso Result")
semi_quasi_ridge_company_count%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("semi strong ridge-like EN Result")

semi_en_company_count %>% 
  left_join(semi_lasso_company_count, by = "Ticker") %>%
  left_join(semi_quasi_ridge_company_count, by = "Ticker") %>% 
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
strong_en_result       ; strong_lasso_result       ; strong_quasi_ridge_result
strong_en_model        ; strong_lasso_model        ; strong_quasi_ridge_model
strong_en_coef         ; strong_lasso_coef         ; strong_quasi_ridge_coef
strong_en_matrix       ; strong_lasso_matrix       ; strong_quasi_ridge_matrix
strong_en_company_count; strong_lasso_company_count; strong_quasi_ridge_company_count

strong_en_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong Elastic Net Result")
strong_lasso_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong Lasso Result")
strong_quasi_ridge_company_count%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("strong ridge-like EN Result")

strong_en_company_count %>% 
  left_join(strong_lasso_company_count, by = "Ticker") %>%
  left_join(strong_quasi_ridge_company_count, by = "Ticker") %>% 
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
mix_en_result       ; mix_lasso_result       ; mix_quasi_ridge_result
mix_en_model        ; mix_lasso_model        ; mix_quasi_ridge_model
mix_en_coef         ; mix_lasso_coef         ; mix_quasi_ridge_coef
mix_en_matrix       ; mix_lasso_matrix       ; mix_quasi_ridge_matrix
mix_en_company_count; mix_lasso_company_count; mix_quasi_ridge_company_count

mean(model_factor_count(mix_en_matrix)$count)

mix_en_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Elastic Net Result")
mix_lasso_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed Lasso Result")
mix_quasi_ridge_company_count%>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("mixed ridge-like EN Result")

mix_en_company_count %>% 
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



en_result_0506       ; lasso_result_0506       ; quasi_ridge_result_0506
en_model_0506        ; lasso_model_0506        ; quasi_ridge_model_0506
en_coef_0506         ; lasso_coef_0506         ; quasi_ridge_coef_0506
en_matrix_0506       ; lasso_matrix_0506       ; quasi_ridge_matrix_0506
en_company_count_0506; lasso_company_count_0506; quasi_ridge_company_count_0506

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




en_result_0607       ; lasso_result_0607       ; quasi_ridge_result_0607
en_model_0607        ; lasso_model_0607        ; quasi_ridge_model_0607
en_coef_0607         ; lasso_coef_0607         ; quasi_ridge_coef_0607
en_matrix_0607       ; lasso_matrix_0607       ; quasi_ridge_matrix_0607
en_company_count_0607; lasso_company_count_0607; quasi_ridge_company_count_0607

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




en_result_0809       ; lasso_result_0809       ; quasi_ridge_result_0809
en_model_0809        ; lasso_model_0809        ; quasi_ridge_model_0809
en_coef_0809         ; lasso_coef_0809         ; quasi_ridge_coef_0809
en_matrix_0809       ; lasso_matrix_0809       ; quasi_ridge_matrix_0809
en_company_count_0809; lasso_company_count_0809; quasi_ridge_company_count_0809

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
