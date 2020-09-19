library(corrr)
### All Weak factors: Strength lower than 0.5, alpha = 0.377
weak_en_result       ; weak_lasso_result       ; weak_quasi_ridge_result 
weak_en_model        ; weak_lasso_model        ; weak_quasi_ridge_model
weak_en_coef         ; weak_lasso_coef         ; weak_quasi_ridge_coef 
weak_en_matrix       ; weak_lasso_matrix       ; weak_quasi_ridge_matrix
weak_en_company_count; weak_lasso_company_count; weak_quasi_ridge_company_count 

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



strong_factors<-  thirty_strength %>% dplyr::filter(strength >=0.9& strength <1)
weak_factors<-  thirty_strength %>% dplyr::filter(strength >=0& strength <0.5)
semi_strong <- thirty_strength %>% dplyr::filter(strength >=0.7& strength <0.8)

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[strong_factors$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% summary()

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[weak_factors$Factor],
          use = 'pairwise.complete.obs',
          method = "pearson") %>% 
          shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% summary()

correlate((risk_factors %>% pivot_wider(names_from = Factor, values_from = Value))[semi_strong$Factor],
          use = "pairwise.complete.obs",
          method = "pearson") %>% 
  shave(TRUE) %>% 
  pivot_longer(-rowname, names_to = "factorII", values_to = "corr") %>% 
  na.omit() %>% 
  mutate(corr = abs(corr)) %>% summary()
