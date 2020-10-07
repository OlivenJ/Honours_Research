library(glmnet)
library(tidyverse)
library(broom)
library(caret)
library(glmnetUtils)
library(xtable)
library(imputeTS)
set.seed(123)

#================#================#================#================#================#================#
#================#================#=Fit the model with learning set==#================#================#\


select_alpha_table  <- tibble(combine = character(), alpha = double())
for(strength_crit in c(0.8, 0.9)){
  
  
#raw_learning_data <- sample_n_groups(semi_join(thirty_factor_return, thirty_strength %>% filter(strength >= 0 & strength < 0.4 + 0.1), by = "Factor") %>% 
#                  group_by(Factor),
#                size = 10) %>%
#  pivot_wider(names_from = Factor, values_from = Value)%>% 
#  select(-unique_id) %>% 
#  group_by(Ticker, Date, Return, Excess, Market) %>% 
#  summarise_each(funs(first(.[!is.na(.)]))) %>% 
#  ungroup() %>% 
#  select(-c(Date, Return))

alpha_table <- tibble(alpha = rep(0,2000))
count = 1
repeat{
#  learning_data <- sample_n_groups(raw_learning_data %>% group_by(Ticker),
#                                   size = 10) %>% select(-unique_id) %>% 
#    group_by(Ticker,Excess, Market) %>% 
#    summarise_each(funs(first(.[!is.na(.)])), ) %>% 
#    ungroup() 
  
learning_data <- semi_join(thirty_factor_return, thirty_strength , by = "Factor") %>% 
    filter(Factor %in% sample(unique(Factor),10)) %>%
   
    filter(Ticker %in% sample(unique(Ticker), 10)) %>% 
    pivot_wider(names_from = Factor, values_from = Value)%>% 
    select(-c(Date, Return))
  
smp_size <- floor(0.9 * nrow(learning_data))
train_ind <- sample(seq_len(nrow(learning_data)), size = smp_size)
train <- learning_data[train_ind, ]
test <- learning_data[-train_ind, ]

learning_feature_matrix<- train %>% 
  dplyr::select(-c(Ticker,Excess,Market)) %>% 
  as.matrix() 

learning_resid_matrix<- (lm(Excess ~ Market, train) %>% 
  augment())$.resid %>% as.matrix()
#Because the theory indicaes that we must have market factor, and other factors are almost all correlated with 
#the market factor, so we use the resudals to remove the influence of market factor
test_feature_matrix <- test%>% 
  dplyr::select(-c(Ticker,Excess,Market)) %>% 
  as.matrix() 

test_resid_matrix<- (lm(Excess ~ Market, test) %>% 
                           augment())$.resid %>% as.matrix()

#================#================#Using MSE to decide the alpha#================#==================#

mse_alpha <- alpha_determin(learning_feature_matrix, learning_resid_matrix, test_feature_data = test_feature_matrix, test_resid_data = test_resid_matrix)

alpha_table[count,]<- mse_alpha
print(count)
 if(count == nrow(alpha_table)){
  break
}
count <- count + 1
}

 #0.251 # 0.380 #0.3089 #0.3876
select_alpha_table <- select_alpha_table %>%  add_row(combine = "Random", alpha = mean(alpha_table$alpha))
}



 below_05_graph
between_05_06 <-   alpha_table %>%  ggplot(aes(x = alpha)) + geom_density()
between_07_08 <-   alpha_table %>%  ggplot(aes(x = alpha)) + geom_density()
above_09 <- alpha_table %>%  ggplot(aes(x = alpha)) + geom_density()
random_plot <-alpha_table %>%  ggplot(aes(x = alpha)) + geom_density()
#=============#=============#=============#=============#=============#=============#
learning_data <- semi_join(thirty_factor_return, thirty_strength %>% 
                             dplyr::filter(strength >=0.9& strength <1), by = "Factor") %>% 
  #dplyr::filter(Factor %in% sample(unique(Factor),10)) %>% 
  #dplyr::filter(Ticker %in% sample(unique(Ticker),30)) %>%   
  tidyr::pivot_wider(names_from = Factor, values_from = Value)%>% 
  dplyr::select(-c(Date, Return))

learning_data <-semi_join(thirty_factor_return, thirty_strength %>% dplyr::filter(strength >=0.9& strength <1), by = "Factor")  %>% 
  dplyr::filter(Factor %in% sample(unique(Factor),10)) %>% 
  bind_rows(semi_join(thirty_factor_return, thirty_strength %>% dplyr::filter(strength >=0& strength <0.5), by = "Factor") %>% 
               dplyr::filter(Factor %in% sample(unique(Factor),10))) %>% 
  tidyr::pivot_wider(names_from = Factor, values_from = Value)%>% 
  dplyr::select(-c(Date, Return))

learning_feature_matrix <- learning_data %>% 
  dplyr::select(-c(Ticker,Excess,Market)) %>% 
  as.matrix() 
learning_resid_matrix <- (lm(Excess ~ Market, learning_data) %>% 
                           augment())$.resid %>% as.matrix()

# Apply the elastic net model
en_alpha <- 0.431
en_result_mix <-  auto_count_select(learning_data, alpha = en_alpha) 
en_model_mix <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = en_alpha)
en_coef_mix <- coef(en_model_mix, s = "lambda.min") 
en_matrix_mix <-  coef_determine(learning_data, alpha = en_alpha) 

en_company_count_mix <-en_matrix_mix %>% dplyr::filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

#en_company_count_mix %>% 
#  ggplot(aes(x = prop)) +
#  geom_bar()+
#  ggtitle("Elastic Net Result")


# Apply the lasso model
lasso_result_mix <-  auto_count_select(learning_data, alpha = 1) 
lasso_model_mix <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 1)
lasso_coef_mix <- coef(mix_lasso_model_mix, s = "lambda.min") 
lasso_matrix_mix   <- coef_determine(learning_data, alpha = 1) 

lasso_company_count_mix <-lasso_matrix_mix %>% dplyr::filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

#lasso_company_count_mix %>% 
#  ggplot(aes(x = prop)) +
#  geom_bar()+
#  ggtitle("Lasso Result")


# quasi_ridge_result_mix  <-  auto_count_select(learning_data, alpha = 0.1) 
# quasi_ridge_model_mix <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 0.1)
# quasi_ridge_coef_mix <- coef(quasi_ridge_model_mix, s = "lambda.min") 
# quasi_ridge_matrix_mix   <- coef_determine(learning_data, alpha = 0.1) 
# 
# quasi_ridge_company_count_mix <-quasi_ridge_matrix_mix %>% dplyr::filter(Origin != "(Intercept)") %>% 
#   pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
#   mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
#   group_by(Ticker) %>% 
#   summarise(prop = (sum(none_zero)/mean(deno))) %>% 
#   arrange(desc(prop))
# 
# quasi_ridge_company_count_mix %>% 
#   ggplot(aes(x = prop)) +
#   geom_bar()+
#   ggtitle("Small alpha Elastic Net")
# 
# 
# 
# 
# # Apply the ridge model
# ridge_result <-  auto_count_select(learning_data, alpha = 0) 
# ridge_model <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 0)
# ridge_coef <- coef(ridge_model, s = "lambda.min") 
# ridge_matrix   <-  coef_determine(learning_data, alpha = 0) 
# 
# ridge_company_count <-ridge_matrix %>% filter(Origin != "(Intercept)") %>% 
#   pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
#   mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
#   group_by(Ticker) %>% 
#   summarise(prop = (sum(none_zero)/mean(deno))) %>% 
#   arrange(desc(prop))
# 
# ridge_company_count %>% 
#   ggplot(aes(x = prop)) +
#   geom_histogram()+
#   ggtitle("Ridge Regression")



#en_company_count %>% 
#  left_join(lasso_company_count, by = "Ticker") %>%
#  left_join(asy_ridge_company_count, by = "Ticker") %>% 
#  select(everything(),en = prop.x, lasso = prop.y, asy_ridge = prop) %>% 
#  pivot_longer(c("en", "lasso", "asy_ridge")) %>% 
#  ggplot(aes(x = value))+
#  geom_bar()+
#  facet_wrap(.~name)
#  geom_density(aes(color = name))

  
#  zero_counter(en_matrix)
#  zero_counter(lasso_matrix)
  
  
  comp_mix<- matrix_compare(en_matrix_mix, lasso_matrix_mix) %>% 
    mutate(toler = ifelse(same_call>= floor((ncol(learning_data) - 3) *0.9), 1,0),
                                                                exact = ifelse(same_call == (ncol(learning_data)-3), 1, 0))

  
  comp_mix  
  comp_0910
  comp_0809
  comp_0708
  comp_0607
  comp_0506
  
  sum(comp_mix$toler)/nrow(comp_mix)
  sum(comp_mix$exact)/nrow(comp_mix)
  
  sum(comp_0910$toler)/nrow(comp_0910)
  sum(comp_0910$exact)/nrow(comp_0910)
  
  sum(comp_0809$toler)/nrow(comp_0809)
  sum(comp_0809$exact)/nrow(comp_0809)
  
  sum(comp_0708$toler)/nrow(comp_0708)
  sum(comp_0708$exact)/nrow(comp_0708)
  
  sum(comp_0607$toler)/nrow(comp_0607)
  sum(comp_0607$exact)/nrow(comp_0607)
  
  sum(comp_0506$toler)/nrow(comp_0506)
  sum(comp_0506$exact)/nrow(comp_0506)
  
  sum(comp_0005$toler)/nrow(comp_0005)
  sum(comp_0005$exact)/nrow(comp_0005)