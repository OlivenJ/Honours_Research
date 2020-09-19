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
learning_data <- semi_join(thirty_factor_return, thirty_strength %>% dplyr::filter(strength >=0.9& strength <1), by = "Factor") %>% 
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
en_alpha <- 0.448
mix_en_result <-  auto_count_select(learning_data, alpha = en_alpha) 
mix_en_model <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = en_alpha)
mix_en_coef <- coef(mix_en_model, s = "lambda.min") 
mix_en_matrix <-  coef_determine(learning_data, alpha = en_alpha) 

mix_en_company_count <-mix_en_matrix %>% dplyr::filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

mix_en_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("Elastic Net Result")


# Apply the lasso model
mix_lasso_result <-  auto_count_select(learning_data, alpha = 1) 
mix_lasso_model <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 1)
mix_lasso_coef <- coef(mix_lasso_model, s = "lambda.min") 
mix_lasso_matrix   <- coef_determine(learning_data, alpha = 1) 

mix_lasso_company_count <-mix_lasso_matrix %>% dplyr::filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

mix_lasso_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("Lasso Result")


mix_quasi_ridge_result  <-  auto_count_select(learning_data, alpha = 0.1) 
mix_quasi_ridge_model <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 0.1)
mix_quasi_ridge_coef <- coef(mix_quasi_ridge_model, s = "lambda.min") 
mix_quasi_ridge_matrix   <- coef_determine(learning_data, alpha = 0.1) 

mix_quasi_ridge_company_count <-mix_quasi_ridge_matrix %>% dplyr::filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

mix_quasi_ridge_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_bar()+
  ggtitle("Small alpha Elastic Net")




# Apply the ridge model
ridge_result <-  auto_count_select(learning_data, alpha = 0) 
ridge_model <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 0)
ridge_coef <- coef(ridge_model, s = "lambda.min") 
ridge_matrix   <-  coef_determine(learning_data, alpha = 0) 

ridge_company_count <-ridge_matrix %>% filter(Origin != "(Intercept)") %>% 
  pivot_longer(!Origin, names_to = "Ticker", values_to = "Coeff") %>% 
  mutate(none_zero = ifelse(Coeff == 0, 0,1), deno = length(unique(Origin))) %>% 
  group_by(Ticker) %>% 
  summarise(prop = (sum(none_zero)/mean(deno))) %>% 
  arrange(desc(prop))

ridge_company_count %>% 
  ggplot(aes(x = prop)) +
  geom_histogram()+
  ggtitle("Ridge Regression")



en_company_count %>% 
  left_join(lasso_company_count, by = "Ticker") %>%
  left_join(asy_ridge_company_count, by = "Ticker") %>% 
  select(everything(),en = prop.x, lasso = prop.y, asy_ridge = prop) %>% 
  pivot_longer(c("en", "lasso", "asy_ridge")) %>% 
  ggplot(aes(x = value))+
  geom_bar()+
  facet_wrap(.~name)
  geom_density(aes(color = name))

  
  zero_counter(en_matrix)
  zero_counter(lasso_matrix)
  
  
  #comp_0005<- matrix_compare(en_matrix, lasso_matrix) %>% mutate(toler = ifelse(same_call>= floor((ncol(learning_data) - 3) *0.9), 1,0),
   #                                                             exact = ifelse(same_call == (ncol(learning_data)-3), 1, 0))

  
  comp_mix  
  comp_0910
  comp_0809
  comp_0708
  comp_0607
  comp_0506
  sum(comp_0005$toler)/243
  sum(comp_0005$exact)/243
