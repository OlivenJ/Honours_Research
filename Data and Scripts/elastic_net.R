library(glmnet)
library(tidyverse)
library(broom)
library(caret)
library(glmnetUtils)
library(xtable)
library(imputeTS)
set.seed(123)


#================#================#================#================#================#================#
#================#================#================#================#================#================#
#================#================#=Fit the model with learning set==#================#================#\
learning_data <- sample_n_groups(semi_join(thirty_factor_return, thirty_strength %>% filter(strength <0.5), by = "Factor") %>% 
                  group_by(Factor),
                size = 10) %>%
  pivot_wider(names_from = Factor, values_from = Value)%>% 
  select(-unique_id) %>% 
  group_by(Ticker, Date, Return, Excess, Market) %>% 
  summarise_each(funs(first(.[!is.na(.)]))) %>% 
  ungroup() %>% 
  select(-c(Date, Return)) 

alpha_table <- tibble(alpha = rep(0,100))
count = 1
repeat{
smp_size <- floor(0.75 * nrow(learning_data))
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


alpha_table[count,]<- mse_alpha$alpha[which.min(mse_alpha$rmse)]
print(count)
 if(count == nrow(alpha_table)){
  break
}
count <- count + 1
}
alpha_table %>% view(
)
mean(alpha_table$alpha)
#=============#=============#=============#=============#=============#=============#
cv_10 = trainControl(method = "cv", number = 10)
train_elnet<- train(
  resid ~., data = learning_data,
  method = "glmnet",
  trControl = cv_10
)

best_learning <- cva.glmnet(resid ~ .,learning_data)
predict(best_learning, learning_data, whihc = 6)
best_learning$modlist
plot(best_learning$modlist[[10]])
plot(best_learning)

#=============#=============#=============#=============#=============#=============#
#=============#=============#=============#=============#=============#=============#

mod_result <-  auto_count_select(learning_data, alpha = 1)
learning_result <- cv.glmnet(learning_feature_matrix, learning_resid_matrix, alpha = 1)
mod_result %>% arrange(desc(count)) 
mod_result %>% arrange(count) 

learning_coef <- coef(learning_result, s = "lambda.min") 
test_matrix   <-  coef_determine(learning_data, alpha = 1) 



xtable(thirty_strength %>% filter(Factor %in% mod_result$Factor) %>% right_join(mod_result, by= "Factor") %>% arrange(desc(count)) %>% 
         select(Factor, prop, strength, pi),
       digits = c(0,0,3,3,3))

two_thirty_count <- given_count_select(two_thirty_factor_return,alpha = 0.54, lambda = learning_lambda, strength_threshold = 0.7 )
auto_two_thirty_count <- auto_count_select(two_thirty_factor_return,alpha = 0.54,  strength_threshold = 0.7 )
two_thirty_count %>% arrange(desc(count))
auto_two_thirty_count %>% arrange(desc(count))

three_thirty_count <- given_count_select(three_thirty_factor_return,alpha = 0.54, lambda = learning_lambda, strength_threshold = 0 )
auto_three_thirty_count <- auto_count_select(three_thirty_factor_return,alpha = 0.54,  strength_threshold = 0 )
three_thirty_count %>% arrange(desc(count))
auto_three_thirty_count %>% arrange(desc(count))


ten_count <- given_count_select(ten_factor_return,alpha = 0.54, lambda = learning_lambda, strength_threshold = 0 )
auto_ten_count <- auto_count_select(ten_factor_return,alpha = 0.54,  strength_threshold = 0 )

two_thirty_count %>% view()


