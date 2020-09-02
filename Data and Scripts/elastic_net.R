library(glmnet)
library(tidyverse)
library(broom)
library(caret)
library(glmnetUtils)
set.seed(2020)

#================#================#================#================#================#================#
#================#================#================#================#================#================#
#================#================#=Fit the model with learning set==#================#================#\
alpha_1 <- rep(0,10)
alpha_2 <- rep(0,10)

mean(alpha_1)
mean(alpha_2)


  
learning_data <- sample_n_groups(semi_join(ten_factor_return, thirty_strength %>% filter(strength >0.9), by = "Factor") %>% 
                  group_by(Factor),
                size = 34) %>% 
  pivot_wider(names_from = Factor, values_from = Value)%>% 
  select(-unique_id) %>% 
  group_by(Ticker, Date, Return, Excess, Market) %>% 
  summarise_each(funs(first(.[!is.na(.)]))) %>% 
  ungroup() %>% select(-c(Date, Return)) 

learning_feature_matrix<- learning_data %>% 
  dplyr::select(-c(Ticker,Excess,Market)) %>% 
  as.matrix() 

learning_resid_matrix<- (lm(Excess ~ Market, learning_data) %>% 
  augment())$.resid %>% as.matrix()
#Because the theory indicaes that we must have market factor, and other factors are almost all correlated with 
#the market factor, so we use the resudals to remove the influence of market factor

#================#================#Using MSE to decide the alpha#================#==================#
fold_id <- sample(x = 1:10, size = length(learning_resid_matrix), replace = TRUE)
# search across a range of alphas
tuning_grid <- tibble::tibble(
  alpha      = seq(0, 1, by = .01),
  mse_min    = NA,
  mse_1se    = NA,
  lambda_min = NA,
  lambda_1se = NA
)
for(i in seq_along(tuning_grid$alpha)){
  # fit CV model for each alpha value
  fit <- cv.glmnet(x = learning_feature_matrix, y = learning_resid_matrix, alpha = tuning_grid$alpha[i],foldid = fold_id)
  
  # extract MSE and lambda values
  tuning_grid$mse_min[i]    <- fit$cvm[fit$lambda == fit$lambda.min]
  tuning_grid$mse_1se[i]    <- fit$cvm[fit$lambda == fit$lambda.1se]
  tuning_grid$lambda_min[i] <- fit$lambda.min
  tuning_grid$lambda_1se[i] <- fit$lambda.1se
  print(i)
}


mse_alpha <- alpha_determin(learning_feature_matrix, learning_resid_matrix)
mse_alpha %>% arrange(rmse)

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

mod_result <-  auto_count_select(learning_data, alpha = 0.05)

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


