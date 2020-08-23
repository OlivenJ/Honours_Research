library(glmnet)
library(tidyverse)
library(broom)

#================#================#================#================#================#================#
#================#================#================#================#================#================#
#================#================#=Fit the model with learning set==#================#================#\
#learning_return_matrix<- two_thirty_factor_return%>% pivot_wider(names_from = Factor, values_from = Value) %>% 
#  dplyr::select(Excess) %>% 
#  as.matrix()

learning_feature_matrix <- semi_join(three_thirty_factor_return, thirty_strength %>% filter(strength > 0.9), by = "Factor") %>% 
  pivot_wider(names_from = Factor, values_from = Value) %>% 
  dplyr::select(-c(Ticker, Date, Return, Excess,Market)) %>% 
  as.matrix() # we only use the factor has strength above 0.7

#learning_feature_matrix<- one_thirty_return %>% pivot_wider(names_from = Factor, values_from = Value) %>% 
#  dplyr::select(-c(Ticker, Date, Close, Return, Excess,Market)) %>% 
#  as.matrix()

resid_matrix<- (lm(Excess ~ Market, three_thirty_factor_return %>% pivot_wider(names_from = Factor, values_from = Value)) %>% 
  augment())$.resid %>% as.matrix()
#Because the theory indicaes that we must have market factor, and other factors are almost all correlated with 
#the market factor, so we use the resudals to remove the influence of market factor

#================#================#Using MSE to decide the alpha#================#==================#
 MSE_table %>% arrange(mse) %>% view()

three_thirty_alpha <- alpha_determin(resid_matrix, learning_feature_matrix, grid=100)
  
 
 
best.mse<- glmnet(learning_feature_matrix, resid_matrix, alpha= 0.09)
plot(best.mse,label= TRUE)


cv.best.mse <-cv.glmnet(learning_feature_matrix, resid_matrix, alpha= 0.09)
cv.best.mse$lambda.min
plot(cv.best.mse,label = TRUE)
coef(cv.best.mse, s = "lambda.min")

learning_lambda<- cv.best.mse$lambda.min

coef(cv.best.mse,s = "lambda.min")
coef(cv.best.mse,s = "lambda.1se")

length(best.mse$lambda)
aic = deviance(best.mse)+2*best.mse$df
bic = deviance(best.mse)+log(29160)*best.mse$df

which.min(aic)
which.min(bic)


#=============#=============#=============#=============#=============#=============#
#=============#=============#=============#=============#=============#=============#
#=============#=============#=============#=============#=============#=============#


two_thirty_a1_s0<- count_select(data = two_thirty_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
two_thirty_a05_s0<- count_select(data = two_thirty_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)
two_thirty_a05_s07<- count_select(data = two_thirty_factor_return, alpha = 0.5,strength_threshold = 0.7 ,folds = 20)

three_thirty_a1_s0<- count_select(data = three_thirty_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
three_thirty_a05_s0<- count_select(data = three_thirty_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)
three_thirty_a066_s09<- count_select(data = three_thirty_factor_return, alpha = 0.66,strength_threshold = 0.9 ,folds = 10)
three_thirty_a066_s07<- count_select(data = three_thirty_factor_return, alpha = 0.66,strength_threshold = 0.7 ,folds = 10)


one_thirty_a1_s0<- count_select(data = one_thirty_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
one_thirty_a05_s0<- count_select(data = one_thirty_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)

thirty_a1_s0<- count_select(data = thirty_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
thirty_a05_s0<- count_select(data = thirty_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)

twenty_a1_s0<- count_select(data = twenty_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
twenty_a05_s0<- count_select(data = twenty_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)

ten_a1_s0<- count_select(data = ten_factor_return, alpha = 1,strength_threshold = 0 ,folds = 20)
ten_a05_s0<- count_select(data = ten_factor_return, alpha = 0.5,strength_threshold = 0 ,folds = 20)

ten_a05_s0 %>% arrange(desc(count))
twenty_a05_s0 %>% arrange(desc(count))
thirty_a05_s0 %>% arrange(desc(count))

three_thirty_a05_s0 %>% arrange(desc(count))
three_thirty_a1_s0%>% arrange(desc(count))

two_thirty_a05_s0 %>% arrange(desc(count))
two_thirty_a1_s0 %>% arrange(desc(count))
two_thirty_a05_s07%>% arrange(desc(count))
two_thirty_a1_s0 %>% summary()
two_thirty_strength %>% arrange(desc(strength))
 
