
#=============================================================================================#
#===========================#Cakculate the Factor Strength#===================================#
strength_calc <- function(return_data){
  model <- function(data){
    lm(Excess ~ Market + Value + 1, data = data)
  }
  result_table <- tibble(Factor = character(), Market_strength = double(), strength = double(), pi = double())
  nested_return <- return_data %>% 
    group_by(Factor) %>% 
    nest()
  n <- length(unique(return_data$Ticker))
  for (i in 1:length(unique(return_data$Factor))) {
    result <- nested_return[i,] %>% 
      unnest(cols = data) %>% 
      group_by(Ticker) %>% 
      nest() %>% 
      mutate(model = map(data,model)) %>% 
      mutate(summary = map(model, broom::tidy)) %>% 
      unnest(summary) %>% 
      mutate(sig_indi = if_else(abs(statistic) > qnorm(1-(0.05/(2*n^0.5))), 1, 0)) %>% 
      group_by(term) %>% 
      nest()
    result_table <- result_table %>% 
      add_row(tibble_row(Factor = as.character(nested_return[1][i,]), 
                         pi = sum((result[3,] %>% unnest(cols = c(data)))$sig_indi)/n, 
                         strength = if_else(pi == 0,0, 1 + (log(pi)/log(n))),
                         Market_strength = 1 + (log(sum((result[2,] %>% unnest(cols = c(data)))$sig_indi)/n)/log(n)))
    )
    print(paste0("Processing ", round(i/length(unique(return_data$Factor)),3),"%"))
  }
  return(result_table)
}


#=============================================================================================#
#===========================#Classify the Factor Strength#====================================#

label_strength<- function(result){
  result <- result %>% add_column(level = rep("", nrow(result)))
  for(i in 1:nrow(result)){
    if(result[i,]$strength >= 0.9){
      result[i,]$level = "above09"
    }else if(result[i,]$strength < 0.9 & result[i,]$strength >= 0.85){
      result[i,]$level = "085to09"
    }else if(result[i,]$strength < 0.85 & result[i,]$strength >= 0.8){
      result[i,]$level = "08to085"
    }else if(result[i,]$strength < 0.8 & result[i,]$strength >= 0.75){
      result[i,]$level = "075to08"
    }else if(result[i,]$strength < 0.75 & result[i,]$strength >= 0.7){
      result[i,]$level = "07to075"
    }else if(result[i,]$strength < 0.7 & result[i,]$strength >= 0.65){
      result[i,]$level = "065to07"
    }else if(result[i,]$strength < 0.65 & result[i,]$strength >= 0.6){
      result[i,]$level = "06to065"
    }else if(result[i,]$strength < 0.6 & result[i,]$strength >= 0.55){
      result[i,]$level = "055to06"
    }else if(result[i,]$strength < 0.55 & result[i,]$strength >= 0.5){
      result[i,]$level = "05to055"
    }else{
      result[i,]$level = "below05"
    }
  }
  return(result)
}

#=============================================================================================#
#===========================#Elastic Net related Functions#===================================#

alpha_determin<- function(lhs, rhs, grid=100){
  MSE_table <- tibble(alpha = double(),mse = double())
  for(i in 0:grid){
    temp_fit <- cv.glmnet(rhs, lhs, alpha= i/grid,
                          family = "gaussian",
                          type.measure = "mse")
    temp_pred<-predict(temp_fit, s=temp_fit$lambda.min, newx=rhs)
    MSE_table<-MSE_table %>% add_row(alpha = i/grid, mse =   mean((lhs - temp_pred)^2))
    print(i)
  }
  return(MSE_table)
}



auto_count_select<- function(data, alpha, strength_threshold = 0.8, folds = 10){
  data <- data %>% semi_join(thirty_strength %>% filter(strength >= strength_threshold), by = "Factor") %>% 
    pivot_wider(names_from = Factor, values_from = Value) %>% select(- Return)
  len = length(unique(data$Ticker))
  temp_nest <- data %>% group_by(Ticker) %>% nest()
  count_table<- tibble(Factor= (thirty_strength %>% filter(strength > strength_threshold))$Factor,count = 0)
  for (i in 1:len) {
    residuals_matrix<-(lm(Excess ~ Market,  temp_nest[i,] %>% 
                            unnest(cols = c(data)) %>% 
                            ungroup()) %>% augment())$.resid %>% as.matrix()
    charac_matrix <- temp_nest[i,] %>% unnest(cols = c(data)) %>% ungroup() %>% 
      select(-c(Ticker, Date, Excess, Market)) %>% 
      as.matrix()
    
    temp.fit<- cv.glmnet(charac_matrix, residuals_matrix, alpha = alpha, nfolds = folds)
    
    for(j in 1:nrow(count_table)){
      if(coef(temp.fit, s = "lambda.min")[j+1,] !=0){
        count_table[j,] <-count_table[j,] %>% mutate(count = count + 1)
      }else{}
    }
    print(paste0("processing ", round((i/len)*100,3),"%"))
    
  }
  count_table <- count_table %>% mutate(prop = count/len, quasi_strength = 1 + (log(prop)/log(len)))
  return(count_table)
}

given_count_select<- function(data, alpha, strength_threshold = 0.8, lambda){
  data <- data %>% semi_join(thirty_strength %>% filter(strength >= strength_threshold), by = "Factor") %>% 
    pivot_wider(names_from = Factor, values_from = Value) %>% select(- Return)
  len = length(unique(data$Ticker))
  temp_nest <- data %>% group_by(Ticker) %>% nest()
  count_table<- tibble(Factor= (thirty_strength %>% filter(strength > strength_threshold))$Factor,count = 0)
  for (i in 1:len) {
    residuals_matrix<-(lm(Excess ~ Market,  temp_nest[i,] %>% 
                            unnest(cols = c(data)) %>% 
                            ungroup()) %>% augment())$.resid %>% as.matrix()
    charac_matrix <- temp_nest[i,] %>% unnest(cols = c(data)) %>% ungroup() %>% 
      select(-c(Ticker, Date, Excess, Market)) %>% 
      as.matrix()
    
    temp.fit<- glmnet(charac_matrix, residuals_matrix, alpha = alpha, lambda = lambda)
    
    for(j in 1:nrow(count_table)){
      if(coef(temp.fit)[j+1,] !=0){
        count_table[j,] <-count_table[j,] %>% mutate(count = count + 1)
      }else{}
    }
    print(paste0("processing ", round((i/len)*100,3),"%"))
    
  }
  count_table <- count_table %>% mutate(prop = count/len, quasi_strength = 1 + (log(prop)/log(len)))
  return(count_table)
}



