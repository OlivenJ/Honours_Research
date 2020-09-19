library(glmnet)
library(tidyverse)
library(broom)
library(caret)
library(glmnetUtils)
library(xtable)
library(imputeTS)
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
    print(paste0("Processing ", round(i/length(unique(return_data$Factor)),2),"%"))
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


sample_n_groups = function(grouped_df, size, replace = FALSE, weight=NULL) {
  grp_var <- grouped_df %>% 
    groups %>%
    unlist %>% 
    as.character
  random_grp <- grouped_df %>% 
    summarise() %>% 
    sample_n(size, replace, weight) %>% 
    mutate(unique_id = 1:NROW(.))
  grouped_df %>% 
    right_join(random_grp, by=grp_var) %>% 
    group_by_(grp_var) 
}

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}



learning_feature_generate <- function(learning_data){
  smp_size <- floor(0.75 * nrow(learning_data))
  train_ind <- sample(seq_len(nrow(learning_data)), size = smp_size)
  train <- learning_data[train_ind, ]
  test <- learning_data[-train_ind, ]
  
  return( train %>% 
            dplyr::select(-c(Ticker,Excess,Market)) %>% 
            as.matrix() )
} #Prepare the learning set's feature (RHS)
learning_resid_generate <- function(learning_data){
  smp_size <- floor(0.75 * nrow(learning_data))
  train_ind <- sample(seq_len(nrow(learning_data)), size = smp_size)
  train <- learning_data[train_ind, ]
  test <- learning_data[-train_ind, ]
  
  learning_resid_matrix<- (lm(Excess ~ Market, train) %>% 
                             augment())$.resid %>% as.matrix()
}  #Prepare the learning set's resid (LHS)

error_calculation <- function(predict_value, true_value){
  return(sqrt(mean((true_value - predict_value)^2)))
} #Been used in the below function
alpha_determin<- function(learning_feature_matrix, reid_matrix, test_feature_data, test_resid_data , strength_threshold = 0){
  
#  RMSE_table <- tibble(
#    alpha = seq(0,1,0.01),
#    rmse = NA)
  fold_id <- sample(x = 1:10, size = length(reid_matrix), replace = TRUE)
  
  seq_model <- seq(0,1,0.01) %>% 
    map(function(x) cv.glmnet(learning_feature_matrix, reid_matrix, alpha = x, 
                              family = "gaussian",
                              type.measure = "mse",
                              folidid = fold_id)) %>% 
    map(function(x) predict(x, s="lambda.min", newx=test_feature_data))%>% 
    map(function(x) error_calculation(x, test_resid_data) )
  
#  for(i in seq_along(RMSE_table$alpha)){
#    temp_fit <- cv.glmnet(learning_feature_matrix, reid_matrix, alpha= RMSE_table$alpha[i],
#                          family = "gaussian",
#                          type.measure = "mse",
#                          foldid = fold_id)
#    temp_pred<-predict(temp_fit, s="lambda.min", newx=test_feature_data)
#    RMSE_table$rmse[i] <- sqrt(mean((test_resid_matrix - temp_pred)^2))
#    print(i / 101 * 100)
#  }

#   return(RMSE_table)
  return(seq(0, 1, 0.01)[which.min(seq_model)])
}



auto_count_select<- function(input_data, alpha, folds = 10){
  len = length(unique(input_data$Ticker))
  temp_nest <- input_data %>% group_by(Ticker) %>% nest()
  count_table<- tibble(Factor= variable.names(input_data)[-c(1:3)],count = 0)
  
  for (i in 1:len) {
    residuals_matrix<-(lm(Excess ~ Market,  temp_nest[i,] %>% 
                            unnest(cols = c(data)) %>% 
                            ungroup()) %>% 
                         augment())$.resid %>% as.matrix()
    
    charac_matrix <- temp_nest[i,] %>% unnest(cols = c(data)) %>% ungroup() %>% 
      dplyr::select(-c(Ticker,Excess,Market)) %>% 
      as.matrix() 
    
    temp.fit<- cv.glmnet(charac_matrix, residuals_matrix, alpha = alpha, nfolds = folds)
    
    for(j in 1:nrow(count_table)){
      if(coef(temp.fit, s = "lambda.min")[j+1,] !=0){
        count_table[j,] <-count_table[j,] %>% mutate(count = count + 1)
      }else{}
    }
    
    
    
    print(paste0("processing ", round((i/len)*100,2),"%"))
    
  }
  count_table <- count_table %>% mutate(prop = count/len, quasi_strength = 1 + (log(prop)/log(len)))
  return(count_table)
}

given_count_select<- function(data, alpha, strength_threshold = 0.8, lambda){
  data <- data %>% semi_join(thirty_strength %>% filter(strength >= strength_threshold), by = "Factor") %>% 
    pivot_wider(names_from = Factor, values_from = Value) %>% dplyr::select(- Return)
  len = length(unique(data$Ticker))
  temp_nest <- data %>% group_by(Ticker) %>% nest()
  count_table<- tibble(Factor= (thirty_strength %>% filter(strength > strength_threshold))$Factor,count = 0)
  for (i in 1:len) {
    residuals_matrix<-(lm(Excess ~ Market,  temp_nest[i,] %>% 
                            unnest(cols = c(data)) %>% 
                            ungroup()) %>% augment())$.resid %>% as.matrix()
    charac_matrix <- temp_nest[i,] %>% unnest(cols = c(data)) %>% ungroup() %>% 
      dplyr::select(-c(Ticker, Date, Excess, Market)) %>% 
      as.matrix()
    
    temp.fit<- glmnet(charac_matrix, residuals_matrix, alpha = alpha, lambda = lambda)
    
    for(j in 1:nrow(count_table)){
      if(coef(temp.fit)[j+1,] !=0){
        count_table[j,] <-count_table[j,] %>% mutate(count = count + 1)
      }else{}
    }
    print(paste0("processing ", round((i/len)*100,2),"%"))
    
  }
  count_table <- count_table %>% mutate(prop = count/len, quasi_strength = 1 + (log(prop)/log(len)))
  return(count_table)
}

coef_determine<- function(input_data, alpha, folds = 10){
  len = length(unique(input_data$Ticker))
  temp_nest <- input_data %>% group_by(Ticker) %>% nest()
  start_matrix <- as(matrix(nrow = ncol(input_data) -2 , ncol = 1, data = 0), "dgTMatrix")
  
  
  for (i in 1:len) {
    residuals_matrix<-(lm(Excess ~ Market,  temp_nest[i,] %>% 
                            unnest(cols = c(data)) %>% 
                            ungroup()) %>% 
                         augment())$.resid %>% as.matrix()
    
    charac_matrix <- temp_nest[i,] %>% unnest(cols = c(data)) %>% ungroup() %>% 
      dplyr::select(-c(Ticker,Excess,Market)) %>% 
      as.matrix() 
    
    temp.fit<- cv.glmnet(charac_matrix, residuals_matrix, alpha = alpha, nfolds = folds)
    
    add_matrix <-  coef(temp.fit, s = "lambda.min") 
    colnames(add_matrix) <- temp_nest$Ticker[i]
    
    start_matrix <-  (cbind(start_matrix,add_matrix))
  
    print(paste0("processing ", round((i/len)*100,2),"%"))
    
  }
  summ<-summary(start_matrix)
  
  
  return(data.frame(Origin      = rownames(start_matrix)[summ$i],
                    Destination = colnames(start_matrix)[summ$j],
                    Weight      = summ$x) %>% as_tibble() %>% 
           pivot_wider(names_from = Destination, values_from = Weight) %>% na_replace(0)
         )
}

matrix_compare <- function(matrix_one, matrix_two){
  #comp_one <- matrix_one %>% dplyr::filter(Origin != "(Intercept)") %>% arrange(Origin)
  #comp_two <- matrix_two %>% dplyr::filter(Origin != "(Intercept)") %>% arrange(Origin)
  
  #row_num <-  dim(comp_one)[1]
  #col_num <- dim(comp_one)[2]
  #cell_num <- row_num * (col_num -1)
  
  #equals <- 0
  #for(i in 1:row_num){
  #  for(j in 2:col_num) {
  #    if(as.numeric(comp_one[i, j]) == 0 & as.numeric(comp_two[i, j]) == 0){
  #      equals <- equals + 1
  #    }
  #    else if(as.numeric(comp_one[i, j]) != 0 & as.numeric(comp_two[i, j]) != 0){
  #      equals <- equals + 1
  #    }
  #  }
  #}
  
  #return(equals)
  
  comp_one <- matrix_one %>% dplyr::filter(Origin != "(Intercept)") %>% arrange(Origin)%>%
    pivot_longer(-Origin) %>% 
    pivot_wider(names_from=Origin  , values_from=value) 
  
  comp_two <- matrix_two %>% dplyr::filter(Origin != "(Intercept)") %>% arrange(Origin)%>%
    pivot_longer(-Origin) %>% 
    pivot_wider(names_from=Origin  , values_from=value) 
  
  row_num <-  dim(comp_one)[1]
  col_num <- dim(comp_one)[2]
  cell_num <- row_num * (col_num -1)
  
  call_table <-tibble(companies = comp_one$name, same_call = NULL) 
  
  for(i in 1:row_num){
    col_equals <-0
    for(j in 2:col_num) {
      if(as.numeric(comp_one[i, j]) == 0 & as.numeric(comp_two[i, j]) == 0){
        col_equals <- col_equals + 1
      }
      else if(as.numeric(comp_one[i, j]) != 0 & as.numeric(comp_two[i, j]) != 0){
        col_equals <- col_equals + 1
      }
      
    }
    call_table[i,2] <- col_equals
  }
  
  return(call_table %>% dplyr::select(companies, same_call = ...2) )
  
}

zero_counter<- function(matrix){
  pos_matrix <- matrix %>% dplyr::filter(Origin != "(Intercept)") %>% arrange(Origin)
  row_num <- dim(pos_matrix)[1]
  col_num <- dim(pos_matrix)[2]
  cell_num <- row_num * col_num
  
  count <- 0
  for(i in 1:row_num){
    for(j in 2:col_num){
      if(as.numeric(pos_matrix[i,j]) == 0){
        count <- count + 1
      }
    }
  }
  
  return(count)
}