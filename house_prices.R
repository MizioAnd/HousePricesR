library(data.table)
library(xgboost)
library(Metrics)
library(Matrix)
library(mice)
library(dplyr)
library('ggplot2')
library('ggthemes')
library('scales')

HousePrices <- setClass(
  # Set name of class
  "HousePrices",
  
  # Define slots
  slots = c(
            df = "numeric",
            df_test = "numeric",
            df_train_test_merged = "numeric"
            ),
  
  # Set default values
  prototype=list(
    df = fread('/home/mizio/Documents/Kaggle/HousePrices/train.csv', showProgress = T),
    df_test = fread('/home/mizio/Documents/Kaggle/HousePrices/test.csv', showProgress = T),
    y_train = df$SalePrice
    # Merge training and test data together
    # df_train_test_merged = merge_train_and_test_dataframe(df, df_test)
  )
)

setGeneric(name="merge_train_and_test_dataframe",
           def=function(df, df_test)
           {
             standardGeneric("merge_train_and_test_dataframe")
           }
           )

setMethod(f="merge_train_and_test_dataframe",
          signature="HousePrices",
          definition=function(df, df_test)
          {
            # Remove Id feature and SalePrice (stored in y_train)
            df$Id <- NULL
            df$SalePrice <- NULL
            # Id_column = test_data$Id
            df_test$Id <- NULL
            return(rbind(df, df_test))
          }
          )

merge_train_and_test_dataframe=function(df, df_test)
{
  # Remove Id feature and SalePrice (stored in y_train)
  df$Id <- NULL
  df$SalePrice <- NULL
  # Id_column = test_data$Id
  df_test$Id <- NULL
  return(rbind(df, df_test))
}

setGeneric(name="drop_variable_before_preparation",
           def=function(df)
           {
             standardGeneric("drop_variable_before_preparation")
           }
           )

setMethod(f="drop_variable_before_preparation",
          signature="HousePrices",
          definition=function(df)
          {
            # Drop features that have certain procentage of missing values considering the training data and test, 
            # since they will undergo imputation together.
            print(colSums(is.na(df_train_test_merged)))
            number_of_missing_values_in_features <- colSums(is.na(df_train_test_merged))
            features_with_many_missing_values <- character(0)
            
            for(feature in features_in_train)
              {
              if(number_of_missing_values_in_features[[feature]] >= 0.3*nrow(df_train_test_merged))
                {
                features_with_many_missing_values <- append(features_with_many_missing_values, feature)
                }
              }
            print(features_with_many_missing_values)
            df_train_test_merged <- df_train_test_merged[, -features_with_many_missing_values, with=F]
            return(df_train_test_merged)
          }
          )

drop_variable_before_preparation=function(df)
{
  # Drop features that have certain procentage of missing values considering the training data and test, 
  # since they will undergo imputation together.
  print(colSums(is.na(df_train_test_merged)))
  number_of_missing_values_in_features <- colSums(is.na(df_train_test_merged))
  features_with_many_missing_values <- character(0)
  
  for(feature in features_in_train){
    if(number_of_missing_values_in_features[[feature]] >= 0.3*nrow(df_train_test_merged)){
      features_with_many_missing_values <- append(features_with_many_missing_values, feature)
    }
  }
  print(features_with_many_missing_values)
  df_train_test_merged <- df_train_test_merged[, -features_with_many_missing_values, with=F]
  return(df_train_test_merged)
}









if(interactive()){
  browser()
  
  
  
  # Create object to load data
  train_data <- fread('/home/mizio/Documents/Kaggle/HousePrices/train.csv', showProgress = T)
  test_data <- fread('/home/mizio/Documents/Kaggle/HousePrices/test.csv', showProgress = T)
  y_train <- train_data$SalePrice
  
  # Remove Id feature and SalePrice (stored in y_train)
  train_data$Id <- NULL
  train_data$SalePrice <- NULL
  # Id_column = test_data$Id
  test_data$Id <- NULL
  
  # Merge training and test data together
  train_test_merged = rbind(train_data, test_data)
  
  # Number of rows in training data for later splitting
  rows_in_train <- nrow(train_data)
  features_in_train <- names(train_data)
  
  # Encode categorical features as integers
  for(feature in features_in_train){
    if(class(train_test_merged[[feature]]) == "character"){
      levels = sort(unique(train_test_merged[[feature]]))
      train_test_merged[[feature]] = as.integer(factor(train_test_merged[[feature]], 
                                                       levels=levels))
    }
  }
  
  # Drop features that have certain procentage of missing values considering the training data and test, 
  # since they will undergo imputation together.
  print(colSums(is.na(train_test_merged)))
  number_of_missing_values_in_features <- colSums(is.na(train_test_merged))
  features_with_many_missing_values <- character(0)
  
  for(feature in features_in_train){
    if(number_of_missing_values_in_features[[feature]] >= 0.3*nrow(train_test_merged)){
      features_with_many_missing_values <- append(features_with_many_missing_values, feature)
    }
  }
  print(features_with_many_missing_values)
  train_test_merged <- train_test_merged[, -features_with_many_missing_values, with=F]
  
  # Todo: implement one-hot encoding
  # Todo: implement feature engineering to correct for skewness and apply log1p to numerical features 
  
  # Imputation with MICE
  set.seed(0)
  imputed_dataframe_train_and_test <- as.data.frame(train_test_merged)
  res <- complete(mice(imputed_dataframe_train_and_test))  # method='rf'))
  
  # Plot sale price distributions
  par(mfrow=c(1,2))
  hist(imputed_dataframe_train_and_test$LotFrontage, freq=F, main='LotFrontage: Original Data', 
       col='darkgreen')
  hist(res$LotFrontage, freq=F, main='LotFrontage: Mice Imputed Data', 
       col='lightgreen')
    
  # Splitting merged data set
  x_train <- res[1:rows_in_train,]
  x_test <- res[(rows_in_train + 1):nrow(res),]
  
  # casting all object types to numeric type
  x_train[] <- lapply(x_train, as.numeric)
  x_test[] <- lapply(x_test, as.numeric)
  
  # --- xgboost ---
  dtrain <- xgb.DMatrix(as.matrix(x_train), label=y_train)
  dtest <- xgb.DMatrix(as.matrix(x_test))
  
  # Params
  xgb_params <- list(
    seed = 0,
    colsample_bytree = 0.8,
    silent = 1,
    subsample = 0.6,
    learning_rate = 0.01,
    # eta = 0.02, 
    objective = 'reg:linear',
    max_depth = 1,
    num_parallel_tree = 1,
    # alpha = 1,
    # gamma = 2,
    min_child_weight = 1,
    eval_metric = 'rmse'
  )
  
  xgb_cv <- xgb.cv(xgb_params, dtrain, nrounds=100, nfold=10, stratified=F, early_stopping_rounds=100)
  
  best_nrounds <- xgb_cv$best_ntreelimit
  
  gbdt <- xgb.train(xgb_params, dtrain, nrounds=as.integer(best_nrounds))
  output_xgb_cv <- predict(gbdt, dtest)
  
  save_path <- '/home/mizio/Documents/Kaggle/HousePrices/submission/submission.csv'
  submission <- fread(save_path, colClasses=c("integer", "numeric"))
  submission$SalePrice <- output_xgb_cv
  # submission$Id <- Id_column
  write.csv(submission, save_path, row.names=F)

}
