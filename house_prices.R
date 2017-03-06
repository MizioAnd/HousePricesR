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
                        df = "data.frame",
                        df_test = "data.frame"
                        # df_train_test_merged = "numeric"
                        ),
              
              # Set default values
              prototype=list(
                    df = fread('/home/mizio/Documents/Kaggle/HousePrices/train.csv', showProgress = T),
                    df_test = fread('/home/mizio/Documents/Kaggle/HousePrices/test.csv', showProgress = T)
                    # y_train = df$SalePrice
                    
                    # Merge training and test data together
                    # df_train_test_merged = merge_train_and_test_dataframe(df, df_test)
                    )
              )

setGeneric(name="merge_train_and_test_dataframe",
                                           def=function(theObject, df, df_test)
                                           {
                                                    standardGeneric("merge_train_and_test_dataframe")
                                           }
                                           )

setMethod(f="merge_train_and_test_dataframe",
                                          signature="HousePrices",
                                          definition=function(theObject, df, df_test)
                                          {
                                                    # Remove Id feature and SalePrice (stored in y_train)
                                                    df$Id <- NULL
                                                    df$SalePrice <- NULL
                                                    # Id_column = test_data$Id
                                                    df_test$Id <- NULL
                                                    return(rbind(df, df_test))
                                          }
                                          )

setGeneric(name="drop_variable_before_preparation",
                                         def=function(theObject, df)
                                         {
                                                    standardGeneric("drop_variable_before_preparation")
                                         }
                                         )

setMethod(f="drop_variable_before_preparation",
                                        signature="HousePrices",
                                        definition=function(theObject, df)
                                        {
                                                    # Drop features that have certain procentage of missing values considering the training data and test, 
                                                    # since they will undergo imputation together.
                                                    # print(colSums(is.na(df)))
                                                    number_of_missing_values_in_features <- colSums(is.na(df))
                                                    features_with_many_missing_values <- character(0)
                                                    features_in_df <- names(df)
                                                    for(feature in features_in_df)
                                                      {
                                                      if(number_of_missing_values_in_features[[feature]] >= 0.3*nrow(df))
                                                        {
                                                        features_with_many_missing_values <- append(features_with_many_missing_values, feature)
                                                        }
                                                      }
                                                    print(features_with_many_missing_values)
                                                    df <- df[, -features_with_many_missing_values, with=F]
                                                    return(df)
                                        }
                                        )

setGeneric(name="clean_data",
                       def=function(theObject, df)
                       {
                                  standardGeneric("clean_data")
                       }
                       )

setMethod(f="clean_data",
                      signature="HousePrices",
                      definition=function(theObject, df)
                      {
                                  # Imputation with MICE
                                  set.seed(0)
                                  df <- as.data.frame(df)
                                  df_imputed <- complete(mice(df))  # method='rf'))
                                  return(df_imputed)
                      }
                      )


if(interactive())
  {
  # browser()
  
  # Create instance of class
  house_prices <- HousePrices()
  is.object(house_prices)
  isS4(house_prices)
  
  
  
  # Create object to load data
  df <- slot(house_prices, "df") 
  df_test <- slot(house_prices, "df_test")
  y_train <- df$SalePrice

  # Merge training and test data together
  train_test_merged <- merge_train_and_test_dataframe(house_prices, df, df_test)
  features_in_train_test_merged <- names(train_test_merged)
  
  # Number of rows in training data for later splitting
  rows_in_train <- nrow(df)
  features_in_train <- names(df)

  
  # Encode categorical features as integers
  for(feature in features_in_train_test_merged)
    {
    if(class(train_test_merged[[feature]]) == "character")
      {
      levels = sort(unique(train_test_merged[[feature]]))
      train_test_merged[[feature]] = as.integer(factor(train_test_merged[[feature]], 
                                                       levels=levels))
      }
    }
  
  # Drop features that have certain procentage of missing values considering the training data and test, 
  # since they will undergo imputation together.
  print(colSums(is.na(train_test_merged)))
  train_test_merged <- drop_variable_before_preparation(house_prices, train_test_merged)
  
  # Todo: implement one-hot encoding
  # Todo: implement feature engineering to correct for skewness and apply log1p to numerical features 
  
  # Imputation with MICE
  res <- clean_data(house_prices, train_test_merged)

  # Check how distributions change after imputation
  # Plot LotFrontage price distributions
  par(mfrow=c(1,2))
  hist(train_test_merged$LotFrontage, freq=F, main='LotFrontage: Original Data', 
       col='darkgreen')
  hist(res$LotFrontage, freq=F, main='LotFrontage: Mice Imputed Data', 
       col='lightgreen')
  
  # Plot GarageYrBlt price distributions
  par(mfrow=c(1,2))
  hist(train_test_merged$GarageYrBlt, freq=F, main='GarageYrBlt: Original Data', 
       col='darkgreen')
  hist(res$GarageYrBlt, freq=F, main='GarageYrBlt: Mice Imputed Data', 
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
