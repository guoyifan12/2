#####All variable model results#####
library(h2o)
h2o.init()
set.seed(434)

data <- read.csv("data.csv", fileEncoding = "UTF-8")

# Load the data as the H2OFrame object
data <- as.h2o(data)

# Specify the features and target columns
y <- "TP"
x <- setdiff(names(data), y)

# Initialize the result matrix
n_iterations <- 10
results <- matrix(data = NA, nrow = n_iterations, ncol = 4, 
                  dimnames = list(NULL, c("Train-MSE", "Train_R-Squared", "Test-MSE", "Test_R-Squared")))
pred_list <- vector("list", n_iterations)

# Initialize a list to store all the predicted results and actual values of the test set
all_test_results <- list()

# Cyclic training and evaluation models
for (i in 1:n_iterations) {
  # Randomly divide the data set into the training set and the test set
  splits <- h2o.splitFrame(data, ratios = 0.7, seed = 434+i)
  train <- splits[[1]]
  test <- splits[[2]]
  
  # Ensure that the dimensions of the training set and the test set are correct
  if (dim(train)[1] > 0 && dim(test)[1] > 0) {
    model <- h2o.randomForest(x = x,y = y,training_frame = train, seed = 434+i)
    
    # Make predictions on the training set
    train_predictions <- h2o.predict(model, newdata = train)
    
    # Make predictions on the test set
    test_predictions <- h2o.predict(model, newdata = test)
    
    # 保存测试集的预测结果
    pred_list[[i]] <- as.data.frame(test_predictions)
    
    # Convert the prediction results and the training set/test set into an R data frame
    train_predictions_df <- as.data.frame(train_predictions)
    test_predictions_df <- as.data.frame(test_predictions)
    train_df <- as.data.frame(train)
    test_df <- as.data.frame(test)
    
    train_mse <- mean((train_predictions_df$predict - train_df$TP)^2)
    train_rsquare <- 1 - (train_mse / var(train_df$TP))
    
    test_mse <- mean((test_predictions_df$predict - test_df$TP)^2)
    test_rsquare <- 1 - (test_mse / var(test_df$TP))
    
    results[i, "Train-MSE"] <- train_mse
    results[i, "Train_R-Squared"] <- train_rsquare
    results[i, "Test-MSE"] <- test_mse
    results[i, "Test_R-Squared"] <- test_rsquare
    
    
  }
}

library(writexl)

results_df <- as.data.frame(results)

write_xlsx(results_df, "results.xlsx")

#####Sort the importance of explanatory variables #####
#init
data <- read.csv("data.csv")

# Load the data as the H2OFrame object
data<- as.h2o(data)

# Specify the features and target columns
y <- "TP"
x <- setdiff(names(data), y)

# Initialize the result matrix
n_iterations <- 10
results <- matrix(data = NA, nrow = n_iterations, ncol = 2, 
                  dimnames = list(NULL, c("Train-MSE", "Train_R-Squared")))
pred_list <- vector("list", n_iterations)

# Cyclic training and evaluation models
for (i in 1:n_iterations) {
  # Train the Model
  model <- h2o.randomForest(x = x, y = y, training_frame = data)
  
  
}

# Obtain the variable importance of the model
var_importance <- h2o.varimp(model)

write.csv(var_importance,"result1.csv")


#####Top10—RFModel prediction effect#####

set.seed(40)
data <- read.csv("data.csv", fileEncoding = "UTF-8")  

data <- as.h2o(data)

y <- "TP"
x <- setdiff(names(data), y)

# Initialize the result matrix
n_iterations <- 10
results <- matrix(data = NA, nrow = n_iterations, ncol = 4, 
                  dimnames = list(NULL, c("Train-MSE", "Train_R-Squared", "Test-MSE", "Test_R-Squared")))
pred_list <- vector("list", n_iterations)

# Initialize a list to store all the predicted results and actual values of the test set
all_test_results <- list()

# Cyclic training and evaluation model
for (i in 1:n_iterations) {
  # Randomly divide the dataset into a training set and a test set
  splits <- h2o.splitFrame(data, ratios = 0.7, seed = 40+i)
  train <- splits[[1]]
  test <- splits[[2]]
  
  # Ensure that the dimensions of the training set and the test set are correct
  if (dim(train)[1] > 0 && dim(test)[1] > 0) {
    model <- h2o.randomForest( 
      x = c("data", "data", "data", "data", "data", "data", "data", "data", "data", "data"),
      y = "TP",
      training_frame = train,
      seed = 40 + i 
    )
    
    # Make predictions on the training set
    train_predictions <- h2o.predict(model, newdata = train)
    
    # Make predictions on the test set
    test_predictions <- h2o.predict(model, newdata = test)
    
    # Save the prediction results of the test set
    pred_list[[i]] <- as.data.frame(test_predictions)
    
    # Convert the prediction results and the training set/test set into an R data frame
    train_predictions_df <- as.data.frame(train_predictions)
    test_predictions_df <- as.data.frame(test_predictions)
    train_df <- as.data.frame(train)
    test_df <- as.data.frame(test)
    
    train_mse <- mean((train_predictions_df$predict - train_df$TP)^2)
    train_rsquare <- 1 - (train_mse / var(train_df$TP))
    
    test_mse <- mean((test_predictions_df$predict - test_df$TP)^2)
    test_rsquare <- 1 - (test_mse / var(test_df$TP))
    
    results[i, "Train-MSE"] <- train_mse
    results[i, "Train_R-Squared"] <- train_rsquare
    results[i, "Test-MSE"] <- test_mse
    results[i, "Test_R-Squared"] <- test_rsquare
    
  }
}
library(writexl)

results_df <- as.data.frame(results)

write_xlsx(results_df, "Top10-results.xlsx")