######################################################
### Fit the classification model with testing data ###
######################################################

### Author: Ginny Gao
### Project 2
### ADS Spring 2018

test <- function(fit_train, dat_test){
  
  ### Fit the classfication model with testing data
  
  ### Input: 
  ###  - the fitted classification model using training data
  ###  -  processed features from testing images 
  ### Output: training model specification
  
  ### load libraries
  
  library("randomForest")
  
  pred <- predict(fit_train$fit, newdata = dat_test) 
  
  return(as.numeric(pred > 0.5))
}
