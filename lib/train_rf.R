#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Ginny Gao
### Project 2
### ADS Spring 2018

train <- function(dat_train, label_train){
  
  ### Train a Random Forest Model (rf) using processed features from training images
  
  ### Input: 
  ###  -  processed features from images 
  ###  -  class labels for training images
  ### Output: training model specification
  
  ### load libraries
  library("randomForest")
  
  ### Train with Randome Forest model
  fit_rf <- randomForest(x = dat_train, y = label_train,
                         ntree = 500)

  return(list(fit = fit_rf))
}
