#########################################################
### Train a classification model with training images ###
#########################################################

### Author: Yuting Ma with modification by Ginny Gao
### Project 2
### ADS Spring 2018

train <- function(dat_train, label_train){
    
  ### load libraries
  library("e1071")
  
  fit_svm <- svm(x = dat_train, y = label_train)

  return(list(fit = fit_svm))
}
