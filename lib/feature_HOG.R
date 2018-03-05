#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Ginny Gao
### Project 2
### ADS Spring 2018

feature <- function(img_dir, set_name, data_name = "data", export=T){
  
  ### Construct process features for training/testing images
  ### Sample simple feature: Extract row average raw pixel values as features
  
  ### Input: a directory that contains images ready for processing
  ### Output: an .RData file contains processed features for the images
  
  ### load libraries
  library(reticulate)
  cv2 <- reticulate::import('cv2')
  library("EBImage")
  
  ### create a HOG object
  winSize <- tuple(64L, 64L)
  blockSize <- tuple(16L, 16L)
  blockStride <- tuple(8L, 8L)
  cellSize <- tuple(8L, 8L)
  nbins = 9L

  hog = cv2$HOGDescriptor(winSize, blockSize, blockStride, cellSize, nbins)

  ######################### CHANGE THIS VARIABLE #########################
  #set the starting pet image number in test_features_dir

  test_start = 1601

  ########################################################################

  
  if (img_dir == img_train_dir) {

  n_files <- length(list.files(img_dir))
  
  # ### determine img dimensions
  # img0 <-  cv2$imread(paste0(img_dir, data_name, 1, ".jpg")) / 255
  # mat1 <- as.matrix(img0)
  # ### resize image and produce hog features of the image in a vector format
  # img_resized <- cv2$resize(img0, dsize=tuple(64L, 64L))
  # hog_values <- hog$compute(np_array(img_resized * 255, dtype='uint8'))

  ### store vectorized pixel values of images
  dat <- matrix(NA, n_files, 1764)
  for(i in 1 : n_files){
    img     <- cv2$imread(paste0(img_dir, data_name, i, ".jpg")) / 255
    img_resized <- cv2$resize(img, dsize=tuple(64L, 64L))
    hog_values <- hog$compute(np_array(img_resized * 255, dtype='uint8'))
    dat[i, ] <- t(hog_values)
  }
  }
  
  else {
  
  n_files <- length(list.files(img_dir))
  
  ### store vectorized pixel values of images
  dat <- matrix(NA, n_files, 1764)
  for(i in test_start : (test_start - 1 + n_files)) {
    img     <- cv2$imread(paste0(img_dir, data_name, i, ".jpg")) / 255
    img_resized <- cv2$resize(img, dsize=tuple(64L, 64L))
    hog_values <- hog$compute(np_array(img_resized * 255, dtype='uint8'))
    dat[i - (test_start - 1), ] <- t(hog_values)
  }
  }

  
  ### output constructed features
  if(export){
    save(dat, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
  }
  return(dat)
}