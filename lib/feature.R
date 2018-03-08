#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Ginny Gao
### Project 2
### ADS Spring 2018

#SIFT + HoG features

feature <- function(img_dir, set_name, data_name = "data", export = T){

#import normalize vector library
library(ppls)

# import libraries for HoG
library(reticulate)
cv2 <- reticulate::import('cv2')
library("EBImage")

# create a HOG object
winSize <- tuple(64L, 64L)
blockSize <- tuple(16L, 16L)
blockStride <- tuple(8L, 8L)
cellSize <- tuple(8L, 8L)
nbins = 9L

hog = cv2$HOGDescriptor(winSize, blockSize, blockStride, cellSize, nbins)

######################### CHANGE THIS VARIABLE #########################
#set the starting pet image number in test_features_dir

test_start = 1801

########################################################################


train_features_dir <- paste(experiment_dir, "train-features/", sep = "")

test_features_dir <- paste(experiment_dir, "test-features/", sep = "")

if (img_dir == img_train_dir) {

n_files <- length(list.files(img_dir))

load(paste(train_features_dir, 'pet', 1, '.jpg.sift.Rdata', sep = ''))

set.seed(1234)
bof <- kmeans(features, 40, iter.max = 20)

dat1 <- cbind(normalize.vector(t(bof$centers[1, ])), 
             normalize.vector(t(bof$centers[2, ])),
             normalize.vector(t(bof$centers[3, ])),
             normalize.vector(t(bof$centers[4, ])),
             normalize.vector(t(bof$centers[5, ])),
             normalize.vector(t(bof$centers[6, ])),
             normalize.vector(t(bof$centers[7, ])),
             normalize.vector(t(bof$centers[8, ])),
             normalize.vector(t(bof$centers[9, ])),
             normalize.vector(t(bof$centers[10, ])),
             normalize.vector(t(bof$centers[11, ])),
             normalize.vector(t(bof$centers[12, ])),
             normalize.vector(t(bof$centers[13, ])),
             normalize.vector(t(bof$centers[14, ])),
             normalize.vector(t(bof$centers[15, ])),
             normalize.vector(t(bof$centers[16, ])),
             normalize.vector(t(bof$centers[17, ])),
             normalize.vector(t(bof$centers[18, ])),
             normalize.vector(t(bof$centers[19, ])),
             normalize.vector(t(bof$centers[20, ])),
             normalize.vector(t(bof$centers[21, ])),
             normalize.vector(t(bof$centers[22, ])),
             normalize.vector(t(bof$centers[23, ])),
             normalize.vector(t(bof$centers[24, ])),
             normalize.vector(t(bof$centers[25, ])),
             normalize.vector(t(bof$centers[26, ])),
             normalize.vector(t(bof$centers[27, ])),
             normalize.vector(t(bof$centers[28, ])),
             normalize.vector(t(bof$centers[29, ])),
             normalize.vector(t(bof$centers[30, ])),
             normalize.vector(t(bof$centers[31, ])),
             normalize.vector(t(bof$centers[32, ])),
             normalize.vector(t(bof$centers[33, ])),
             normalize.vector(t(bof$centers[34, ])),
             normalize.vector(t(bof$centers[35, ])),
             normalize.vector(t(bof$centers[36, ])),
             normalize.vector(t(bof$centers[37, ])),
             normalize.vector(t(bof$centers[38, ])),
             normalize.vector(t(bof$centers[39, ])),
             normalize.vector(t(bof$centers[40, ]))
            )

for (i in 2 : n_files) {
    	load(paste(train_features_dir, 'pet', i, '.jpg.sift.Rdata', sep = ''))
        set.seed(1234)
        bof <- kmeans(features, 40, iter.max = 20)
    	dat1 <- rbind(dat1, cbind(normalize.vector(t(bof$centers[1, ])), 
                                normalize.vector(t(bof$centers[2, ])),
                                normalize.vector(t(bof$centers[3, ])),
                                normalize.vector(t(bof$centers[4, ])),
                                normalize.vector(t(bof$centers[5, ])),
                                normalize.vector(t(bof$centers[6, ])),
                                normalize.vector(t(bof$centers[7, ])),
                                normalize.vector(t(bof$centers[8, ])),
                                normalize.vector(t(bof$centers[9, ])),
                                normalize.vector(t(bof$centers[10, ])),
                                normalize.vector(t(bof$centers[11, ])),
                                normalize.vector(t(bof$centers[12, ])),
                                normalize.vector(t(bof$centers[13, ])),
                                normalize.vector(t(bof$centers[14, ])),
                                normalize.vector(t(bof$centers[15, ])),
                                normalize.vector(t(bof$centers[16, ])),
                                normalize.vector(t(bof$centers[17, ])),
                                normalize.vector(t(bof$centers[18, ])),
                                normalize.vector(t(bof$centers[19, ])),
                                normalize.vector(t(bof$centers[20, ])),
                                normalize.vector(t(bof$centers[21, ])),
                                normalize.vector(t(bof$centers[22, ])),
                                normalize.vector(t(bof$centers[23, ])),
                                normalize.vector(t(bof$centers[24, ])),
                                normalize.vector(t(bof$centers[25, ])),
                                normalize.vector(t(bof$centers[26, ])),
                                normalize.vector(t(bof$centers[27, ])),
                                normalize.vector(t(bof$centers[28, ])),
                                normalize.vector(t(bof$centers[29, ])),
                                normalize.vector(t(bof$centers[30, ])),
                                normalize.vector(t(bof$centers[31, ])),
                                normalize.vector(t(bof$centers[32, ])),
                                normalize.vector(t(bof$centers[33, ])),
                                normalize.vector(t(bof$centers[34, ])),
                                normalize.vector(t(bof$centers[35, ])),
                                normalize.vector(t(bof$centers[36, ])),
                                normalize.vector(t(bof$centers[37, ])),
                                normalize.vector(t(bof$centers[38, ])),
                                normalize.vector(t(bof$centers[39, ])),
                                normalize.vector(t(bof$centers[40, ]))
    	                       ))
	}


dat2 <- matrix(NA, n_files, 1764)
for(i in 1 : n_files){
img     <- cv2$imread(paste0(img_dir, data_name, i, ".jpg")) / 255
img_resized <- cv2$resize(img, dsize=tuple(64L, 64L))
hog_values <- hog$compute(np_array(img_resized * 255, dtype='uint8'))
dat2[i, ] <- t(hog_values)
}

dat <- cbind(dat1, dat2)

}

#when img_dir == test_features_dir

else {

n_files <- length(list.files(img_dir))

load(paste(test_features_dir, 'pet', test_start, '.jpg.sift.Rdata', sep = ''))

set.seed(1234)
bof <- kmeans(features, 40, iter.max = 20)

dat1 <- cbind(normalize.vector(t(bof$centers[1, ])), 
             normalize.vector(t(bof$centers[2, ])),
             normalize.vector(t(bof$centers[3, ])),
             normalize.vector(t(bof$centers[4, ])),
             normalize.vector(t(bof$centers[5, ])),
             normalize.vector(t(bof$centers[6, ])),
             normalize.vector(t(bof$centers[7, ])),
             normalize.vector(t(bof$centers[8, ])),
             normalize.vector(t(bof$centers[9, ])),
             normalize.vector(t(bof$centers[10, ])),
             normalize.vector(t(bof$centers[11, ])),
             normalize.vector(t(bof$centers[12, ])),
             normalize.vector(t(bof$centers[13, ])),
             normalize.vector(t(bof$centers[14, ])),
             normalize.vector(t(bof$centers[15, ])),
             normalize.vector(t(bof$centers[16, ])),
             normalize.vector(t(bof$centers[17, ])),
             normalize.vector(t(bof$centers[18, ])),
             normalize.vector(t(bof$centers[19, ])),
             normalize.vector(t(bof$centers[20, ])),
             normalize.vector(t(bof$centers[21, ])),
             normalize.vector(t(bof$centers[22, ])),
             normalize.vector(t(bof$centers[23, ])),
             normalize.vector(t(bof$centers[24, ])),
             normalize.vector(t(bof$centers[25, ])),
             normalize.vector(t(bof$centers[26, ])),
             normalize.vector(t(bof$centers[27, ])),
             normalize.vector(t(bof$centers[28, ])),
             normalize.vector(t(bof$centers[29, ])),
             normalize.vector(t(bof$centers[30, ])),
             normalize.vector(t(bof$centers[31, ])),
             normalize.vector(t(bof$centers[32, ])),
             normalize.vector(t(bof$centers[33, ])),
             normalize.vector(t(bof$centers[34, ])),
             normalize.vector(t(bof$centers[35, ])),
             normalize.vector(t(bof$centers[36, ])),
             normalize.vector(t(bof$centers[37, ])),
             normalize.vector(t(bof$centers[38, ])),
             normalize.vector(t(bof$centers[39, ])),
             normalize.vector(t(bof$centers[40, ]))
             )

for (i in (test_start + 1) : (test_start - 1 + n_files)) {
        load(paste(test_features_dir, 'pet', i, '.jpg.sift.Rdata', sep = ''))
        set.seed(1234)
        bof <- kmeans(features, 40, iter.max = 20)
        dat1 <- rbind(dat1, cbind(normalize.vector(t(bof$centers[1, ])), 
                                normalize.vector(t(bof$centers[2, ])),
                                normalize.vector(t(bof$centers[3, ])),
                                normalize.vector(t(bof$centers[4, ])),
                                normalize.vector(t(bof$centers[5, ])),
                                normalize.vector(t(bof$centers[6, ])),
                                normalize.vector(t(bof$centers[7, ])),
                                normalize.vector(t(bof$centers[8, ])),
                                normalize.vector(t(bof$centers[9, ])),
                                normalize.vector(t(bof$centers[10, ])),
                                normalize.vector(t(bof$centers[11, ])),
                                normalize.vector(t(bof$centers[12, ])),
                                normalize.vector(t(bof$centers[13, ])),
                                normalize.vector(t(bof$centers[14, ])),
                                normalize.vector(t(bof$centers[15, ])),
                                normalize.vector(t(bof$centers[16, ])),
                                normalize.vector(t(bof$centers[17, ])),
                                normalize.vector(t(bof$centers[18, ])),
                                normalize.vector(t(bof$centers[19, ])),
                                normalize.vector(t(bof$centers[20, ])),
                                normalize.vector(t(bof$centers[21, ])),
                                normalize.vector(t(bof$centers[22, ])),
                                normalize.vector(t(bof$centers[23, ])),
                                normalize.vector(t(bof$centers[24, ])),
                                normalize.vector(t(bof$centers[25, ])),
                                normalize.vector(t(bof$centers[26, ])),
                                normalize.vector(t(bof$centers[27, ])),
                                normalize.vector(t(bof$centers[28, ])),
                                normalize.vector(t(bof$centers[29, ])),
                                normalize.vector(t(bof$centers[30, ])),
                                normalize.vector(t(bof$centers[31, ])),
                                normalize.vector(t(bof$centers[32, ])),
                                normalize.vector(t(bof$centers[33, ])),
                                normalize.vector(t(bof$centers[34, ])),
                                normalize.vector(t(bof$centers[35, ])),
                                normalize.vector(t(bof$centers[36, ])),
                                normalize.vector(t(bof$centers[37, ])),
                                normalize.vector(t(bof$centers[38, ])),
                                normalize.vector(t(bof$centers[39, ])),
                                normalize.vector(t(bof$centers[40, ]))                               
                                ))
    }

dat2 <- matrix(NA, n_files, 1764)
for(i in test_start : (test_start - 1 + n_files)){
img     <- cv2$imread(paste0(img_dir, data_name, i, ".jpg")) / 255
img_resized <- cv2$resize(img, dsize=tuple(64L, 64L))
hog_values <- hog$compute(np_array(img_resized * 255, dtype='uint8'))
dat2[i - (test_start - 1), ] <- t(hog_values)
}

dat <- cbind(dat1, dat2)

}


### output constructed features
if(export){
    save(dat, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
    }
return(dat)
}

