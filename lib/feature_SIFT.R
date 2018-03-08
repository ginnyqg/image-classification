#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Ginny Gao
### Project 2
### ADS Spring 2018

#SIFT feature


feature <- function(img_dir, set_name, data_name = "data", export = T){

#import normalize vector library
library(ppls)

######################### CHANGE THIS VARIABLE #########################
#set the starting pet image number in test_features_dir

test_start = 1601

########################################################################


train_features_dir <- paste(experiment_dir, "train-features/", sep = "")

test_features_dir <- paste(experiment_dir, "test-features/", sep = "")

if (img_dir == img_train_dir) {

n_files <- length(list.files(img_dir))

load(paste(train_features_dir, 'pet', 1, '.jpg.sift.Rdata', sep = ''))

set.seed(1234)
bof <- kmeans(features, 20, iter.max = 20)

dat <- cbind(normalize.vector(t(bof$centers[1, ])), 
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
             normalize.vector(t(bof$centers[20, ]))
            )

for (i in 2 : n_files) {
    	load(paste(train_features_dir, 'pet', i, '.jpg.sift.Rdata', sep = ''))
        set.seed(1234)
        bof <- kmeans(features, 20, iter.max = 20)
    	dat <- rbind(dat, cbind(normalize.vector(t(bof$centers[1, ])), 
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
                                normalize.vector(t(bof$centers[20, ]))
    	                       ))
	}
}

else {

n_files <- length(list.files(img_dir))

load(paste(test_features_dir, 'pet', test_start, '.jpg.sift.Rdata', sep = ''))

set.seed(1234)
bof <- kmeans(features, 20, iter.max = 20)

dat <- cbind(normalize.vector(t(bof$centers[1, ])), 
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
             normalize.vector(t(bof$centers[20, ]))
             )

for (i in (test_start + 1) : (test_start - 1 + n_files)) {
        load(paste(test_features_dir, 'pet', i, '.jpg.sift.Rdata', sep = ''))
        set.seed(1234)
        bof <- kmeans(features, 20, iter.max = 20)
        dat <- rbind(dat, cbind(normalize.vector(t(bof$centers[1, ])), 
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
                                normalize.vector(t(bof$centers[20, ]))                                
                                ))
    }
}


### output constructed features
if(export){
    save(dat, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
    }
return(dat)
}

