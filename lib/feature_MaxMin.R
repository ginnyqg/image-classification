#############################################################
### Construct visual features for training/testing images ###
#############################################################

### Authors: Ginny Gao
### Project 2
### ADS Spring 2018

#SIFT features with max, min sum per image, one row per image


feature <- function(img_dir, set_name, data_name = "data", export = T){

train_features_dir <- paste(experiment_dir, "train-features/", sep = "")

n_files <- length(list.files(img_dir))

load(paste(train_features_dir, 'pet', 1, '.jpg.sift.Rdata', sep = ''))
dat <- cbind(t(subset(features, rowSums(features) == max(rowSums(features)))[1, ]), 
             t(subset(features, rowSums(features) == min(rowSums(features)))[1, ]))

for (i in 2 : n_files) {
    	load(paste(train_features_dir, 'pet', i, '.jpg.sift.Rdata', sep = ''))
    	dat <- rbind(dat, cbind(t(subset(features, rowSums(features) == max(rowSums(features)))[1, ]),
    	                        t(subset(features, rowSums(features) == min(rowSums(features)))[1, ])))
	}


### output constructed features
if(export){
    save(dat, file = paste0("../output/feature_", data_name, "_", set_name, ".RData"))
    }
return(dat)
}

