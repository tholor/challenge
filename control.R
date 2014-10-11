
rm(list = ls())
gc()
#1. load packages
library(R.matlab)
library(rpart)

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")

###########################
#3. preprocess data
# 3a)load data
#load interictal clips to ff variables
numFiles = 20 #how many of the available clips should be loaded
for(i in 1:numFiles){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffInter",i)
  temp = readMat(paste0(path,interictalFileNames[i]))
  assign(varName, ff(vmode = "short",dim = dim(temp[[1]][1][[1]])))
  assign(varName, temp[[1]][1][[1]])
}

#load preIctal Clips to ff variables
for(i in 1:numFiles){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffPre",i)
  temp = readMat(paste0(path,preictalFileNames[i]))
  assign(varName, ff(vmode = "short",dim = dim(temp[[1]][1][[1]])))
  assign(varName, temp[[1]][1][[1]])
}


# 3b)extract features and build dataframe for the classifier
#
wishedFeatures = c('variance')
#get the features for each interictal clip
for(i in 1: numFiles){
  ffName = paste0("ffInter",i)
  assign(paste0("featureInter", i), getFeatures(get(ffName),wishedFeatures,16))
}

#get the features for each preictal clip
for(i in 1: numFiles){
  ffName = paste0("ffPre",i)
  assign(paste0("featurePre", i), getFeatures(get(ffName),wishedFeatures,16))
}
#combine interictal features to one data.frame and add target column
featureFrameInter = featureInter1
for (i in 2: numFiles){
  featureFrameInter = rbind(featureFrameInter, get(paste0("featureInter",i)))
}
featureFrameInter$preseizure = rep(0,nrow(featureFrameInter))
#combine preictal features to one data.frame and add target column
featureFramePre = featurePre1
for (i in 2: numFiles){
  featureFramePre = rbind(featureFramePre, get(paste0("featurePre",i)))
}
featureFramePre$preseizure = rep(1,nrow(featureFramePre))
#To DO: remove unneccessary variables
gc()

# 3c) combine the data
featureFrame = rbind(featureFrameInter,featureFramePre)
#split into training and testing sample
trainRows = sample(1:nrow(featureFrame),nrow(featureFrame)*0.5)
testRows = - trainRows
trainData = featureFrame[trainRows,]
testData = featureFrame[testRows,]

#########################################
#4. train classifier
tree = rpart(preseizure ~., method="class", data=trainData)
print(tree)
summary(tree)
plot(tree)
text(tree)
# 5. evaluate classifier
#(6. predict & 7. create submission)
