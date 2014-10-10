
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
#load interictal
interData = loadData(path,interictalFileNames)
#load preictal
preData = loadData(path,preictalFileNames)

# 3b)extract features and build dataframe for the classifier
#
wishedFeatures = c('variance')
featureFrameInter = getFeatures(interData,wishedFeatures,16)
featureFrameInter$preseizure = rep(0,length(interData))
#rm(interData)
featureFramePre = getFeatures(preData,wishedFeatures,16)
featureFramePre$preseizure = rep(1,length(preData))
#rm(preData)
#gs()

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
