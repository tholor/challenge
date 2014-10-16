
rm(list = ls())
gc()
#1. load packages
library(R.matlab)
library(rpart)
library(ff)
library(pracma)
library(propagate)

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")


#3. preprocess data
########## Loading #################
# 3a)load data
#load interictal clips to ff variables
#numFilesInter = length(interictalFileNames) #how many of the available clips should be loaded
#numFilesPre = length(preictalFileNames)
numFilesInter = 3
numFilesPre = 3
doFFT = TRUE

#Load Raw Time Data
#load interictal clips to ff variables
for(i in 1:numFilesInter){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffTimeInter",i)
  temp = readMat(paste0(path,interictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",  dim = dim(temp[[1]][1][[1]])))
}
#load preIctal Clips to ff variables
for(i in 1:numFilesPre){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffTimePre",i)
  temp = readMat(paste0(path,preictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
}

#Build Frequency Data (Fourier Transformation of Time Data)
if(doFFT){
  #Transform interictal Data
  for(i in 1:numFilesInter){
    ffName = paste0("ffTimeInter",i)
    temp = getFFT(get(ffName))
    assign(paste0("ffFreqInter",i), ff(initdata = temp, vmode = "short",  dim = dim(temp)))
  }
  #Transform preictal Data
  for(i in 1:numFilesPre){
    ffName = paste0("ffTimePre",i)
    temp = getFFT(get(ffName))
    assign(paste0("ffFreqPre",i), ff(initdata = temp, vmode = "short",dim = dim(temp)))
  }

}

########## Features ##########
# 3b)extract features and build dataframe for the classifier
#
source("preprocessFunctions.R")

wishedFeatures = c('variance','correlation')
#get the features for each interictal clip
for(i in 1: numFilesInter){
  ffName = paste0("ffTimeInter",i)
  ffFreqName = paste0("ffFreqInter",i)
  #only temporary version until Tom'S restructuring (deleting the "wishedFeatures" structure)
  timeFeatures=getFeatures(get(ffName),wishedFeatures,16)
  freqFeatures = freqCorrelation(get(ffFreqName)) 
  assign(paste0("featureInter", i), cbind(timeFeatures,freqFeatures))
}

#get the features for each preictal clip
for(i in 1: numFilesPre){
  ffName = paste0("ffTimePre",i)
  ffFreqName = paste0("ffFreqPre",i)
  #only temporary version until Tom'S restructuring (deleting the "wishedFeatures" structure)
  timeFeatures=getFeatures(get(ffName),wishedFeatures,16)
  freqFeatures = freqCorrelation(get(ffFreqName)) 
  assign(paste0("featurePre", i), cbind(timeFeatures,freqFeatures))
}
#combine interictal features to one data.frame, remove single clip files and add target column
featureFrameInter = featureInter1
for (i in 2: numFilesInter){
  varName = get(paste0("featureInter",i))
  featureFrameInter = rbind(featureFrameInter, varName)
}
featureFrameInter$preseizure = rep(0,nrow(featureFrameInter))
#combine preictal features to one data.frame and add target column
featureFramePre = featurePre1
for (i in 2: numFilesPre){
  varName = get(paste0("featurePre",i))
  featureFramePre = rbind(featureFramePre, varName)
}
featureFramePre$preseizure = rep(1,nrow(featureFramePre))
#remove unused ff files and variables
rm(list = ls()[grepl("+ffFreq+",ls())])
rm(list = ls()[grepl("+ffTime+",ls())])
rm(list = ls()[grepl("+featurePre+",ls())])
rm(list = ls()[grepl("+featureInter+",ls())])
gc()
file.remove(list.files(getOption("fftempdir"), full.names="true"))



# 3c) combine the data
featureFrame = rbind(featureFrameInter,featureFramePre)
#split into training and testing sample
trainRows = sample(1:nrow(featureFrame),nrow(featureFrame)*0.5)
testRows = - trainRows
trainData = featureFrame[trainRows,]
testData = featureFrame[testRows,]

########### Classifier ##############################
#4. train classifier
tree = rpart(preseizure ~., method="class", data=trainData)
print(tree)
summary(tree)
plot(tree)
text(tree)
# 5. evaluate classifier
#(6. predict & 7. create submission)
