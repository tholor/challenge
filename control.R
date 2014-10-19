rm(list = ls())
gc()
#1. load packages
library(R.matlab) #matlab import
library(rpart) #trees
library(ff)  #flat files
library(pracma)
#library(propagate)
library(caret) #  classifier Framework
library(xlsx) # Excel export/import
library(doParallel) # later for parallelization of caret functionality: 

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")
source("classifier.R")


#3. preprocess data
########## Loading #################
# 3a)load data
#load interictal clips to ff variables
#numFilesInter = length(interictalFileNames) #how many of the available clips should be loaded
#numFilesPre = length(preictalFileNames)
numFilesInter = 480
numFilesPre = 24
doFFT = TRUE
target = "Dog1"

#Load Raw Time Data
#load interictal clips to ff variables
for(i in 1:numFilesInter){
  varName = paste0("ffTimeInter",i)
  #check if file has already been cached (maybe later: outsource as a function): 
  if(file.exists(paste0(pathCache,target,"\\",varName,".ffData"))){ 
    ffload(paste0(pathCache,target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target))
  }
  else{ #if not load it from .mat File and save it
  print(paste0("loaded from .mat: ", varName,, " for Target: ", target))
  temp = readMat(paste0(path,interictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",  dim = dim(temp[[1]][1][[1]])))
  t = get(varName)
  ffsave(t,list = c(varName), file = paste0(pathCache, target,"\\",varName))
  }
}
#load preIctal Clips to ff variables
for(i in 1:numFilesPre){
  varName = paste0("ffTimePre",i)
  #check if file has already been cached: 
  if(file.exists(paste0(pathCache,target,"\\",varName,".ffData"))){ 
    ffload(paste0(pathCache, target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target))
  }else{ #if not load it from .mat File and save it
  temp = readMat(paste0(path,preictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
  t = get(varName)
  ffsave(t,list = c(varName), file = paste0(pathCache, target,"\\",varName)) 
  print(paste0("loaded from .mat: ", varName, " for Target: ", target))
  }
}

#Build Frequency Data (Fourier Transformation of Time Data)
if(doFFT){
  print("### Start FFT Transformation ###")
  #Transform interictal Data
  for(i in 1:numFilesInter){
    ffName = paste0("ffTimeInter",i)
    ffFreqName = paste0("ffFreqInter",i)
    #check Cache
    if(file.exists(paste0(pathCache,target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(pathCache, target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target))
    }else{ #if not: do fft and save it to cache
    temp = getFFT(get(ffName))
    assign(ffFreqName, ff(initdata = temp, vmode = "short",  dim = dim(temp)))
    t = get(ffFreqName)
    ffsave(t,list = c(ffFreqName), file = paste0(pathCache, target,"\\",ffFreqName)) 
    print(paste0("transformed with fft: ", ffFreqName, " for Target: ", target))
    }
  }
  #Transform preictal Data
  for(i in 1:numFilesPre){
    ffName = paste0("ffTimePre",i)
    ffFreqName = paste0("ffFreqPre",i)
    #check Cache
    if(file.exists(paste0(pathCache,target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(pathCache, target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target))
    }else{ #if not: do fft and save it to cache
    temp = getFFT(get(ffName))
    assign(paste0("ffFreqPre",i), ff(initdata = temp, vmode = "short",dim = dim(temp)))
    t = get(ffFreqName)
    ffsave(t,list = c(ffFreqName), file = paste0(pathCache, target,"\\",ffFreqName)) 
    print(paste0("transformed with fft: ", ffFreqName, " for Target: ", target))
    }
  }
}

########## Features ##########
# 3b)extract features and build dataframe for the classifier
print("### Start Feature Extraction ###")
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
featureFrameInter$preseizure = as.factor(rep(0,nrow(featureFrameInter)))
#combine preictal features to one data.frame and add target column
featureFramePre = featurePre1
for (i in 2: numFilesPre){
  varName = get(paste0("featurePre",i))
  featureFramePre = rbind(featureFramePre, varName)
}
featureFramePre$preseizure = as.factor(rep(1,nrow(featureFramePre)))

#remove unused ff files (from disk) and variables (from RAM)
rm(temp)
rm(list = ls()[grepl("+ffFreq+",ls())])
rm(list = ls()[grepl("+ffTime+",ls())])
rm(list = ls()[grepl("+featurePre+",ls())])
rm(list = ls()[grepl("+featureInter+",ls())])
gc()
file.remove(list.files(getOption("fftempdir"), full.names="true"))


# 3c) combine the data
featureFrame = rbind(featureFrameInter,featureFramePre)
write.table(featureFrame, "D:\\Seizure Competition\\Data\\Cache\\Dog1\\Features\\test.txt", sep="\t")
#split into training and testing sample
trainRows = sample(1:nrow(featureFrame),nrow(featureFrame)*0.5)
testRows = - trainRows
trainData = featureFrame[trainRows,]
testData = featureFrame[testRows,]

########### Classifier ##############################
#4. train classifier
#later: cost sensitive (more important to classify the rare preseizure clips correctly)
print("### Start training the classifier ###")
#list of possible classifiers
lossMatrix = matrix(c(0,100000,1,0), nrow=2)
lossVector = as.vector(c(0.95,0.05))
names(lossVector)= c(0,1)
classifier = rpartTree(trainData, lossMatrix) #simple (unpruned) CART Tree with package rpart
rm(classifier)

#tuned classifiiers from caret package:
#set tune parameters: 3x 10folds cross validation; later: make sure that train and test data contain *different sequences* of interictal data
cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3) 
classifier = train(preseizure ~ ., data = trainData, method = "rpart", trControl = cvCtrl, parms = list(loss = lossMatrix))
classifier = train(preseizure ~ ., data = trainData, method = "C5.0")
classifier = train(preseizure ~ ., data = trainData, method = "C5.0")
classifier = train(preseizure ~ ., data = trainData, method = "svmLinear",class.weights = c("1" = 95, "0"= 5))
#classifier = train(preseizure ~ ., data = trainData, method = "nb") #naive Bayes: pretty bad!

#if tree: plot can be interesting
plot(classifier$finalModel)
text(classifier$finalModel)
# 5. evaluate classifier
#pred = predict(classifier, testData, type = "class") #for rpart
pred = predict(classifier, testData) #for all caret classifier
confusionMatrix(pred,testData$preseizure)


#ROC
# fit.pr = predict(prunedtree,newdata=data$val,type="prob")[,2]
# fit.pred = prediction(fit.pr,data$val$income)
# fit.perf = performance(fit.pred,"tpr","fpr")
# plot(fit.perf,lwd=2,col="blue",
#      main="ROC:  Classification Trees on Adult Dataset")
# abline(a=0,b=1)
#(6. predict & 7. create submission)
