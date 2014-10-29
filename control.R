rm(list = ls())
gc()
#1. load packages
library(R.matlab) #matlab import
library(rpart) #trees
library(ff)  #flat files
library(pracma) # Functions from numerical analysis and linear algebra, numerical optimization, differential equations, plus some special functions. Uses Matlab function names where appropriate to simplify porting.
#library(propagate)
library(caret) #  classifier Framework
#library(xlsx) # Excel export/import
library(doParallel) # later for parallelization of caret functionality: 
library(beepr) #beep sound to notify end of a run

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")
source("classifier.R")
source("submission.R")


#3. preprocess data
########## Loading #################
# 3a)load data
#load interictal clips to ff variables
#numFilesInter = length(interictalFileNames) #how many of the available clips should be loaded
#numFilesPre = length(preictalFileNames)
numFilesInter = 480
numFilesPre = 24
numFilesTest = 502
doFFT = TRUE
makePredictions = TRUE
target = "Dog1"


#Get Filenames
a = getFilenames(path)
interictalFileNames = a[[1]]
preictalFileNames = a[[2]]
testFileNames = a[[3]]

#Load Raw Time Data
#load interictal clips to ff variables
for(i in 1:numFilesInter){
  varName = paste0("ffTimeInter",i)
  #check if file has already been cached (maybe later: outsource as a function): 
  if(file.exists(paste0(pathCache,target,"\\",varName,".ffData"))){ 
    ffload(paste0(pathCache,target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target)) # REMOVE debug
  }
  else{ #if not load it from .mat File and save it
  print(paste0("loaded from .mat: ", varName, " for Target: ", target)) # REMOVE debug
  temp = readMat(paste0(path,interictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",  dim = dim(temp[[1]][1][[1]])))
  t = get(varName) # TODO simpler way?
  ffsave(t,list = c(varName), file = paste0(pathCache, target,"\\",varName))
  }
}
#load preIctal Clips to ff variables
for(i in 1:numFilesPre){
  varName = paste0("ffTimePre",i)
  #check if file has already been cached: 
  if(file.exists(paste0(pathCache,target,"\\",varName,".ffData"))){ 
    ffload(paste0(pathCache, target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target)) # REMOVE debug
  }else{ #if not load it from .mat File and save it
  temp = readMat(paste0(path,preictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
  t = get(varName) # TODO simpler way?
  ffsave(t,list = c(varName), file = paste0(pathCache, target,"\\",varName)) 
  print(paste0("loaded from .mat: ", varName, " for Target: ", target)) # REMOVE debug
  }
}
#Extract Sequencenumber for each clip (relevant for splitting test/train sets later)
if(file.exists(paste0(pathCache,target,"\\sequences.txt"))){
  seqOfClips = read.table(paste0(pathCache,target,"\\sequences.txt"), sep="\t")
}else{
allFileNames = c(interictalFileNames, preictalFileNames)
seqOfClips = getSequences(allFileNames)
row.names(seqOfClips) = allFileNames
#save them (loading is extreme expensive!)
write.table(seqOfClips, paste0(pathCache,target,"\\sequences.txt"), sep="\t")
}

#Build Frequency Data (Fourier Transformation of Time Data)
#note for later: patient data has different sample frequency!
if(doFFT){
  print("### Start FFT Transformation ###") # REMOVE debug
  #Transform interictal Data
  for(i in 1:numFilesInter){
    ffName = paste0("ffTimeInter",i)
    ffFreqName = paste0("ffFreqInter",i)
    #check Cache
    if(file.exists(paste0(pathCache,target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(pathCache, target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target)) # REMOVE debug
    }else{ #if not: do fft and save it to cache
    temp = getFFT(get(ffName))
    assign(ffFreqName, ff(initdata = temp, vmode = "integer",  dim = dim(temp)))
    t = get(ffFreqName)
    ffsave(t,list = c(ffFreqName), file = paste0(pathCache, target,"\\",ffFreqName)) 
    print(paste0("transformed with fft: ", ffFreqName, " for Target: ", target)) # REMOVE debug
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
    assign(paste0("ffFreqPre",i), ff(initdata = temp, vmode = "integer",dim = dim(temp)))
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
featureFrameInter$preseizure = as.factor(rep("No",nrow(featureFrameInter)))
row.names(featureFrameInter) = interictalFileNames
#combine preictal features to one data.frame and add target column
featureFramePre = featurePre1
for (i in 2: numFilesPre){
  varName = get(paste0("featurePre",i))
  featureFramePre = rbind(featureFramePre, varName)
}
featureFramePre$preseizure = as.factor(rep("Yes",nrow(featureFramePre)))
row.names(featureFramePre) = preictalFileNames
#remove unused ff files (from disk) and variables (from RAM)
rm(temp)
rm(list = ls()[grepl("+ffFreq+",ls())])
rm(list = ls()[grepl("+ffTime+",ls())])
rm(list = ls()[grepl("+featurePre+",ls())])
rm(list = ls()[grepl("+featureInter+",ls())])
gc()
file.remove(list.files(getOption("fftempdir"), full.names="true"))


# 3c) combine the data
#temporary: saving the featureFrame
featureFrame = rbind(featureFrameInter,featureFramePre)
write.table(featureFrame, paste0(pathCache,target,"\\Features\\test_neu.txt"), sep="\t")
beep()
#(shortcut: loading the current "standard feature frame")
featureFrame = read.table(paste0(pathCache,target,"\\Features\\test_neu.txt"), header=TRUE, sep="\t")
featureFrame$preseizure = as.factor(featureFrame$preseizure)
featureFrameInter = featureFrame[featureFrame$preseizure == "No",] 
featureFramePre = featureFrame[featureFrame$preseizure == "Yes",]

#split into training and testing sample, keeping the sequences in intact (6 in a row)
splittedFrame = splitToTestTrain(featureFrameInter,featureFramePre, seqOfClips, 0.75)
trainData = splittedFrame[[1]]
testData = splittedFrame[[2]]


########### Classifier ##############################
#4. train classifier
print("### Start training the classifier ###")
#list of possible classifiers
lossMatrix = matrix(c(0,100,1,0), nrow=2)
classifier = rpartTree(trainData, lossMatrix) #simple (unpruned) CART Tree with package rpart
rm(classifier)

#tuned classifiiers from caret package:
#set tune parameters: 3x 10folds cross validation; later: make sure that train and test data contain *different sequences* of data
cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
#cvCtrlClass = trainControl(method = "repeatedcv",number = 5, repeats = 3)

  #treebased classifier
  classifierRpart =           train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "rpart",   parms = list(loss = lossMatrix))#AUC: 0.46
  classifierc5 =              train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "C5.0", costs = lossMatrix)
  classifierRandomForest =    train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "rf")#0.6
  #support vector machines
  classifierSVMlin =          train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "svmLinear", scaled = FALSE)
  classifierSVMexp =          train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "svmRadial", scaled = FALSE)#0.51

#Combine the results of the different classifiers and print summary
performanceResults <- resamples(list(rPart = classifierRpart,
                                    svmLin = classifierSVMlin,
                                    svmexp = classifierSVMexp,
                                    C5 = classifierc5))
summary(performanceResults)

#visualize performance of different train methods with box-and-whisker-plot :
bwplot(performanceResults, layout = c(3, 1))

# 5. evaluate a single classifier
curClassifier = classifierRandomForest
#pred = predict(classifier, featureFrame, type = "class") #for rpart
pred = predict(curClassifier, testData) #should work for all caret classifiers
confusionMatrix(pred,testData$preseizure, positive = "Yes")
#AUC Calculation (Area under the ROC Curve)
#Sanity Check! Seems to be super high
predRoc = predict(curClassifier, testData, type = "prob")
myroc = pROC::roc(testData$preseizure, as.vector(predRoc[,2]))
plot(myroc, print.thres = "best")
auc(myroc)


#adjust optimal cut-off threshold for class probabilities
predAdj = predict(curClassifier, testData, type = "prob")
threshold = coords(myroc,x="best",best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
predCut = factor( ifelse(predAdj[, "Yes"] > threshold, "Yes", "No") )
#predCut = relevel(predCut, "yes")   #try that, if error occurs
confusionMatrix(predCut, testData$preseizure)

########## Prediction & Submission ########## 
#6. Make Predictions for test clips
if(makePredictions) source("preprocessTestclips.R")
#7. Create Submission File in Data/Submission
#per Target:
#createTargetSubmission(curClassifier, featureFrame, c(interictalFileNames, preictalFileNames), target)
createTargetSubmission(curClassifier, featureFrameTest, testFileNames, target)

#combine them all 
createFullSubmission()

#### OTHER STUFF / HELPER / ETC. ####
#old ROC Plot
# fit.pr = predict(prunedtree,newdata=data$val,type="prob")[,2]
# fit.pred = prediction(fit.pr,data$val$income)
# fit.perf = performance(fit.pred,"tpr","fpr")
# plot(fit.perf,lwd=2,col="blue",
#      main="ROC:  Classification Trees on Adult Dataset")
# abline(a=0,b=1)
#(6. predict & 7. create submission)


#Naive model which predicts "No" all the time
# predNaive= rep(1,504)
# myroc = pROC::roc(featureFrame$preseizure, predNaive,levels = c("Yes","No"),plot=TRUE)


#if tree: plot can be interesting
# plot(classifierRpart$finalModel)
# text(classifierRpart$finalModel)
