targets = c("Dog1","Dog2","Dog3","Dog4","Dog5")
for(curTarget in 1:length(targets)){
  
rm(list = ls()[!ls() %in% c("targets","curTarget")])
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
library(matrixStats)

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")
source("classifier.R")
source("submission.R")


#3. preprocess data
########## Loading #################
# 3a)load data
target = targets[curTarget]
a = getFilenames(path,target)
interictalFileNames = a[[1]]
preictalFileNames = a[[2]]
testFileNames = a[[3]]
#load interictal clips to ff variables
#numFilesInter = length(interictalFileNames) #how many of the available clips should be loaded
#numFilesPre = length(preictalFileNames)
numFilesInter = length(interictalFileNames)# 480#500 #480
numFilesPre = length(preictalFileNames)#42 #24
numFilesTest = length(testFileNames)#1000 #502
doFFT = TRUE
if(target %in% c("Patient1","Patient2")){
  freq = 5000
}else {
    freq =  399.6098  
}
freqFrom = 1
freqTo = 47
fftMethod = "sum"
makePredictions = TRUE



#Get Filenames
# a = getFilenames(path,target)
# interictalFileNames = a[[1]][1:numFilesInter]
# preictalFileNames = a[[2]][1:numFilesPre]
# testFileNames = a[[3]][1:numFilesTest]

#Load Raw Time Data
#load interictal clips to ff variables
for(i in 1:numFilesInter){
  varName = paste0("ffTimeInter",i)
  #check if file has already been cached (maybe later: outsource as a function): 
  if(file.exists(paste0(path,"Cache\\",target,"\\",varName,".ffData"))){ 
    ffload(paste0(path,"Cache\\",target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target)) # REMOVE debug
  }
  else{ #if not load it from .mat File and save it
  print(paste0("loaded from .mat: ", varName, " for Target: ", target)) # REMOVE debug
  temp = readMat(paste0(path,target,"\\",interictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",  dim = dim(temp[[1]][1][[1]])))
  t = get(varName)
  ffsave(t,list = c(varName), file = paste0(path,"Cache\\", target,"\\",varName))
  }
}
#load preIctal Clips to ff variables
for(i in 1:numFilesPre){
  varName = paste0("ffTimePre",i)
  #check if file has already been cached: 
  if(file.exists(paste0(path,"Cache\\",target,"\\",varName,".ffData"))){ 
    ffload(paste0(path,"Cache\\", target,"\\",varName), overwrite = TRUE)
    print(paste0("loaded from cache: ", varName, " for Target: ", target)) # REMOVE debug
  }else{ #if not load it from .mat File and save it
  temp = readMat(paste0(path,target,"\\",preictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
  t = get(varName) # TODO simpler way?
  ffsave(t,list = c(varName), file = paste0(path,"Cache\\", target,"\\",varName)) 
  print(paste0("loaded from .mat: ", varName, " for Target: ", target)) # REMOVE debug
  }
}
#Extract Sequencenumber for each clip (relevant for splitting test/train sets later)
if(file.exists(paste0(path,"Cache\\",target,"\\Meta\\sequences.txt"))){
  seqOfClips = read.table(paste0(path,"Cache\\",target,"\\Meta\\sequences.txt"), sep="\t")
}else{
allFileNames = c(interictalFileNames, preictalFileNames)
seqOfClips = getSequences(allFileNames)
row.names(seqOfClips) = allFileNames
#save them (loading is extreme expensive!)
write.table(seqOfClips, paste0(path,"Cache\\",target,"\\Meta\\sequences.txt"), sep="\t")
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
    if(file.exists(paste0(path,"Cache\\",target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(path,"Cache\\", target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target)) # REMOVE debug
    }else{ #if not: do fft and save it to cache
    temp = getFFT(get(ffName),freq,freqFrom,freqTo,fftMethod)
    assign(ffFreqName, ff(initdata = temp, vmode = "integer",  dim = dim(temp)))
    t = get(ffFreqName)
    ffsave(t,list = c(ffFreqName), file = paste0(path,"Cache\\", target,"\\",ffFreqName)) 
    print(paste0("transformed with fft: ", ffFreqName, " for Target: ", target)) # REMOVE debug
    }
  }
  #Transform preictal Data
  for(i in 1:numFilesPre){
    ffName = paste0("ffTimePre",i)
    ffFreqName = paste0("ffFreqPre",i)
    #check Cache
    if(file.exists(paste0(path,"Cache\\",target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(path,"Cache\\", target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target))
    }else{ #if not: do fft and save it to cache
    temp = getFFT(get(ffName),freq,freqFrom,freqTo,fftMethod)
    assign(paste0("ffFreqPre",i), ff(initdata = temp, vmode = "integer",dim = dim(temp)))
    t = get(ffFreqName)
    ffsave(t,list = c(ffFreqName), file = paste0(path,"Cache\\", target,"\\",ffFreqName)) 
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
  timeFeatures= cbind(timeVariance(get(ffName)), timeCorrelationAlt(get(ffName)),timeCorrelationEigen(get(ffName))) 
  freqFeatures = cbind(freqCorrelationAlt(get(ffFreqName)), freqCorrelationEigen(get(ffFreqName)), freqLogMagnitudes(get(ffFreqName))) 
  assign(paste0("featureInter", i), cbind(timeFeatures,freqFeatures))
}

#get the features for each preictal clip
for(i in 1: numFilesPre){
  ffName = paste0("ffTimePre",i)
  ffFreqName = paste0("ffFreqPre",i)
  #only temporary version until Tom'S restructuring (deleting the "wishedFeatures" structure)
  timeFeatures= cbind(timeVariance(get(ffName)), timeCorrelationAlt(get(ffName)),timeCorrelationEigen(get(ffName))) 
  freqFeatures = cbind(freqCorrelationAlt(get(ffFreqName)), freqCorrelationEigen(get(ffFreqName)), freqLogMagnitudes(get(ffFreqName)))  
  assign(paste0("featurePre", i), cbind(timeFeatures,freqFeatures))
}

#combine interictal features to one data.frame, remove single clip files and add target column
featureFrameInter = as.data.frame(featureInter1)
for (i in 2: numFilesInter){
  varName = get(paste0("featureInter",i))
  featureFrameInter = rbind(featureFrameInter, varName)
}
featureFrameInter$preseizure = as.factor(rep("No",nrow(featureFrameInter)))
row.names(featureFrameInter) = interictalFileNames
#combine preictal features to one data.frame and add target column
featureFramePre = as.data.frame(featurePre1)
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
write.table(featureFrame, paste0(path,"Cache\\",target,"\\Features\\allFeatures.txt"), sep="\t")

#end of for loop for targets
print(paste0("#### End of Loop for: ", target))
}
####classifier loop
#arrClassifier = c("classifierRandomForest","classifierBayes","classifierBoostLogit","classifierLogTree")
arrClassifier = c("classifierBayes")
#target = "Dog3"

targets = c("Dog1","Dog2","Dog3","Dog4","Dog5")

for(curTarget in 1:length(targets)){
    target = targets[curTarget]
  #(shortcut: loading the current "standard feature frame")
  featureFrame = read.table(paste0(path,"Cache\\",target,"\\Features\\allFeatures"), header=TRUE, sep="\t")
  variance = featureFrame[,1:16]
  timeCorrAlt =  featureFrame[,17:136]
  timeCorrNeu =  featureFrame[,137:256]
  timeEigen = featureFrame[,257:272]
  freqCorrAlt =  featureFrame[,273:392]
  freqCorrNeu = featureFrame[,393:512]
  freqEigen = featureFrame[,513:528]
  logMagn = featureFrame[,529:1280]
  preseizure = featureFrame[,1281]
  
  featureString = "variance,timeCorrAlt,freqCorrAlt"
  featureFrame = cbind(variance,timeCorrAlt,freqCorrNeu,preseizure)
  
  
  featureFrame$preseizure = as.factor(featureFrame$preseizure)
  featureFrameInter = featureFrame[featureFrame$preseizure == "No",] 
  featureFramePre = featureFrame[featureFrame$preseizure == "Yes",]
  
  #get sequences
  seqOfClips = read.table(paste0(path,"Cache\\",target,"\\Meta\\sequences.txt"), sep="\t")
  #split into training and testing sample, keeping the sequences in intact (6 in a row)
  splittedFrame = splitToTestTrain(featureFrameInter,featureFramePre, seqOfClips, 0.75)
  trainData = splittedFrame[[1]]
  testData = splittedFrame[[2]]
  
  
  ########### Classifier ##############################
  #4. train classifier
  print("### Start training the classifier ###")
  #list of possible classifiers
  # lossMatrix = matrix(c(0,100,1,0), nrow=2)
  # classifier = rpartTree(trainData, lossMatrix) #simple (unpruned) CART Tree with package rpart
  # rm(classifier)
  
  #tuned classifiiers from caret package:
  #set tune parameters: 3x 10folds cross validation; later: make sure that train and test data contain *different sequences* of data
  cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
  #cvCtrlClass = trainControl(method = "repeatedcv",number = 5, repeats = 3)
  
    #treebased classifier
    #classifierRpart =           train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "rpart",   parms = list(loss = lossMatrix))#AUC: 0.46
    #classifierc5 =              train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "C5.0", costs = lossMatrix)
  if("classifierRandomForest" %in% arrClassifier){
  print(paste0("evaluating: rf for ", target))
  classifierRandomForest =    train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "rf")}#0.6
    #support vector machines
    #classifierSVMlin =          train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "svmLinear", scaled = FALSE)
    #classifierSVMexp =          train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "svmRadial", scaled = FALSE)#0.51
    #classifierAda =             train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "ada")
  if("classifierBayes" %in% arrClassifier){
  print(paste0("evaluating: bayes for ", target))  
  classifierBayes =           train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "bayesglm")}#0,98
  if("classifierBoostLogit" %in% arrClassifier){
  print(paste0("evaluating: boost logit for ", target))  
  classifierBoostLogit=       train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "LogitBoost")}#0.966
  if("classifierLogTree" %in% arrClassifier){
  print(paste0("evaluating: logtree for ", target))
  classifierLogTree =         train(preseizure ~ ., data = trainData, trControl = cvCtrl, metric = "ROC", method = "LMT")}#0.992
  #Combine the results of the different classifiers and print summary
  # performanceResults <- resamples(list(rPart = classifierRpart,
  #                                     svmLin = classifierSVMlin,
  #                                     svmexp = classifierSVMexp,
  #                                     C5 = classifierc5))
  #summary(performanceResults)
  
  #visualize performance of different train methods with box-and-whisker-plot :
  #bwplot(performanceResults, layout = c(3, 1))
  
  for(k in 1: length(arrClassifier)){
    #curClassifier = classifierBoostLogit
      # 5. evaluate a single classifier
      curClassifier = get(arrClassifier[k])
      print(paste0("evaluating: ",arrClassifier[k]," for ", target))
      #pred = predict(classifier, featureFrame, type = "class") #for rpart
      #pred = predict(curClassifier, testData) #should work for all caret classifiers
      #confusionMatrix(pred,testData$preseizure, positive = "Yes")
      #AUC Calculation (Area under the ROC Curve)
      predRoc = predict(curClassifier, testData, type = "prob")
      myroc = pROC::roc(testData$preseizure, as.vector(predRoc[,2]))
      plot(myroc, print.thres = "best")
      currentScore = auc(myroc) #that's our "final" evaluation score 
      currentScore
      
      #adjust optimal cut-off threshold for class probabilities
      #predAdj = predict(curClassifier, testData, type = "prob")
      threshold = coords(myroc,x="best",best.method = "closest.topleft")[[1]] #get optimal cutoff threshold
      predCut = factor( ifelse(predRoc[, "Yes"] > threshold, "Yes", "No") )
      #predCut = relevel(predCut, "yes")   #try that, if error occurs
      confusionMatrix(predCut, testData$preseizure)
      
      #save the results for this classifier&Feature&target combination to a summary file
      newRow = data.frame(time = as.character(Sys.time()), AUC = currentScore, OptThreshold = threshold,target = target, classifier = curClassifier$modelInfo$label, preprocessing = "fft w/ 1,47,sum",features = featureString)
      oldSummary = read.table(paste0(path,"\\summary.csv"), sep=";", header = TRUE)
      combinedSummary = rbind(oldSummary, newRow)
      write.table(combinedSummary, paste0(path,"\\summary.csv"), sep=";",row.names = FALSE)
      beep()
}
}


########## Prediction & Submission ########## 
#train again with full trainingset 
arrClassifier = c("rf","bayesglm","LMT","LogitBoost")
target = "Dog3"
targets = c("Dog2","Dog3","Dog4","Dog5")
for (curClassifier in 1:length(arrClassifier)){
  classifier = arrClassifier[curClassifier]
for(curTarget in 1:length(targets)){
  target = targets[curTarget]
  a = getFilenames(path,target)
  interictalFileNames = a[[1]]
  preictalFileNames = a[[2]]
  testFileNames = a[[3]]
  featureFrame = read.table(paste0(path,"Cache\\",target,"\\Features\\allFeatures.txt"), header=TRUE, sep="\t")
  variance = featureFrame[,1:16]
  timeCorrAlt =  featureFrame[,17:136]
  timeCorrNeu =  featureFrame[,137:256]
  timeEigen = featureFrame[,257:272]
  freqCorrAlt =  featureFrame[,273:392]
  freqCorrNeu = featureFrame[,393:512]
  freqEigen = featureFrame[,513:528]
  logMagn = featureFrame[,529:1280]
  preseizure = featureFrame[,1281]
  
  #combining the wanted features
  featureString = "timeCorrNeu,freqCorrNeu,logMagn"
  featureFrame = cbind(timeCorrNeu,freqCorrNeu,logMagn,preseizure) 
  featureFrame$preseizure = as.factor(featureFrame$preseizure)

  #splitting the features (for testclips)
  featureFrameTest = read.table(paste0(path,"Cache\\",target,"\\Features\\allTestFeatures.txt"), header=TRUE, sep="\t")
  variance = featureFrameTest[,1:16]
  timeCorrAlt =  featureFrameTest[,17:136]
  timeCorrNeu =  featureFrameTest[,137:256]
  timeEigen = featureFrameTest[,257:272]
  freqCorrAlt =  featureFrameTest[,273:392]
  freqCorrNeu = featureFrameTest[,393:512]
  freqEigen = featureFrameTest[,513:528]
  logMagn = featureFrameTest[,529:1280]
  
  #combining the wanted features
  featureFrameTest = cbind(timeCorrNeu,freqCorrNeu,logMagn) 

cvCtrl = trainControl(method = "repeatedcv",number = 10, repeats = 3, classProbs = TRUE, summaryFunction = twoClassSummary)
print(paste0("classifier: ",classifier," target: ",target))
finalClassifier =    train(preseizure ~ ., data = featureFrame, trControl = cvCtrl, metric = "ROC", method = classifier)#0.6

#6. Make Predictions for test clips
#if(makePredictions) source("preprocessTestclips.R")
#7. Create Submission File in Data/Submission
#per Target:
versionname = paste0("-",classifier,"-",featureString)
submissionName = paste0(target,version)
createTargetSubmission(finalClassifier, featureFrameTest, testFileNames, target, submissionName)
beep()
#combine them all 
createFullSubmission(versionName)
}}
#### OTHER STUFF / HELPER / ETC. ####


#if tree: plot can be interesting
# plot(classifierRpart$finalModel)
# text(classifierRpart$finalModel)
