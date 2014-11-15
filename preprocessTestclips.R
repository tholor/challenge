#produces Predictions for test clips (load, transform, get features, predict)

targets = c("Patient2","Patient1")
for(curTarget in 1:length(targets)){
  rm(list = ls()[!ls() %in% c("targets","curTarget")])
  gc()
  
  source("config.R")
  source("loadFunctions.R")
  source("preprocessFunctions.R")
  source("classifier.R")
  source("submission.R")
  
  target = targets[curTarget]
  a = getFilenames(path,target)
  interictalFileNames = a[[1]]
  preictalFileNames = a[[2]]
  testFileNames = a[[3]]
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
  
  #load them
  #load test Clips to ff variables
  
  for(i in 1:numFilesTest){
    varName = paste0("ffTimeTest",i)
    #check if file has already been cached: 
    if(file.exists(paste0(path,"Cache\\",target,"\\",varName,".ffData"))){ 
      ffload(paste0(path,"Cache\\", target,"\\",varName), overwrite = TRUE)
      print(paste0("loaded from cache: ", varName, " for Target: ", target)) # REMOVE debug
    }else{ #if not load it from .mat File and save it
      temp = readMat(paste0(path,target,"\\",testFileNames[i]))
      assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
      t = get(varName) 
      ffsave(t,list = c(varName), file = paste0(path,"Cache\\", target,"\\",varName)) 
      print(paste0("loaded from .mat: ", varName, " for Target: ", target)) # REMOVE debug
    }
  }
  #transform them
  for(i in 1:numFilesTest){
    ffName = paste0("ffTimeTest",i)
    ffFreqName = paste0("ffFreqTest",i)
    #check Cache
    if(file.exists(paste0(path,"Cache\\",target,"\\",ffFreqName,".ffData"))){ 
      ffload(paste0(path,"Cache\\", target,"\\",ffFreqName), overwrite = TRUE)
      print(paste0("FFT from cache: ", ffFreqName, " for Target: ", target))
    }else{ #if not: do fft and save it to cache
      temp = getFFT(get(ffName),freq,freqFrom,freqTo,fftMethod)
      assign(paste0("ffFreqTest",i), ff(initdata = temp, vmode = "integer",dim = dim(temp)))
      t = get(ffFreqName)
      ffsave(t,list = c(ffFreqName), file = paste0(path,"Cache\\", target,"\\",ffFreqName)) 
      print(paste0("transformed with fft: ", ffFreqName, " for Target: ", target))
    }
  }
  #get features
  for(i in 1: numFilesTest){
    ffName = paste0("ffTimeTest",i)
    ffFreqName = paste0("ffFreqTest",i)
    #only temporary version until Tom'S restructuring (deleting the "wishedFeatures" structure)
    timeFeatures= cbind(timeVariance(get(ffName)), timeCorrelationAlt(get(ffName)),timeCorrelationEigen(get(ffName))) 
    freqFeatures = cbind(freqCorrelationAlt(get(ffFreqName)), freqCorrelationEigen(get(ffFreqName)), freqLogMagnitudes(get(ffFreqName)))  
    assign(paste0("featureTest", i), cbind(timeFeatures,freqFeatures))
  }
  #combine them to testFrame
  featureFrameTest = featureTest1
  for (i in 2: numFilesTest){
    varName = get(paste0("featureTest",i))
    featureFrameTest = rbind(featureFrameTest, varName)
  }
  #save them
  write.table(featureFrameTest, paste0(path,"Cache\\",target,"\\Features\\allTestFeatures.txt"), sep="\t")
}