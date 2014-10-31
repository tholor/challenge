#produces Predictions for test clips (load, transform, get features, predict)
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
    t = get(varName) # TODO simpler way?
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
  timeFeatures=getFeatures(get(ffName),wishedFeatures,16)
  freqFeatures = freqCorrelation(get(ffFreqName)) 
  assign(paste0("featureTest", i), cbind(timeFeatures,freqFeatures))
}
#combine them to testFrame
featureFrameTest = featureTest1
for (i in 2: numFilesTest){
  varName = get(paste0("featureTest",i))
  featureFrameTest = rbind(featureFrameTest, varName)
}
