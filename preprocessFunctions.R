##new Version:
getFeatures = function (ffClip, wishedFeatures, startColumns = 16,type){
  feature = data.frame(matrix(NA,1,startColumns))
 # row.names(feature) = "Clipname"
 colCounter = 0
 
 ##Assembling the wished Features ##
 ## 1. Variance ##
 if('variance' %in% wishedFeatures){
   numNewFeatures = 16 #number of features, which will be added
   newColNeeded = (colCounter+numNewFeatures)-ncol(feature)
   #add new (empty) columns
   if (newColNeeded>0) feature = addColumns(feature,newColNeeded)
   #calculate Variance
     feature[1,(colCounter+1):(colCounter+numNewFeatures)] = getVarianceOfClip(ffClip)
   #Name the columns
   columnNames = rep(0,numNewFeatures)
   for (j in 1:numNewFeatures){
     columnNames[j+colCounter] = paste0('VarCh',j)
   }
   colCounter = colCounter+numNewFeatures
 }

 # return the assembled features
 names(feature) = columnNames
 return(feature)
}


##Old Version:
getFeaturesOld = function (dataList, wishedFeatures,startColumns=16){
  ##gets all the wanted features from the data
  #in: dataList = includes different clips; each clip is an array of 16 eeg-channels x 239766 timepoints; fields = voltages 
  #   wishedFeatures = array of strings stating which features should be extracted from dataList
  #   startColumns = optional number which states the preallocated column size of the feature data frame
  #out: feature = data frame with clips as rows and features as columns
  
  # preallocate data frame
  feature = data.frame(matrix(NA,length(dataList),startColumns))
  row.names(feature) = names(dataList)
  colCounter = 0 #counts the number of already added feature columns (-> find the col for the next new feature)
  
  ##Assembling the wished Features ##
  ## 1. Variance ##
  if('variance' %in% wishedFeatures){
    numNewFeatures = 16 #number of features, which will be added
    newColNeeded = (colCounter+numNewFeatures)-ncol(feature)
    #add new (empty) columns
    if (newColNeeded>0) feature = addColumns(feature,newColNeeded)

    #calculate Variance for each clip
    for (i in 1:length(dataList)){
      feature[i,(colCounter+1):(colCounter+numNewFeatures)] = getVarianceOfClip(dataList[[i]])
    }
    #Name the columns
    columnNames = rep(0,numNewFeatures)
    for (j in 1:numNewFeatures){
      columnNames[j+colCounter] = paste0('VarCh',j)
    }
    colCounter = colCounter+numNewFeatures
  }
  
  ## 2. Correlation ##
  if('correlation' %in% wishedFeatures){
    numNewFeatures = choose(16,2) #number of features, which will be added
    newColNeeded = (colCounter+numNewFeatures)-ncol(feature)
    #add new (empty) columns
    if (newColNeeded>0) feature = addColumns(feature,newColNeeded)
    
    for (i in 1:length(dataList)){
      feature[i,(colCounter+1):(colCounter+numNewFeatures)] = getCorrelationOfClip(dataList[[i]])
    }
    
  }
  # return the assembled features
  names(feature) = columnNames
  return(feature)
}

###further features to implement: 
# bipolar filter, low pass filter, fast fourier trans. , normalization, time series correlation, frequ. correlation ...
############
## helper ##
############
## Variance:
getVarianceOfClip = function (dataArray){
  ## in: data of one clip (as an array)
  ## out: the variance of each channel of one clip (as a vector)
  variance = rep(0,dim(dataArray)[1])
  for (i in 1:dim(dataArray)[1]){
    variance[i] = var(dataArray[i,])
  }
  return(variance)
}

getCorrelationOfClip = function (dataArray){
  ## in: data of one clip (as an array)
  ## out: the variance of each channel of one clip (as a vector)
  correlation = cor(dataArray)
  return(variance)
}

##add columns
addColumns = function(frame, numNew){
  #adds "numNew" times a new column to the data frame "frame"
  print(nrow(frame))
  print(numNew)
  frame[,(ncol(frame)+1):(ncol(frame)+numNew)] = matrix(NA,nrow(frame),numNew)
  return(frame)
}
