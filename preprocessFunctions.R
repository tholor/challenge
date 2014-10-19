##new Version:
getFeatures = function (ffClip, wishedFeatures, startColumns = 16,type){
  feature = data.frame(matrix(NA,1,startColumns))
  colCounter = 0
  columnNames = rep(0,startColumns)
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
   for (j in 1:numNewFeatures){
     columnNames[j+colCounter] = paste0('VarCh',j)
   }
   colCounter = colCounter+numNewFeatures
 }
 
 # 2. Correlation for the time dimension ##
 if('correlation' %in% wishedFeatures){
   numNewFeatures = choose(16,2) #number of features, which will be added
   newColNeeded = (colCounter+numNewFeatures)-ncol(feature)
   #add new (empty) columns
   if (newColNeeded>0) feature = addColumns(feature,newColNeeded)
   #get correlation 
   feature[1,(colCounter+1):(colCounter+numNewFeatures)] = getCorrelationOfClip(t(ffClip[,])) 
   #name the columns (cor1-2 cor1-3 ... )
   combinations = combn(1:dim(ffClip[])[1],2)
   for (j in 1:numNewFeatures){
     columnNames[j+colCounter] = paste0('Cor',combinations[1,j],".",combinations[2,j])
   }
   colCounter = colCounter+numNewFeatures
 } 
 # name and return the assembled features
 names(feature) = columnNames
 return(feature)
}

# 3. Correlation for the frequency dimension
#input: ffClip which has already been transformed by fft and reduced to essential dimensions (otherwise giant correlation matrix)
#To Do: sanity check
freqCorrelation = function(ffClip){
  #in: ff File
  #out: matrix
  numOfChannels = dim(ffClip[,])[1]
  numOfCorrelations = choose(numOfChannels,2)
  columnNames = rep(NA, numOfCorrelations)                       
  features = matrix(getCorrelationOfClip(t(ffClip[,])), nrow=1)
  #Naming
  combinations = combn(1:numOfChannels,2)
  for (j in 1:numOfCorrelations){
    columnNames[j] = paste0('freqCor',combinations[1,j],".",combinations[2,j])
  }
  colnames(features) = columnNames
  return(features)
}

###further features to implement: 
# bipolar filter, low pass filter, fast fourier trans. , normalization, time series correlation, frequ. correlation ...


getFFT = function(ffClip){
  #in: ff File
  #out: matrix
  ffClip =ffTimeInter1
  Fs = 400;                   # % Sampling frequency
  T = 1/Fs;                    # % Sample time
  L = 239766;                    # % Length of signal
  t = c(0:(L-1))*T  
  #apply fourier transformation to detect the original sinus signals
  #NFFT etc for scaling and extracting only the positive values from fft
  NFFT = 2^nextpow2(L); #Next power of 2 from length of y
  #preallocate matrix
  Y_scaled = matrix(NA, nrow = dim(ffClip), ncol = NFFT/2+1)
  #Scale frequencies
  f = (Fs/2)*seq(0,1,length = NFFT/2+1)
  #now transform each channel of the ffCLip:
  for (i in 1: dim(ffClip)[1]){
    Y_temp = fft(ffClip[i,])/L;
    #take only one half of the measures and scale them
    Y_scaled[i,] = 2*abs(Y_temp[1:(NFFT/2+1)])
  }
  colnames(Y_scaled) = f
  return (Y_scaled)
  
  #get frequencies with high amplitudes
  #f[which(Y_scaled>0.2)]
  #Plot single-sided amplitude spectrum.
  #plot(colnames(Y_scaled),Y_scaled[15,],type="l") 
}

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

getCorrelationOfClip = function (data){
  ## in: data of one clip
  ## out: the variance of each channel of one clip (as a vector with cor(1,2), cor(1,3)...cor(2,3)...)
  ## remember: if there are memory problems, try to change "cor" to "bigcor"  
  correlation = cor(data)
  lowerTriangle = correlation[lower.tri(correlation[,])]
  return(lowerTriangle)
}

##add columns
addColumns = function(frame, numNew){
  #adds "numNew" times a new column to the data frame "frame"
  frame[,(ncol(frame)+1):(ncol(frame)+numNew)] = matrix(NA,nrow(frame),numNew)
  return(frame)
}
