
rm(list = ls())
gc()
#1. load packages
library(R.matlab)
library(rpart)
library(ff)
library(pracma)

#2. load config
source("config.R")
source("loadFunctions.R")
source("preprocessFunctions.R")


#3. preprocess data
########## Loading #################
# 3a)load data
#load interictal clips to ff variables
numFiles = 24#how many of the available clips should be loaded

for(i in 1:numFiles){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffInter",i)
  temp = readMat(paste0(path,interictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",  dim = dim(temp[[1]][1][[1]])))
}

#load preIctal Clips to ff variables
for(i in 1:numFiles){
  #later: check here if the file has already been loaded before (=> Cache) 
  varName = paste0("ffPre",i)
  temp = readMat(paste0(path,preictalFileNames[i]))
  assign(varName, ff(initdata = temp[[1]][1][[1]], vmode = "short",dim = dim(temp[[1]][1][[1]])))
}

# # exerimental: fft
# Fs = 400;                   # % Sampling frequency
# T = 1/Fs;                    # % Sample time
# L = 239766;                    # % Length of signal
# t = c(0:(L-1))*T  
# #apply fourier transformation to detect the original sinus signals
# #NFFT etc for scaling and extracting only the positive values from fft
# NFFT = 2^nextpow2(L); #Next power of 2 from length of y
# 
# Y = fft(ffInter1[2,])/L;
# #take only one half of the measures and scale them
# Y_pos = 2*abs(Y[1:(NFFT/2+1)])
# f = (Fs/2)*seq(0,1,length = NFFT/2+1)
# 
# #Plot single-sided amplitude spectrum.
# plot(f,Y_pos,type="l") 
# title('Single-Sided Amplitude Spectrum of y(t)')
# #get frequencies with high amplitudes
# f[which(Y_pos>0.4)]



########## Features ##########
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
#combine interictal features to one data.frame, remove single clip files and add target column
featureFrameInter = featureInter1
for (i in 2: numFiles){
  varName = get(paste0("featureInter",i))
  featureFrameInter = rbind(featureFrameInter, varName)
}
featureFrameInter$preseizure = rep(0,nrow(featureFrameInter))
#combine preictal features to one data.frame and add target column
featureFramePre = featurePre1
for (i in 2: numFiles){
  varName = get(paste0("featurePre",i))
  featureFramePre = rbind(featureFramePre, varName)
}
featureFramePre$preseizure = rep(1,nrow(featureFramePre))
#remove unused ff files and variables
rm(list = ls()[grepl("+ffPre+",ls())])
rm(list = ls()[grepl("+ffInter+",ls())])
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
