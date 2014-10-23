createTargetSubmission = function(classifier, data ,filenames,target){

#only for testing:
#   classifier = curClassifier
#   data  = featureFrame
#   filenames = c(interictalFileNames, preictalFileNames)
#   filenames
#   target = "Dog1"
  
  #get predictions
  pred = predict(classifier, data, type = "prob")
  #select only prob. for preseizure and write them to a file
  header = c("clip", "preictal") 
  combined = cbind(filenames, pred[,2])
  combined = rbind(header,combined)
  write(t(combined), paste0(dataPath,"Submission\\",target,".csv"), ncolumns=2,sep=",")
}

createFullSubmission = function (){
  #combine the different files in the submission folder
  predDog1 = read.table(paste0(dataPath,"Submission\\",target,".csv"), header =TRUE, sep =",")
  predDog1$clip = as.character(predDog1$clip)
  predDog1$preictal = as.numeric(predDog1$preictal)  
  #extract dummy predictions from sampleSubmission
  sampleSubmission =  read.table(paste0(dataPath, "Submission\\sampleSubmission.csv"), header = TRUE, sep=",")
  sampleSubmission$clip =  as.character(sampleSubmission$clip)
  #get all those rows of the dummy targets
   dummies =  sampleSubmission[grepl("+Dog_2+|+Dog_3+|+Dog_4+|+Dog_5+|+Patient_1+|+Patient_2+",sampleSubmission[,1]),]  
  #combine it with real predictions
  combined = rbind(predDog1,dummies)
  write.table(combined ,paste0(dataPath,"Submission\\fullSubmission.csv"), sep=",", quote = FALSE, row.names = FALSE)
}

