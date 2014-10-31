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
  write(t(combined), paste0(path,"Submission\\",target,".csv"), ncolumns=2,sep=",")
}

createFullSubmission = function (){
  #combine the different files in the submission folder
  predDog1 = read.table(paste0(path,"Submission\\Dog1.csv"), header =TRUE, sep =",")
  predDog1$clip = as.character(predDog1$clip)
  predDog1$preictal = as.numeric(predDog1$preictal) 
  predDog2 = read.table(paste0(path,"Submission\\Dog2.csv"), header =TRUE, sep =",")
  predDog2$clip = as.character(predDog2$clip)
  predDog2$preictal = as.numeric(predDog2$preictal)
  predDog4 = read.table(paste0(path,"Submission\\Dog4.csv"), header =TRUE, sep =",")
  predDog4$clip = as.character(predDog4$clip)
  predDog4$preictal = as.numeric(predDog4$preictal)
  predDog5 = read.table(paste0(path,"Submission\\Dog5.csv"), header =TRUE, sep =",")
  predDog5$clip = as.character(predDog5$clip)
  predDog5$preictal = as.numeric(predDog5$preictal)
  
  #extract dummy predictions from sampleSubmission
  sampleSubmission =  read.table(paste0(path, "Submission\\sampleSubmission.csv"), header = TRUE, sep=",")
  sampleSubmission$clip =  as.character(sampleSubmission$clip)
  #get all those rows of the dummy targets
   dummies =  sampleSubmission[grepl("+Dog_3+|+Patient_1+|+Patient_2+",sampleSubmission[,1]),]  
  #combine it with real predictions
  combined = rbind(predDog1,predDog2,predDog4,predDog5,dummies)
  write.table(combined ,paste0(path,"Submission\\fullSubmission",substr(Sys.time(),1,10),"-", substr(Sys.time(),12,13),"h.csv"), sep=",", quote = FALSE, row.names = FALSE)
}

