createTargetSubmission = function(classifier, data ,filenames,target,submissionName){

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
  write(t(combined), paste0(path,"Submission\\",submissionName,".csv"), ncolumns=2,sep=",")
}

createFullSubmission = function (version){
  #combine the different files in the submission folder
  predDog1 = read.table(paste0(path,"Submission\\Dog1",version,".csv"), header =TRUE, sep =",")
  predDog1$clip = as.character(predDog1$clip)
  predDog1$preictal = as.numeric(predDog1$preictal) 
  predDog2 = read.table(paste0(path,"Submission\\Dog2",version,".csv"), header =TRUE, sep =",")
  predDog2$clip = as.character(predDog2$clip)
  predDog2$preictal = as.numeric(predDog2$preictal)
  predDog3 = read.table(paste0(path,"Submission\\Dog3",version,".csv"), header =TRUE, sep =",")
  predDog3$clip = as.character(predDog3$clip)
  predDog3$preictal = as.numeric(predDog3$preictal)
  predDog4 = read.table(paste0(path,"Submission\\Dog4",version,".csv"), header =TRUE, sep =",")
  predDog4$clip = as.character(predDog4$clip)
  predDog4$preictal = as.numeric(predDog4$preictal)
  predDog5 = read.table(paste0(path,"Submission\\Dog5",version,".csv"), header =TRUE, sep =",")
  predDog5$clip = as.character(predDog5$clip)
  predDog5$preictal = as.numeric(predDog5$preictal)
  predPat1 = read.table(paste0(path,"Submission\\Patient1",version,".csv"), header =TRUE, sep =",")
  predPat1$clip = as.character(predPat1$clip)
  predPat1$preictal = as.numeric(predPat1$preictal)
  predPat2 = read.table(paste0(path,"Submission\\Patient2",version,".csv"), header =TRUE, sep =",")
  predPat2$clip = as.character(predPat2$clip)
  predPat2$preictal = as.numeric(predPat2$preictal)
  
  #extract dummy predictions from sampleSubmission
  sampleSubmission =  read.table(paste0(path, "Submission\\sampleSubmission.csv"), header = TRUE, sep=",")
  sampleSubmission$clip =  as.character(sampleSubmission$clip)
  #get all those rows of the dummy targets
   dummies =  sampleSubmission[grepl("+Patient_1+",sampleSubmission[,1]),]  
  #combine it with real predictions
  combined = rbind(predDog1,predDog2,predDog3,predDog4,predDog5,dummies,predPat2)
  write.table(combined ,paste0(path,"Submission\\fullSubmission",substr(Sys.time(),1,10),"-", substr(Sys.time(),12,13),"h.csv"), sep=",", quote = FALSE, row.names = FALSE)
}

