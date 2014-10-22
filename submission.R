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
  header = "clip, preictal" 
  combined = cbind(filenames, pred[,2])
  write(t(combined), paste0("D:\\Seizure Competition\\Data\\Submission\\",target,".csv"), ncolumns=2,sep=",")
}