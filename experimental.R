pred = read.table(paste0(path,"Submission\\fullSubmission2014-10-31-14h.csv"), header =TRUE, sep =",")
pred$clip = as.character(pred$clip)
pred$preictal = as.numeric(pred$preictal)

dog1=pred[grepl("Dog_1+",pred$clip),]
dog2=pred[grepl("Dog_2+",pred$clip),]
dog3=pred[grepl("Dog_3+",pred$clip),]
dog4=pred[grepl("Dog_4+",pred$clip),]
dog5=pred[grepl("Dog_5+",pred$clip),]

#adjust
thresh = c(0.027,0.241,0.055,0.433,0.097)
dog1above = dog1[dog1$preictal>=0.027,]
dog1below = dog1[dog1$preictal<0.027,]
dog1above$preictal = scaleProbsAbove(dog1above$preictal,0.027)
dog1below$preictal = scaleProbsBelow(dog1below$preictal, 0.027)
dog1adj = rbind(dog1above, dog1below )

dog2above = dog2[dog2$preictal>=0.241,]
dog2below = dog2[dog2$preictal<0.241,]
dog2above$preictal = scaleProbsAbove(dog2above$preictal,0.241)
dog2below$preictal = scaleProbsBelow(dog2below$preictal, 0.241)
dog2adj = rbind(dog2above, dog2below )


dog3above = dog3[dog3$preictal>=0.055,]
dog3below = dog3[dog3$preictal<0.055,]
dog3above$preictal = scaleProbsAbove(dog3above$preictal,0.055)
dog3below$preictal = scaleProbsBelow(dog3below$preictal, 0.055)
dog3adj = rbind(dog3above, dog3below )

dog4above = dog4[dog4$preictal>=0.433,]
dog4below = dog4[dog4$preictal<0.433,]
dog4above$preictal = scaleProbsAbove(dog4above$preictal,0.433)
dog4below$preictal = scaleProbsBelow(dog4below$preictal, 0.433)
dog4adj = rbind(dog4above, dog4below )

dog5above = dog5[dog5$preictal>=0.097,]
dog5below = dog5[dog5$preictal<0.097,]
dog5above$preictal = scaleProbsAbove(dog5above$preictal,0.097)
dog5below$preictal = scaleProbsBelow(dog5below$preictal, 0.097)
dog5adj = rbind(dog5above, dog5below )

#combine and write
combined = rbind(dog1adj,dog2adj,dog3adj,dog4adj,dog5adj,dummies)
write.table(combined ,paste0(path,"Submission\\fullSubmission",substr(Sys.time(),1,10),"-", substr(Sys.time(),12,13),"h.csv"), sep=",", quote = FALSE, row.names = FALSE)


scaleProbsAbove= function(probs, thresh){
  thresh = rep(thresh,length(probs))
  scaledProbs = ((probs-thresh)/(1-thresh))*0.5+0.5
  return(scaledProbs)
}

scaleProbsBelow= function(probs, thresh){
  thresh = rep(thresh,length(probs))
  scaledProbs = ((probs-thresh)/thresh)*0.5+0.5
  return(scaledProbs)
}