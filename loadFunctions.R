library(ff)

#get all the correct filenames from the path
fileNames = list.files(path)
#only filenames with the targets in it 
targetString = ""
for (i in 1:(length(targets)-1)){
  print(i)
  targetString = paste0(targetString,targets[i],"|")
}
paste0(targetString,targets[i+1])
#split the filenames according to seizure period
fileNames = fileNames[grepl(paste0("+(",targets[1],"|",targets[2],")+"),fileNames)]
interictalFileNames = fileNames[grepl("+interictal+",fileNames)]
preictalFileNames = fileNames[grepl("+preictal+",fileNames)]
testFileNames = fileNames[grepl("+test+",fileNames)]


###Load Data
#change "20" with dynamic statement later!
loadData = function(path, files){
  data = vector(mode="list", length=20)
  for(i in 1:20){
    filePath = paste0(path,files[i])
    temp = readMat(filePath)
    #foo = ff(files[i], vmode = "short",dim = dim(temp[[1]][1][[1]]))
    data[[i]] = temp[[1]][1][[1]]
    #foo = temp[[1]][1][[1]]
  }
  names(data) = files[1:20]
  #ff(data)
  return(data)
}

loadBigData = function(path, files){
  for(i in 1:20){
    filePath = paste0(path,files[i])
    temp = as.ff(readMat(filePath))
    #foo = ff(files[i], vmode = "short",dim = dim(temp[[1]][1][[1]]))
    data[[i]] = temp[[1]][1][[1]]
    #foo = temp[[1]][1][[1]]
  }
  names(data) = files[1:20]
  #ff(data)
  return(data)
}
