
getFilenames= function(path){
# get all the correct filenames from the path
fileNames = list.files(path)
# only filenames with the targets in it 
targetString = ""
for (i in 1:(length(targets)-1)){
  print(i)
  targetString = paste0(targetString,targets[i],"|")
}
paste0(targetString,targets[i+1])
# split the filenames according to seizure period
fileNames = fileNames[grepl(paste0("+(",targets[1],"|",targets[2],")+"),fileNames)]
interictalFileNames = fileNames[grepl("+interictal+",fileNames)]
preictalFileNames = fileNames[grepl("+preictal+",fileNames)]
testFileNames = fileNames[grepl("+test+",fileNames)]
return(list(interictalFileNames,preictalFileNames,testFileNames))
}

## Load Data
# change "20" with dynamic statement "length(files)" later!
loadData = function(path, files){
  data = vector(mode="list", length=20)
  for(i in 1:20){
    filePath = paste0(path,files[i])
    temp = readMat(filePath)
    data[[i]] = temp[[1]][1][[1]]
  }
  names(data) = files[1:20]
  return(data)
}

## Load function incorporating the ff-package for large data sets
# not working yet
loadBigFiles = function(path, files){
  for(i in 1:2){
    assign (paste0(big,file[i]), loadSingleBigFile)
  }
  # assign all variables
  return(bigFile)
}

# loading a single .mat file into a ff file
loadSingleBigFile = function (filepath){
  temp = readMat(filepath)
  # bigFile = ff(vmode = "short",dim = dim(temp[[1]][1][[1]]))
  bigFile = temp[[1]][1][[1]]
  rm(temp)
  gc()
  return(bigFile)
}
