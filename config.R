###Set Path to Data Files
#dataPath = "D:\\Seizure Competition\\Data\\"
path = "C:\\Seizure Competition\\Data\\"
#pathCache = "D:\\Seizure Competition\\Data\\Cache\\"
#targets = c("Dog_1","Dog_2")
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
#registerDoParallel(cores = 2) #number of CPU cores
#path for the temporary ff files 
options(fftempdir = "C:\\Seizure Competition\\Data\\Temp")
#########

