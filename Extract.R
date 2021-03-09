library(plyr)
names_samples=read.csv("E:/UMinho/2semestre/ECBDB/trabalhogrupo/samples_files.csv")
samples=names_samples[,1]
zipF <- list.files(path = "E:/UMinho/2semestre/ECBDB/trabalhogrupo/1/data", pattern = "*.zip", full.names = TRUE)
names_file=list.files(path = "E:/UMinho/2semestre/ECBDB/trabalhogrupo/1/data", pattern = "*.zip", full.names = FALSE)
# unzip all your files
for (i in 1:length(zipF)){
  ldply(.data = zipF[i], .fun = unzip, exdir = paste("E:/UMinho/2semestre/ECBDB/trabalhogrupo/1/extracted/", paste(samples[i],".fid",sep=""), sep = ""))
}
