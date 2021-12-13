file.list <- list.files(path="f:/Rspace/S/",pattern='*.jpg',recursive = T)
l = length(file.list)

f <- read.csv("../ProgsTmp.csv",header=T)
f2 <- as.character(f$Depart)
#for (i in 1:l){
  file.rename( from = list.files(pattern="*.jpg"), to = paste0(f2,".jpg"))
#}