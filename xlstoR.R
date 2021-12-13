

file.list <- list.files(path="f:/Rspace/From Excel to R/",pattern='*.xlsx',recursive = T)
file.list <- file.list[-c(grep("\\~\\$",file.list))]
df.list <- lapply(paste0("./From Excel to R/",file.list), read_excel)

library(data.table)
df <- rbindlist(df.list, idcol = "id")
d <- df [rowSums(is.na(df[,3:188])) != ncol(df[,3:188]),]
write.csv(d,"data2018.csv")
