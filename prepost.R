file ="training pharmacy1.csv"

f <- read.csv(file, header=T)
fme <- melt (f)
dfme <- dcast(fme,  Trainee + variable ~ Assess,fun.aggregate = sum)
