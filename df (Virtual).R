
setwd("C:/Users/abid/OneDrive/PROGRAMMING//RSpace/2018-19/code")

file = "C:/Users/abid/OneDrive/PROGRAMMING/RSpace/2018-19/data/df (Virtual).csv"
source("df (Virtual) functions.R")
library(likert)
library (lattice)
library (dplyr)
library(reshape2)
library(tidyverse)
library(ggcorrplot)
library(splitstackshape)
library(PerformanceAnalytics)
library(plotrix)
library(vcd)
library(gmodels)
library(scales)
fall <- read.csv(file, header=T)
cl = ncol(fall)
names(fall)[(cl-43):cl] <-  question_short_names()

f <- extract_only_sceq_from_merged_proforma(fall,question_short_names())
f <- add_faculty_column(f)

ff <- extract_only_fac_eval_from_merged_proforma(fall)
ff <- add_faculty_column(ff)


#take 10% grouped samples from original dataset f
f <- stratified(f, group = c("Campuses","Faculties","Levels","Programs"),size = 1)

# the sceq prefix is used for data frames with indicators in column headings with mean values
#sceq <- SCEQ_get_rowMeans(f)
#sceq_correlogram(sceq,By='Faculties')#by Campuses, Faculties, Programs

#pdf(file = '../output/sceq_cor.pdf', onefile = T)
#sceq_correlogram(sceq,By = 'Programs')
#dev.off()


f_with_labels <- relevel_all_with_label_names (f) # e.g. strongly agree , disagree etc.
make_likert_plot(f_with_labels,"Students Course Evaluation 2018-19")

ff_with_labels <- relevel_all_with_label_names(ff)
make_likert_plot(ff_with_labels,"Faculty Evaluation 2018-19")

fmelted <- melt_data(f)
make_correlogram(fmelted, By="Faculties",titl="Students Course EValuation 2018-19") # by Faculties, Campuses, or Programs
#make_correlogram(fmelted, By="Campuses")
#make_correlogram(fmelted, By="Programs")

ffmelted <- melt_data(ff)
ffmelted$Indicators <- ffmelted$Qshorts # for compatibility with SCEQ
make_correlogram(ffmelted, By="Faculties",titl="Faculty Evaluation 2018-19")

fagregated <- aggregate(fmelted$rating, by=list(fmelted$Campuses,fmelted$Faculties,fmelted$Programs,fmelted$Indicators,fmelted$Levels),FUN = mean)
names(fagregated) <- c("Campuses","Faculties","Programs","Indicators","Levels","Means")

# By = 'Faculties' or 'Campuses' or 'Programs'
make_bw_plot(fagregated, By = 'Faculties',titl = "Students Course Evaluation 2018-19")

ffagregated <- aggregate(ffmelted$rating, by=list(ffmelted$Campuses,ffmelted$Faculties,ffmelted$Programs,ffmelted$Indicators,ffmelted$Levels),FUN=mean)
names(ffagregated) <- c("Campuses","Faculties","Programs","Indicators","Levels","Means")
make_bw_plot(ffagregated, By = 'Faculties',titl="Faculty Evaluation 2018-19")

pdf(file = "../output/df (Virtual) report/barchart.pdf",onefile = T)
make_bw_plot(fagregated , By = 'Programs',titl="Students Course Evaluation 2018-19") #if by Programs then barcharts are created
dev.off()

pdf(file = "../output/df (Virtual) report/ffbarchart.pdf",onefile = T)
make_bw_plot(ffagregated , By = 'Programs',titl = "Faculty Evaluation 2018-19") #if by Programs then barcharts are created
dev.off()


#pie3D 
percent1 <- aggregate(fmelted$Programs, by=list(fmelted$Campuses,fmelted$Faculties,fmelted$Levels,fmelted$Programs,fmelted$rating),length)
names(percent1) <- c('Campuses','Faculties','Levels','Programs','rating','Counts')

make_pie3D(percent1,titl="Students_Course_Evaluation_2018-19")

ffpercent1 <- aggregate(ffmelted$Programs, by=list(ffmelted$Campuses,ffmelted$Faculties,ffmelted$Levels,ffmelted$Programs,ffmelted$rating),length)
names(ffpercent1) <- c('Campuses','Faculties','Levels','Programs','rating','Counts')

make_pie3D(ffpercent1,titl="Faculty_Evaluation_2018_19")




tab = CrossTable(fmelted$Indicators,fmelted$rating,chisq = T)
#barplot(tab$prop.row*100)
barpp <- as.data.frame(tab$prop.row)
names(barpp) <- c('Indicators','Rating','percent')
#barpp$percent <- round(barpp$percent*100,2)
ggplot(barpp,aes(x=factor(Rating),y=percent,fill=factor(Rating)))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Set1')+
  ggtitle(label='Barplot of Students Course Evaluation 2018-19\nUniversity of Sindh')+
  scale_y_continuous(labels=percent)



fftab = CrossTable(ffmelted$Indicators,ffmelted$rating,chisq = T)
#barplot(tab$prop.row*100)
ffbarpp <- as.data.frame(fftab$prop.row)
names(ffbarpp) <- c('Indicators','Rating','percent')
#barpp$percent <- round(barpp$percent*100,2)
ggplot(ffbarpp,aes(x=factor(Rating),y=percent,fill=factor(Rating)))+
  geom_bar(stat='identity',position='dodge')+
  scale_fill_brewer(palette = 'Set1')+
  ggtitle(label='Barplot of Faculty Evaluation 2018-19\nUniversity of Sindh')+
  scale_y_continuous(labels=percent)

