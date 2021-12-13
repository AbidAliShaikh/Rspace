
extract_only_sceq_from_merged_proforma <- function(fall,question_shortnames){
  
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""))
   fall <- fall[,!(names(fall) %in% ptrn)]
   #f <- NULL
   #l = ncol(fall)
   for (i in id_columns(fall)) fall[,i] <- factor(fall[,i])
   return (fall)
  
}

extract_only_fac_eval_from_merged_proforma <- function(fall){
  ptrn= c(paste("FACEV0",5:9,sep=""),paste("FACEV",10:19,sep=""))
  fall <- cbind(fall[,id_columns(fall)],fall[,(names(fall) %in% ptrn)])
  #f <- NULL
  #l = ncol(fall)
  for (i in id_columns(fall)) fall[,i] <- factor(fall[,i])
  return (fall)
  
  
  
}

melt_data <- function(f){
  
  fmelted <- melt(f, id.vars  = id_columns(f))
  colnames(fmelted)[grep("variable|value",names(fmelted))] <- c("Qshorts","rating")
  
  fmelted$Indicators <- gsub("[[:digit:]]","",fmelted$Qshorts)
  fmelted$Indicators <- factor(fmelted$Indicators)
  #fmelted$rating <- as.integer(fmelted$rating)
  return(fmelted)
}
Qshort_columns <- function(f){ # select only Qshort column indices
  return (which(names(f)%in% question_short_names())) 
}
sceq_indicator_cols<- function(sceq){
  indicators <- c("Assessment","Course.Content","Environment","Faculty.Evaluation","Practical",
                  "Quality.Delivery","Resources","Student.Contribution","Tutorial")
  return (which(names(sceq) %in% indicators))
}
sceq_id_cols <- function(sceq){
  return (setdiff(1:ncol(sceq),sceq_indicator_cols(sceq)))
}
id_columns <- function(f){ #select indices of  factors other than Qshorts
  return (setdiff(1:ncol(f),Qshort_columns(f))) 
}
relevel_all_with_label_names <- function(f){
  
  label_names = c("Strongly Disagree","Disagree","Somewhat Agree","Agree","Strongly Agree")
  w = Qshort_columns(f)
  wo = id_columns(f)
  
  #w=which(names(f)%in% question_short_names()) # select only short questions columns
  #wo= setdiff(1:ncol(f),w) #select columns other than questions
  
  for (i in w) f[,i] <- as.integer(f[,i])
  f[f==1] <- "Strongly Disagree"
  f[f==2] <- "Disagree"
  f[f==3] <- "Somewhat Agree"
  f[f==4] <- "Agree"
  f[f==5] <- "Strongly Agree"
  
  
  for (i in w) f[,i] <- factor(f[,i],levels=label_names) # label likert questions with long names
  for (i in wo) f[,i] <- factor(f[,i]) # other than questions coloumns 
  
  return (f)
  
}

make_bw_plot<- function(fagregated, By,titl){
  d = fagregated;
  if (By == 'Faculties' )
    d = subset_by(d, 'Jamshoro',levels(d$Faculties), levels(d$Levels))
    
  if (By == 'Campuses')
    d = subset_by(d, levels(d$Campuses)[!levels(d$Campuses)=='Jamshoro'],
                   levels(d$Faculties),levels(d$Levels))
    
  

    if (nrow(d)==0) break; # if data is missing break
  
  if (By == 'Faculties')
    print(bwplot(data=d, 
           Means ~ reorder (d$Indicators,d$Means) | d$Faculties,
           scales= list(x=list(rot=90)),
           main=paste('Boxplots of', titl,'\n ',
                      'in various Faculties, Campus: Jamshoro'))  )
  
  
  
  if (By == 'Campuses')
    print(bwplot(data=d, 
           Means ~ reorder (d$Indicators,d$Means) | d$Campuses,
           scales= list(x=list(rot=90)),
           main=paste('Boxplots of', titl,'\n  ',
                      'in various Campuses')))    
  
    if (By == 'Programs')
    for (lev in levels(d$Levels)) # Bachelors, Masters, M.Phil, PhD
    for (faculty in levels(d$Faculties))
    {
      d2 = subset_by(d, 'Jamshoro',faculty,lev)
      
      #refactor all factor columns except the numeric column
      for (n in names(d2[sapply(d2,class) %in% 'factor'])) 
        d2[,colnames(d2)==n] = factor(d2[,colnames(d2)==n])
      
      #if (nrow(d2)==0) continue; # if data is missing break
      if (!is.data.frame(d2)) next; 
      
      print(barchart(data=d2, 
           Means ~ reorder (d2$Indicators,d2$Means) | d2$Programs,
           scales= list(x=list(rot=90)),
           main = paste("Barchart of", titl,"\n, Main Campus Faculty:",faculty,"  Programs of :",lev),
           xlab="various Indicators"))
    }
  
}

add_faculty_column <- function(f) {
  
  l = ncol(f)
  fac <- read.csv("../data/df (Virtual) fac&Progs.csv",header=T)
  names(fac)[2]<- "Programs"
  f %>%
    left_join(fac, by = "Programs") -> f
  
  return (f)
  
  
}

make_likert_plot <- function(f_with_labels,titl){
  
  lik <- likert(f_with_labels[,names(f_with_labels) %in% question_short_names()])
  likert.heat.plot(lik,centered=F,ordered=T,plot.percent.neutral=F)+
    ggtitle(paste("Heat Plot of", titl, " @University of Sindh\n Data from Google Forms"))
}

make_indicator_label_names <- function(f){
  
  questions_txt_prefixes <- c("ASSE", "CCO", "ENV", "FACEV", "PRAC", "QD", "RES", "STC", "TUT")
  indicators <- c("Assessment","Course.Content","Environment","Faculty.Evaluation","Practical",
                  "Quality.Delivery","Resources","Student.Contribution","Tutorial")
  lookup <- data.frame(questions_txt_prefixes,indicators)
  names(lookup)<- c("Indicators","Indicator_labels")
  f %>% 
    left_join(lookup, by = "Indicators") -> f
  return (f)
}

question_short_names <- function(){
  
  question_shortnames <- read.csv("../data/df (Virtual) Questionnaires.csv", header=T)
  return (as.character(question_shortnames$Qshort[1:44])) #shorten col names
  
}
#subsets and refactoring levels based on subset data
subset_by<- function(d, campuses, faculties, levs){
  
     
    
    d = subset(d, subset = (d$Campuses %in% campuses))
    d=subset(d,subset=(d$Levels %in% levs)) 
    d=subset(d,subset=(d$Faculties %in% faculties))
    
    if (nrow(d)==0) return (0);
    #refactor all factor columns except the numeric column
    for (n in names(d[sapply(d,class) %in% 'factor'])) 
      d[,colnames(d)==n] = factor(d[,colnames(d)==n])
    
    return(d)
    
}

make_correlogram <- function(fmelted, By,titl){
  
  d = fmelted;
  if (By == 'Faculties' ){
    d = subset_by(d, campuses= 'Jamshoro', levels(d$Faculties ), levels(d$Levels))
    d_casted=dcast(data=d,formula=d$Faculties~ d$Indicators,value.var = 'rating',fun.aggregate = mean)
    corr = round(cor(d_casted[,-1]),2)
    heading = paste(titl,"\nThe Correlogram Matrix of Indicators of various Faculties in Jamshoro")
    
  }  
  if (By == 'Campuses'){
    d = subset_by(d, levels(d$Campuses)[!levels(d$Campuses)=='Jamshoro'],
                  levels(d$Faculties),levels(d$Levels))
    
    d_casted=dcast(data=d,formula=d$Campuses~ d$Indicators,value.var='rating',fun.aggregate = mean)  
    corr = round(cor(d_casted[,-1]),2)
    heading = paste(titl,"\nThe Correlogram Matrix of Indicators of additional Campuses"  )
  }
  
  if (nrow(d)==0) break; # if data is missing break
  
  if(By == "Faculties" || By == "Campuses")
  
    {
    chart.Correlation(d_casted[,-1])
    
    print(ggcorrplot(corr, hc.order = TRUE, 
                   type = "lower", 
                   lab = TRUE, 
                   lab_size = 3, 
                   method="circle", 
                   colors = c("tomato2", "white", "springgreen3"), 
                   title= heading, 
                   ggtheme=theme_bw))
  }
  
  if (By == 'Programs')
    for (lev in levels(d$Levels)) # Bachelors, Masters, M.Phil, PhD
      for (faculty in levels(d$Faculties))
      {
        d2 = subset_by(d, 'Jamshoro',faculty,lev)
        if (nrow(d2)==1 || d2 ==0) next; # if data is missing next
        d_casted=dcast(data=d2,formula=d2$Programs~ d2$Indicators,value.var='rating',fun.aggregate = mean)  
        if(nrow(d_casted)==1) next; # 1 row is meaningless for cor and throws error
        if(sum(sapply(d_casted[,-1],sd)==0) >= 1) next; # is sd==0 for any row cor is divide by zero error
        corr = round(cor(d_casted[,-1],use = "complete.obs"),2)
        print(ggcorrplot(corr, hc.order = TRUE, 
                         type = "lower", 
                         lab = TRUE, 
                         lab_size = 3, 
                         method="circle", 
                         colors = c("tomato2", "white", "springgreen3"), 
                         title= paste(titl," -Correlgram of the Faculty of ",faculty,"\n Level of Education: ",lev), 
                         ggtheme=theme_bw))
      }  
  
   
}
# working with raw csv data to convert to columns with mean indicators
SCEQ_get_rowMeans <- function(f){
  
  questions_txt_prefixes <- c("ASSE", "CCO", "ENV", "FACEV", "PRAC", "QD", "RES", "STC", "TUT")
  indicators <- c("Assessment","Course.Content","Environment","Faculty.Evaluation","Practical",
                  "Quality.Delivery","Resources","Student.Contribution","Tutorial")
  f <- as.data.frame(f)
  r = NULL
  for (i in 1:length(questions_txt_prefixes))
  {
    r = cbind(rowMeans(f[,grep(questions_txt_prefixes[i],names(f))]),r)
    
    }
  colnames(r) <- indicators
  f <- cbind(f[,id_columns(f)],r)
  
  return(f)
  
}

sceq_melt <- function(sceq){
  
  sceq_melted <- melt(sceq, id.vars  = sceq_id_cols(sceq))
  colnames(sceq_melted)[grep("variable|value",names(sceq_melted))] <- c("Indicators","Means")
  
  return(sceq_melted)
}


sceq_correlogram <- function(sceq, By){
  
  d = sceq;
  if (By == 'Faculties' ){
    d = subset_by(d, campuses= 'Jamshoro', levels(d$Faculties ), levels(d$Levels))
    corr = round(cor(sceq[,sceq_indicator_cols(sceq)]),2)
    heading = "Students Course Evaluation 2018-19\nThe Correlogram Matrix of Indicators of various Faculties in Jamshoro"
    
  }  
  if (By == 'Campuses'){
    d = subset_by(d, levels(d$Campuses)[!levels(d$Campuses)=='Jamshoro'],
                  levels(d$Faculties),levels(d$Levels))
    corr = round(cor(sceq[,sceq_indicator_cols(sceq)]),2)
    heading = "Students Course Evaluation 2018-19\nThe Correlogram Matrix of Indicators of additional Campuses"  
  }
  
  if (nrow(d)==0) break; # if data is missing break
  
  if(By == "Faculties" || By == "Campuses")
    
  {
    #print(chart.Correlation(corr))
    
    print(ggcorrplot(corr, hc.order = TRUE, 
                     type = "lower", 
                     lab = TRUE, 
                     lab_size = 3, 
                     method="circle", 
                     colors = c("tomato2", "white", "springgreen3"), 
                     title= heading, 
                     ggtheme=theme_bw,
                     title= "Correlogram Plot of Indicators in various Faculties"))
  }
  if (By == 'Programs')
    for (lev in levels(d$Levels)) # Bachelors, Masters, M.Phil, PhD
      for (faculty in levels(d$Faculties))
      {
        d2 = subset_by(d, 'Jamshoro',faculty,lev)
        if (nrow(d2)==1 || d2 ==0) next; # if data is missing next
        if (!is.data.frame(d2)) next;
        corr = round(cor(d2[,sceq_indicator_cols(d2)],use = "complete.obs"),2)
        #print(chart.Correlation(corr))
        #title(heading)
        print(ggcorrplot(corr, hc.order = TRUE, 
                         type = "lower", 
                         lab = TRUE, 
                         lab_size = 3, 
                         method="circle", 
                         colors = c("tomato2", "white", "springgreen3"), 
                         title= paste("SCEQ -Correlgram of the Faculty of ",faculty,"\n Level of Education: ",lev), 
                         ggtheme=theme_bw))
      }  
  
  
}
#under construction
make_pie3D <- function(percent1,titl){
  
  w= which(sapply(percent1,class) == 'factor')
  u = unique(percent1[,w])
  percent1[]
  
  percent1$pasted <- paste(percent1$Campuses,percent1$Faculties,percent1$Levels,percent1$Programs)
  u$pasted <- paste(u$Campuses,u$Faculties,u$Levels,u$Programs)
  
  for (i in 1:nrow(u)){
    data <- subset(percent1[,1:6], percent1$pasted == u$pasted[i])
    
          if (!is.data.frame(data) || nrow(data)==0) return ;
          a=round(prop.table(data$Counts)*100,1)
          cols = c('seagreen','Green','Yellow','Red','darkred')[5:1]
          lab= c('Strongly Disagree','Disagree','Somewhat Agree','Agree','Strongly Agree')
          m=paste(titl,'\nCampus:',u[i,]$Campuses,' Faculty:',u[i,]$Faculties,'\nLevel:',u[i,]$Levels,' Program Name:',u[i,]$Programs)
          filname = paste0(titl,u[i,]$Campuses,'_',u[i,]$Faculties,'_',u[i,]$Levels,'_',u[i,]$Programs)
          #filname = abbreviate(filname,30)
          jpeg(filename = paste0('../output/df (Virtual) report/df (Virtual) pie3D Faculty Eval/',filname,'.jpg'),width = 600)
            pie3D(a,col=cols,labels = lab,main=m,labelcex = 1)
          dev.off()
  }
}