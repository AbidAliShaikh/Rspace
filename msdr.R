#msdr
library(ggplot2)
library(faraway)
data(nepali)

library(dplyr)

nepali <- nepali %>% select (id, sex, wt, ht, age) %>% mutate (id = factor (id),
      sex = factor(sex,levels = c(1,2), labels=c('male','female'))) %>% 
      distinct(id,.keep_all = T) 

ggplot(nepali, aes(ht))+geom_histogram()+theme_tufte()
ggplot(nepali, aes(1,ht))+geom_boxplot()+theme_few()
ggplot(nepali, aes(sex,ht,fill=sex))+geom_boxplot()+theme_solarized()
#library(GGali)
library(gridExtra)
library(ggthemes)
library(dlnm)
data("chicagoNMMAPS")
chic <- chicagoNMMAPS
chic_july <- chic %>% filter (month==7, year == 1995)

########################
chicago_plot <- ggplot(chic, aes(time,death))+
                xlab('Day in July')+
                ylab('All cause deaths')+
                ylim(0,450)
chicago_plot+geom_area(fill='black')+theme_excel()
chicago_plot+geom_line()+theme_tufte()

ggplot(filter(worldcup, Position=="Forward"),aes(x=Shots, y=Passes))+ 
  geom_point(size=1.5)+ geom_smooth()+theme_few()
p1<-ggplot(worldcup,aes(x=Shots, y=Passes))+ 
  geom_point(size=1.5)+ geom_smooth()+theme_few()
p1 + facet_wrap(~Position  )

#####################
us_map <- map_data("state")

us_map %>%
  ggplot(aes(long,lat,group=group))+geom_path()

us_map %>%
  ggplot(aes(long,lat,group=group))+
  geom_polygon(fill='steelblue',color='black')+
  theme_void()

us_map %>%
  ggplot(aes(long,lat,group=group))+
  geom_polygon(fill='steelblue',color='black')+
  theme_void()

votes.repub %>%
  tbl_df() %>%
  mutate (state = row.names(votes.repub), state = tolower(state)) %>%
  right_join(us_map, by = c("state" = "region")) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=`1976`))+
    geom_polygon(color='black')+
  scale_fill_viridis(begin = 1, end = 0)
# The voting data includes Alaska and Hawaii, but the geographic data does not. Therefore, we've used right_join to join the two datasets, so only non-missing values from the us_map geographic data will be kept.


get_map("Pakistan", zoom = 10, 
        source = "stamen", maptype = "toner") %>%
  ggmap()#+ 
  geom_polygon(data = baltimore, aes(x = long, y = lat, group = group),
               color = "navy", fill = "lightblue", alpha = 0.2) + 
  geom_point(data = serial, aes(x = long, y = lat, color = tower)) + 
  theme_void() + 
  scale_color_manual(name = "Cell tower", values = c("black", "red"))

