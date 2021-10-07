#----------------------
#Load libraries
#----------------------

library(data.table)
library(funModeling)
library(plyr)
library(dplyr)
library(ggplot2)
library(reshape2)
library(imputeTS)
library(tsoutliers)
library(lubridate)
library(zoo)
library(xts)
library(forecast)
library(timeSeries)
library(stringi)
library(tidyverse)
library(tibbletime)
library(anomalize)
library(tcltk)
library(readxl)
library(scales)
library(grid)


#----------------------
#Data location
#----------------------

path <- "/01_Data/"
setwd(path) #set directory to declared path

ons.data.filename.anxiety <- "wellbeing_ons_anxiety.csv"
ons.data.filename.happy <- "wellbeing_ons_happy.csv"
ons.data.filename.satisfaction <- "wellbeing_ons_satisfaction.csv"
ons.data.filename.worthwhile <- "wellbeing_ons_worthwhile.csv"

ref.data.filename <- "EU-referendum-result-data.csv"


#----------------------
#Import data
#----------------------
data.ons.anxiety  <- read.csv(ons.data.filename.anxiety,
                          header = TRUE,
                          skip = 0,
                          na.strings ="NA",
                          stringsAsFactors = FALSE)

data.ons.happy  <- read.csv(ons.data.filename.happy,
                              header = TRUE,
                              skip = 0,
                              na.strings ="NA",
                              stringsAsFactors = FALSE)

data.ons.satisfaction  <- read.csv(ons.data.filename.satisfaction,
                              header = TRUE,
                              skip = 0,
                              na.strings ="NA",
                              stringsAsFactors = FALSE)

data.ons.worthwhile  <- read.csv(ons.data.filename.worthwhile,
                              header = TRUE,
                              skip = 0,
                              na.strings ="NA",
                              stringsAsFactors = FALSE)


data.ref  <- read.csv(ref.data.filename,
                      header = TRUE,
                      skip = 0,
                      na.strings ="NA",
                      stringsAsFactors = FALSE)

#----------------------
#Sort data
#----------------------

data.ons.anxiety$type <- "Anxiety Score"
data.ons.happy$type <- "Happines Score"
data.ons.satisfaction$type <- "Satisfaction Score"
data.ons.worthwhile$type <- "Worthwhile Score"


data.ons.all <- rbind(data.ons.anxiety,data.ons.happy,data.ons.satisfaction,data.ons.worthwhile)

data.ons.all$Area <- data.ons.all$Area.Names

merged.data <- merge(data.ref,data.ons.all, by = 'Area')

test <- gather(merged.data,key = "Year",value = "Value",23:32)


test$Brexit.result <- ifelse(test$Pct_Remain > 50 , "Remain", "Leave")
 
test$Value <- as.numeric(test$Value)
test$brexit.diff <- test$Pct_Remain - test$Pct_Leave
test$brexit.count.diff <- ((test$Remain - test$Leave)/test$Electorate) * 100

test <- test %>%
  mutate(Year.clean = case_when(Year == "X2011.12" ~ 2012,
                                Year == "X2012.13" ~ 2013,
                                Year == "X2013.14" ~ 2014,
                                Year == "X2014.15" ~ 2015,
                                Year == "X2015.16" ~ 2016,
                                Year == "X2016.17" ~ 2017,
                                Year == "X2017.18" ~ 2018,
                                Year == "X2018.19" ~ 2019))



#Group differences 
test <- test %>% mutate(DiffGroup = case_when(brexit.diff > -1 & brexit.count.diff < 1 ~ 'small margin',
                                              brexit.diff < -1 ~ 'large margin',
                                              brexit.diff > 1 ~ 'large margin'))


min.diff <- 0 - (15/2)
max.diff <- 0 + (15/2)

test2 <- test %>% group_by(type,Year,Brexit.result,DiffGroup) %>% summarise(Mean = mean(Value,na.rm=TRUE), 
                                                                  Min = min(Value,na.rm=TRUE),
                                                                  Max = max(Value,na.rm=TRUE),
                                                                  sd = sd(Value,na.rm=TRUE)
                                                                  )

test2 <- test2 %>%
  mutate(Year.clean = case_when(Year == "X2011.12" ~ 2012,
                                Year == "X2012.13" ~ 2013,
                                Year == "X2013.14" ~ 2014,
                                Year == "X2014.15" ~ 2015,
                                Year == "X2015.16" ~ 2016,
                                Year == "X2016.17" ~ 2017,
                                Year == "X2017.18" ~ 2018,
                                Year == "X2018.19" ~ 2019))



library(hrbrthemes)
library(gcookbook)
library(tidyverse)
library("viridis") 
  
pp <- ggplot(data = test2, aes(x=Year.clean, y=Mean, colour = Brexit.result)) +
  geom_point() +
  geom_line(aes(group = interaction(DiffGroup))) +
  geom_vline(xintercept = 2016,alpha = 0.75, linetype = "longdash", colour = 'red') +
  labs(colour = "Area Brexit Result",
       title = "Personal Well-being Estimates in Leave/Remain Areas",
       subtitle = "ONS well-being data linked to referendum results",
       caption="Data from @ONS") +
 # geom_text(aes(x=20160),label="Referendum", colour="blue", angle=90, text=element_text(size=11)) +
  ylab("Mean Score of the grouped areas (out of 10)") +
  xlab("") +
  theme_ft_rc() +
  theme(plot.title = element_text(size=12)) +
  theme(plot.subtitle = element_text(size=9)) +
  theme(axis.title.y = element_text(vjust=5)) +
  theme(axis.title.y = element_text(hjust=0.5)) +
 # theme_modern_rc()+
  facet_wrap(~type, scales = "free_y") 


# plotting with margin of vote
ggplot(data = test2, aes(x=Year.clean, y=Mean, colour = interaction(Brexit.result,DiffGroup), group = interaction(Brexit.result,DiffGroup))) +
  geom_point() +
 # geom_line() +
  geom_vline(xintercept = 2016,alpha = 0.75, linetype = "longdash", colour = 'red') +
  
  geom_smooth(method = lm, se = FALSE) +
  labs(colour = "Area Brexit Result",
       title = "Personal Well-being Estimates in Leave/Remain Areas",
       subtitle = "ONS well-being data linked to referendum results",
       caption="Data from @ONS") +
  # geom_text(aes(x=20160),label="Referendum", colour="blue", angle=90, text=element_text(size=11)) +
  ylab("Mean Score of the grouped areas (out of 10)") +
  xlab("") +
  theme_ft_rc() +
  theme(plot.title = element_text(size=12)) +
  theme(plot.subtitle = element_text(size=9)) +
  theme(axis.title.y = element_text(vjust=5)) +
  theme(axis.title.y = element_text(hjust=0.5)) +
  # theme_modern_rc()+
  facet_wrap(~type, scales = "free_y") 





tiff("test.tiff", units="in", width=5, height=5, res=400)
# insert ggplot code
dev.off()

png("heatmap.png", width = 200, height = 160, units='mm', res = 350)
pp
dev.off()
  



#extract year to make plot easier
hold <- strsplit(test2$Year,split="X")


test2$Year.numeric <- 
  test <- strsplit(test2$Year,split="X")[2]
test2 <- lapply(test, `[[`, 2)[[1]]
test3 <- strsplit(test2,split=".")[1]
#+
 # geom_errorbar(aes(ymin=Min, ymax=Max), width=.2,
 #               position=position_dodge(.9)) 
  

#Can we add difference in vote? 
# shade by difference? 
  

ggplot(data = test,aes(x = Year.clean, y=Value,  colour = interaction(Brexit.result,DiffGroup), group = interaction(Brexit.result,DiffGroup))) +
 #geom_jitter(alpha = 0.1) +
  theme_ft_rc() +
  geom_smooth(method = loess, se = FALSE) +
 # scale_colour_gradient2() +
  geom_vline(xintercept = 2016,alpha = 0.75, linetype = "longdash", colour = 'red') +
  facet_wrap(~type, scales = "free_y") 
