##Soda Rarity

#Set Working directory
setwd('D:/Charlie/Videos/Vending Machines/Data')

#Libraries
library(tidyverse)
library(readxl)

#Importing Data
Soda <- read_excel("D:/Charlie/Videos/Vending Machines/Data/MysSoda.xlsx")

##Let's get an idea of diet and non-soda frequency
View(table(Soda$Soda))
View(table(Soda$Diet))




#Creating Frequency Tables of Categorical Variables, turning them into dataframes, and
#prepping them to be merged
ProductCnt<-as.data.frame(table(Soda$Product)) %>% 
  rename(Product=Var1) %>% 
  rename(Pfreq=Freq)
BrandCnt<-as.data.frame(table(Soda$Brand)) %>% 
  rename(Brand=Var1) %>% 
  rename(Bfreq=Freq)
FlavorCnt<-as.data.frame(table(Soda$Flavor)) %>% 
  rename(Flavor=Var1) %>% 
  rename(Ffreq=Freq)
CompanyCnt<-as.data.frame(table(Soda$Company)) %>% 
  rename(Company=Var1) %>% 
  rename(Cfreq=Freq)



#Creating a dataframe that lists each distinct soda once
DSoda<-distinct(Soda,Product,.keep_all=TRUE) %>% 
  select( -c('Button'))



#Merging in frequencies
DSodaM<- merge(x=DSoda,y=ProductCnt,x.all = TRUE) %>% 
  merge(x=.,y=BrandCnt,x.all=TRUE) %>%
  merge(x=.,y=FlavorCnt,x.all=TRUE) %>% 
  merge(x=.,y=CompanyCnt,x.all=TRUE) %>%
  #Creating Rarity by dividing into 5 quantiles
  mutate(Rarity=findInterval(Pfreq,
                               quantile(Pfreq, probs = 1:5/5))) %>% 
  #Recoding them into rarities, combined 4th and 5th quantiles
  mutate(Rarity=recode(Rarity, 
                       '1'= 'Ultra Rare','2'='Rare','3'='Uncommon','4'='Common','5'='Common')) %>%
  #Creating a 'weighted rarity' score.This attempts to draw more distinction between  how rare a soda is
  #and correct for for how frequently the flavor and brand occur.
  #This is mostly subjective, but was tweaked with the feedback from others interested in the machine
  #I think it does a good job of adjusting rarity, but isn't perfect.
  #Lower scores mean the soda is rarer. Another name for this score is Sud-ometer
  mutate(wRarity= Pfreq + .5*Ffreq + .33*Bfreq + .01*Cfreq) %>%
  #Determining quantiles for wRarity, and recoding them into categories
  mutate(wRaritycat=findInterval(wRarity,
                             quantile(wRarity, probs = 1:4/4))) %>% 
  mutate(wRaritycat=recode(as.character(wRaritycat), 
                        '0'= 'Ultra Rare','1'='Rare','2'='Uncommon','3'='Common','4'='Common'))



##Making some histograms to make sense of some things
ggplot(data=DSodaM, aes(DSodaM$Pfreq))+
  geom_histogram(binwidth = 2, fill="#65ff00", color="#F57C13")+
  labs(x='Frequency of Product')+
  theme(panel.background = element_rect(fill = "#F57C13", color = "pink"))


  ggplot(data=DSodaM, aes(DSodaM$wRarity))+
    geom_histogram(bins = 15)
  



