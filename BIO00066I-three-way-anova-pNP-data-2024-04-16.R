#BIO00066I TWO WAY ANOVOA with pNP data
#DCJ 2024-04-16

#load libraries
library(tidyverse)
library(readxl)
library(ggpubr)


################################################################################
#This is based on a two-way ANOVA example from here:
#https://www.r-bloggers.com/2022/05/two-way-anova-example-in-r-quick-guide/#:~:text=Two%2DWay%20ANOVA%20Example%20in%20R%2C%20the%20two%2Dway,varies%20depending%20on%20the%20element.
################################################################################

#A Three-way ANOVA is used to compare 
#the effects of three grouping variables (A, B, C) on a response variable 

#In our case our three grouping variables are: day, clone and the media (differentiated media or not)
#and our response variable is the pNP absorption

#load the data
ap<-read_excel("raw-data/alkaline-phosphatase-activity-assay-Izzy.xlsx", sheet=2)

#rename the 'differentiated' to 'induced', which described it better
names(ap)[3]<-'induced'

#set day, clone and differentiated to factors
ap$day <-as.factor(ap$day)
ap$clone <-as.factor(ap$clone)
ap$induced <-as.factor(ap$induced)

#this is what we have:
head(ap)

#reformat the data 'longer' so we can use all the repeats
#the cols=!c(day,clone,differentiated) part indicates that
#the day,clone,differentiated columns are not to be lengthened in to the 'absorb' column
#so it is onlyabsorptionrep1 absorptionrep2 absorptionrep3 that are put into the 'absorb' column 
ap.pivot<-ap |> 
  pivot_longer(cols=!c(day,clone,induced), names_to = "rep", values_to = "absorb")

#this is what we have now
head(ap.pivot)

#make a plot, to look at the absorbance by day
#we can see that absorbance goes up each day
#here we are ignoring the media ('differentiated')
ggplot(ap.pivot, aes(x = clone, y = absorb, color = day))+
  geom_boxplot()+
  theme_classic()

#Then use facet_wrap to examine by day *and* by media (differentiated)
#We can see that clone A has much higher values in the differentiation media
ggplot(ap.pivot, aes(x = clone, y = absorb, color = day))+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(~induced)

#We want to know if absorbance is affected by day, clone and media
#This question can be answered using the R function aov(). 
#Summary of the function The analysis of the variance model is summarised using aov().

#run the THE ANOVA, with an model is not referred to as an additive model.
#here we assume that the factors are unrelated
res.aov.additive <- aov(absorb ~ day + clone + induced, data = ap.pivot)
#view the results
summary(res.aov.additive)

#Interpretation: as all p-values are < 0.05, 
#day, clone, and induced all have a significant effects


#the variable may have interacting effects though, and not be independent (ie: non additive)
#to test this, we replace the plus symbol (+) with an asterisk (*) 
res.aov.interactions <- aov(absorb ~ day * clone * induced, data = ap.pivot)
summary(res.aov.interactions)

#Interpretation: as all p-values are < 0.05, 
#all variables, and all interactions have effects

