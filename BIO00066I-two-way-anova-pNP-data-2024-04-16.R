#BIO00066I TWO WAY ANOVOA with pNP data
#DCJ 2024-04-16

#load libraries
library(tidyverse)
library(readxl)
library(ggpubr)


#Two-Way ANOVA Example in R, the two-way ANOVA test is used to compare 
#the effects of two grouping variables (A and B) on a response variable at the same time.

################################################################################
#This is based on an example from here:
#https://www.r-bloggers.com/2022/05/two-way-anova-example-in-r-quick-guide/#:~:text=Two%2DWay%20ANOVA%20Example%20in%20R%2C%20the%20two%2Dway,varies%20depending%20on%20the%20element.
################################################################################

#In our case, we have *three* grouping variables (day, clone and differentiated), 
#where 'differentiated' indicates whether the cells were grown in differentiated media or not.
#Our response variable is the pNP absorption

#load the data
ap<-read_excel("raw-data/alkaline-phosphatase-activity-assay-Izzy.xlsx", sheet=2)

#rename the 'differentiated' to 'differentiating.media', which described it better
names(ap)[3]<-'differentiating.media'

#set day, clone and differentiated to factors
ap$day <-as.factor(ap$day)
ap$clone <-as.factor(ap$clone)
ap$differentiating.media <-as.factor(ap$differentiating.media)

#this is what we have:
head(ap)

#reformat the data 'longer' so we can use all the repeats
#the cols=!c(day,clone,differentiated) part indicates that
#the day,clone,differentiated columns are not to be lengthened in to the 'absorb' column
#so it is onlyabsorptionrep1 absorptionrep2 absorptionrep3 that are put into the 'absorb' column 
ap.pivot<-ap |> 
  pivot_longer(cols=!c(day,clone,differentiating.media), names_to = "rep", values_to = "absorb")

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
  facet_wrap(~differentiating.media)

#We want to know if absorbance is affected by day, clone and media
#This question can be answered using the R function aov(). 
#Summary of the function The analysis of the variance model is summarised using aov().

#run the THE ANOVA, with an model is not referred to as an additive model.
#here we assume that the factors are unrelated
res.aov <- aov(absorb ~ day + clone + differentiating.media, data = ap.pivot)
#view the results
summary(res.aov)
#we can see that day, clone, and differentiating.media all have a significant effect

#the variable may have interacting effects though, and not be independent (ie: non additive)
#to test this, we replace the plus symbol (+) with an asterisk (*) 
res.aov2 <- aov(absorb ~ day * clone * differentiating.media, data = ap.pivot)
summary(res.aov2)

#we can see that all variables, and all interactions have an effect
