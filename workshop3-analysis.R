#BIO00066I workshop 3 data analysis from 
#https://djeffares.github.io/BIO66I/workshop3.html


#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the corr library: this is for examining correlations between many metrics
library(corrr)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

#starting with 

#load the manual tracking data
track <-read_tsv(url("https://djeffares.github.io/BIO66I/A1-and-B2-tracking.data.2025-02-27.tsv"))

#compare mean.speed between cell lines
ggplot(track, aes(x=cell.line,y=mean.speed))+
  geom_boxplot()+
  stat_compare_means()

#examine whether track.length and mean.speed are correlated
cor.test(track$track.length,track$mean.speed,method="spearman")

#calculate all pairwise correlations
track.correlations <- 
  track |>
  correlate(method="spearman")

#see what we have
head(track.correlations)

#Adjust the name of the first column to "Variable1"
names(track.correlations)[1]="Variable1"

#simplify the data with pivot_longer
track.correlations.pivot <- 
  track.correlations |> 
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "corr.coeff")

#examine what we have
head(track.correlations.pivot)



#Adjust the name of the first column to "Variable1"
names(track.correlations)[1]="Variable1"

#simplify the data with pivot_longer
track.correlations.pivot <- 
  track.correlations |> 
  select(-Variable1) |>
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "corr.coeff")

#examine what we have
head(track.correlations.pivot)

#plot data in the track.correlations.pivot table
#using geom_tile (for coloured boxes) and geom_text (to show the correlation coefficient values) 
track.correlations.pivot |>
  filter(Variable1 != 'LID') |>
  filter(Variable2 != 'LID') |>
  filter(Variable1 != 'TID') |>
  filter(Variable2 != 'TID') |>
  ggplot(aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


