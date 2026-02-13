##################################
#Title: BIO00066I workshop2
#Date: 2025-02-17
#Summary: Analysis of livecyte cell shape data
#2.2 Research questions
#What parameters do we have in the data obtained from the Livecyte machine/method?
#  Which parameters differ between the clones?
#  Which parameters are correlated?
  
####################################################################
#SET UP 
####################################################################

#clear th previous data
rm(list=ls())

#load libraries
library(tidyverse)
library(readxl)
library(ggpubr)

#install.packages("ggpubr")

#how to print your citations in plan text! :)
print(citation("ggpubr"), style = "text")


####################################################################
#PART 1: 
####################################################################

cells <-read_tsv(url("https://djeffares.github.io/BIO66I/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)
# Why we are doing this:
# What the data is
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)

#to find out how many rows the cells data frame has
nrow(cells)

#but this is not correct, because we have multiple measurements for each cell
#if we decide that the unique number of cells is the number of tracking.id's, 
#we can count them this way:
unique.tracking.ids <- unique(cells$tracking.id)
length(unique.tracking.ids)
#or in one step
length(unique(cells$tracking.id))

#but if we define that the unique number of cells is the numbe of lineage.id's
#we can count them with
unique.lineage.ids <- unique(cells$lineage.id)
length(unique.lineage.ids)
#or in one step
length(unique(cells$lineage.id))






#see what we have
names(cells)

#examine the sphericity data
summary(cells$sphericity)

#we get sphericity values > 1, which is not possible
#we will remove these values



summary(cells$sphericity)

#lets save our data
save.image("BIO00066I-workshop3-cell-movement-metrics-2025-03-03.Rda")

#you can load this any time later with: 
load("BIO00066I-workshop3-cell-movement-metrics-2025-03-03.Rda")


#remove all the movement metrics (which are not reliable)
cells <- select(cells, 
                -position.x,
                -position.y, 
                -pixel.position.x, 
                -pixel.position.y,
                -displacement,
                -instantaneous.velocity,
                -instantaneous.velocity.x,
                -instantaneous.velocity.y,
                -track.length
)

#check that our data is simpler
names(cells)


#Wilcoxon rank sum test
#To test if cloneA and cloneB have statistically different widths
wilcox.test(width ~ clone, data = cells)


#filtered the cells df
cloneA <- cells |> 
  filter(clone == "cloneA") 

glimpse(cloneA)

cells |> 
  ggplot(aes(x= clone, y= width))+
  geom_boxplot()

kruskal.test(width ~ replicate, data = cloneA)$p.value

nrow(cloneA)