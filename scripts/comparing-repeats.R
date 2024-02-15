#Comparing the repeats

############################
#SET UP
############################

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

############################
#LOAD DATA
############################

#get data
cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.tsv"))
names(cells)

#fix repliactes, to factors
unique(cells$replicate)
cells$replicate<-as.factor(cells$replicate)

############################
#PLOT
############################


#look at replicate differences for:
#sphericity
#mean.thickness
#radius
#area
#width
#length
#displacement
#instantaneous.velocity

#object for all plots
all.plots<-list()

#sphericity
all.plots[[1]]<-ggplot(cells, aes(x=replicate,y=sphericity))+
    geom_violin()+
    stat_compare_means()+
    facet_wrap(~clone)

#mean.thickness    
all.plots[[2]]<-ggplot(cells, aes(x=replicate,y=mean.thickness))+
    geom_violin()+
    stat_compare_means()+
    facet_wrap(~clone)

#radius
all.plots[[3]]<-ggplot(cells, aes(x=replicate,y=radius))+
    geom_violin()+
    stat_compare_means()+
    facet_wrap(~clone)

#length
all.plots[[4]]<-ggplot(cells, aes(x=replicate,y=length))+
    geom_violin()+
    facet_wrap(~clone)+
    stat_compare_means()

#width
all.plots[[5]]<-ggplot(cells, aes(x=replicate,y=width))+
    geom_violin()+
    facet_wrap(~clone)+
    stat_compare_means()

#displacement
all.plots[[6]]<-ggplot(cells, aes(x=replicate,y=displacement))+
    geom_violin()+
    facet_wrap(~clone)+
    stat_compare_means()

#instantaneous.velocity
all.plots[[7]]<-ggplot(cells, aes(x=replicate,y=instantaneous.velocity))+
    geom_violin()+
    facet_wrap(~clone)+
    stat_compare_means()
#volume
all.plots[[8]]<-ggplot(cells, aes(x=replicate,y=volume))+
    geom_violin()+
    facet_wrap(~clone)+
    stat_compare_means()

ggarrange(plotlist=all.plots,ncol=2,nrow=4)

