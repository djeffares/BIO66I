#2024-01-16 looking at Manual Cell Tracking data (mtrackJ)

##############################
#SET UP
##############################


#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)


##############################
#DATA CLEAN UP
##############################

#load data:
A1<-read_csv('/Users/dj757/gd/modules/BIO00066I/raw-data/A1-points-python-output.csv')
view(A1)
#not good column names, fixed in excel

#this is better:
A1<-read_csv('/Users/dj757/gd/modules/BIO00066I/data/A1-points-python-output-improved-names-2024-01-16.csv',skip=1)
view(A1)
names(A1)

#load B2 and fix names
B2<-read_csv('/Users/dj757/gd/modules/BIO00066I/raw-data/B2-points-python-output.csv',col_names=F,skip=1)
names(B2)<-names(A1)
view(B2)

#columns are:
#CID: cluster id
#TID:tracking id 

#append the A1 and B2
A1$cell.line = "A1"
B2$cell.line = "B2"
tracking<-bind_rows(A1, B2)

#check the data is OK, and save it:
names(tracking)
#output all tracking data for A1 and B2
write_tsv(tracking, file="/Users/dj757/gd/modules/BIO00066I/data/A1-and-B2-tracking.data.tsv")


##############################
#DATA ANALYSIS
##############################

#check some metrics that might differ between A1 and B2:
#: very different
speed.plot<-tracking |> 
    group_by(cell.line, CID) |> 
    summarise(metric = median(mean.speed)) |>
    ggplot(aes(x=cell.line, y = metric))+
    geom_violin()+
    stat_compare_means()
speed.plot

#track.length: very different
track.length.plot<-tracking |> 
    group_by(cell.line, CID) |> 
    summarise(metric = median(track.length)) |>
    ggplot(aes(x=cell.line, y = metric))+
    geom_violin()+
    stat_compare_means()
track.length.plot

#meandering.index: not signif different
#but A1 does have a much large range of values
meandering.index.plot<-tracking |> 
    group_by(cell.line, CID) |> 
    summarise(metric = median(meandering.index)) |>
    ggplot(aes(x=cell.line, y = metric))+
    geom_violin()+
    stat_compare_means()
meandering.index.plot


