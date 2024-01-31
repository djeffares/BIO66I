#playing with the data
library(tidyverse)
library(ggpubr)
#setwd("/Users/dj757/gd/modules/BIO00066I/")

#load the data
cells<-read_tsv("data/all-cell-data-FFT.tsv")
cells$replicate<-as.factor(cells$replicate)

view(cells)

#or
cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.tsv"))

#check out some values
summary.table<- cells |> 
    group_by(clone, replicate) |> 
    summarise(
        length=median(length),
        width=median(width),
        width=median(width),
        thick=median(mean.thickness),
        radius=median(radius),
        area=median(area),
        sphericity=median(sphericity),
        dry.mass=median(dry.mass),
        track.length=median(track.length),
        length.to.width=median(length.to.width)
    )

#look at this table, so we can decide what might be good to investgate more
view(summary.table)

#save this table for later
write_tsv(summary.table,file="data/summary.table.tsv")

#some simple plots
#width
plot1<-cells |>
    ggplot(aes(x=clone,y=width,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$width,0.99))+
    stat_compare_means()

#area
plot2<-cells |>
    ggplot(aes(x=clone,y=area,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$area,0.99))+
    stat_compare_means()

#length
plot3<-cells |>
    ggplot(aes(x=clone,y=length,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$length,0.99))+
    stat_compare_means()

#track.length  
plot4<-cells |>
    ggplot(aes(x=clone,y=track.length,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$track.length,0.99))+
    stat_compare_means()

#length.to.width
plot5<-cells |>
    ggplot(aes(x=clone,y=length.to.width,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$length.to.width,0.99))+
    stat_compare_means()

#sphericity
plot6<-cells |>
    ggplot(aes(x=clone,y=sphericity,fill=replicate))+
    geom_boxplot(outlier.shape = NA)+
    ylim(0,quantile(cells$sphericity,0.99))+
    stat_compare_means()

allplot<-ggarrange(
    plot1,plot2,plot3,plot4,plot5,plot6,
    nrow=2,ncol=3
)

ggsave(allplot,
       file="/Users/dj757/gd/modules/BIO00066I/plots/sample.metrics.2023-12-18.jpg")



