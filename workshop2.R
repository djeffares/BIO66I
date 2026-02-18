#Data Analysis 2: Cell Biology
#Date 2026-02-13


#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)


#load the ggpubr package for multi-part plots
library(ggpubr)

cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)

#look at the data, like an excel table:
view(cells)

#what are the names of the columns?
names(cells)

#how many rows and columns you we have?
nrow(cells)
ncol(cells)
dim(cells)

#other ways to peek at data:
summary(cells)
glimpse(cells)

#see what we have
names(cells)

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

#save all my stuff
save.image("BIO00066I-workshop2.Rda")

#load all my stuff from last time
load("BIO00066I-workshop2.Rda")

summary.table <- cells |> 
    group_by(clone, replicate) |> 
    summarise(
        volume=median(volume),
        mean.thickness=median(mean.thickness),
        radius=median(radius),
        area=median(area),
        sphericity=median(sphericity),
        length=median(length),
        dry.mass=median(dry.mass),
        length.to.width=median(length.to.width)
)

view(summary.table)

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_boxplot()

names(cells)
ggplot(cells, aes(x = width,colour = replicate)) +
  geom_density()+
  facet_wrap(~clone)

ggplot(cells, aes(x = mean.thickness,y=sphericity,colour = clone)) +
  geom_point()+
  facet_wrap(~clone)
  


#
ggplot(cells, aes(x = mean.thickness,colour = replicate)) +
  geom_density()+
  facet_wrap(~clone)+
  scale_x_log10()+
  geom_vline(xintercept = median(cells$mean.thickness), linetype = "dashed", color = "red")

#Wilcoxon rank sum test
#To test if cloneA and cloneB have statistically different widths
wilcox.test(width ~ clone, data = cells)

ggplot(cells,aes(x=clone,y=width))+
    geom_boxplot()+
    stat_compare_means()

names(cells)

#read in some data from a website
small.tracking.data <-read_tsv(url("https://djeffares.github.io/BIO66I/data/trackingid.small.data.tsv"),
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)

#check what we have
glimpse(small.tracking.data)
summary(small.tracking.data)

#Load from local file for rendering
small.tracking.data <-read_tsv("data/trackingid.small.data.tsv",
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)
glimpse(small.tracking.data)
summary(small.tracking.data)

ggplot(small.tracking.data, aes(x=clone, y=width))+
    geom_boxplot(fill=NA)+
    geom_jitter(width = 0.2,size=3,pch=1)+
    theme_minimal()

names(small.tracking.data)

#test with small.tracking.data
wilcox.test(width ~ clone, data = small.tracking.data)

#test with 'cells' data frame - a *much* larger data set  
wilcox.test(width ~ clone, data = cells)

#plot with the large data
#storing the plot in an object called large.data.plot
large.data.plot <- ggplot(cells, aes(x=clone, y=width))+
    geom_boxplot()+
    geom_jitter(width = 0.2,size=3,pch=1)+
    stat_compare_means()+
    ylim(0,200)+
    ggtitle("large data")

#plot with the small data
#storing the plot in an object called small.data.plot
small.data.plot <- ggplot(small.tracking.data, aes(x=clone, y=width))+
    geom_boxplot()+
    geom_jitter(width = 0.2,size=3,pch=1)+
    stat_compare_means()+
    ylim(0,200)+
    ggtitle("small data")

#create a two panel plot, with large.data.plot and small.data.plot
ggarrange(large.data.plot,small.data.plot)

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_violin()


ggplot(cells,aes(x=clone,y=length,fill=replicate))+
    geom_violin()+
    ggtitle("put your title here!")+
    xlab("my X axis label")+
    ylab("my Y axis label")+
    theme_classic()


#take the mean of each unique tracking id
trackingid.summary.table<-cells |>
    group_by(clone, replicate, tracking.id) |>
    summarise(
        volume=median(volume),
        mean.thickness=median(mean.thickness),
        radius=median(radius),
        area=median(area),
        sphericity=median(sphericity),
        length=median(length),
        width=median(width),
        dry.mass=median(dry.mass),
        length.to.width=median(length.to.width)
    )



#collect a random subset, of 5 rows for each tracking id
trackingid.small.data <- sample_n(trackingid.summary.table, 5)

#check that we have
nrow(trackingid.small.data)
glimpse(trackingid.small.data)

#output trackingid.small.data as a tab-separated value (tsv) file
write_tsv(trackingid.small.data, file ="/Users/dj757/gd/modules/BIO66I/data/trackingid.small.data.tsv")

#how many rows do we have?
nrow(trackingid.small.data)

#what does the data look like?
view(trackingid.small.data)
