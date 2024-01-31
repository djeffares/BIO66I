#BIO00066I-workshop3-cell-movement-metrics.R
#2024-01-20

############################
#SET UP
############################

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the corrr library
library(corrr)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

############################
#LOAD DATA
############################

#get data
#NB: this data is filtered to remove the top end and low end of area, dry.mass and volume
#as described in filtering-FFT-data-2024-01-19.R
cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv"))

#this time we will also load the unfiltered FFT data:
unfilt.cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.tsv"))

#make replicates factors
cells$replicate<-as.factor(cells$replicate)
unfilt.cells$replicate<-as.factor(unfilt.cells$replicate)


########################################################
#RECAP: UNDERSTANDING WHAT WE HAVE
########################################################

#last time we looked at our data, like this:
#look at the data, like an excel table:
view(cells)

#what columns do we have?
names(cells)

#and we made some plots, like this:
#let's try the same plot, with two different metrics:
#sphericity and area
cells |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(x=sphericity,y=dry.mass,colour=clone))+
    geom_point(alpha=0.1)+
    facet_wrap(~replicate)+
    theme_classic()

########################################################
#REMEBER!
########################################################

#save your script **often** using the hot keys

#save your data like so:
save.image("data/BIO00066I-workshop3-cell-movement-metrics.Rda")

########################################################
#MOVEMENT DATA
########################################################

#our tibble of data also contains information about cell movement
#lets see what we have:
names(cells)

#cell movement data is:
#displacement, track.length, 
#instantaneous.velocity, instantaneous.velocity.x, instantaneous.velocity.y,
#position.x, position.y, pixel.position.x, pixel.position.y

##Amanda:
#Can you please write definitions of this data in this script?

cell.move.data<-select(cells,
        clone,
        replicate,
        displacement, 
        track.length, 
        instantaneous.velocity,
        instantaneous.velocity.x,
        instantaneous.velocity.y,
        position.x, 
        position.y, 
        pixel.position.x, 
        pixel.position.y
)
names(cell.move.data)
summary(cell.move.data)

#lets save our data
save.image("data/BIO00066I-workshop3-cell-movement-metrics.Rda")

#first a quick look at some of the metrics
#instantaneous.velocity
ggplot(cell.move.data,aes(x=clone,y=instantaneous.velocity,colour=clone))+
    geom_violin(alpha=0.5)+
    facet_wrap(~replicate)+
    stat_compare_means()

#most cells don't move very fast

#displacement
ggplot(cell.move.data,aes(x=clone,y=displacement,colour=clone))+
    geom_violin(alpha=0.5)+
    facet_wrap(~replicate)+
    stat_compare_means()

#most cells don't go very far

#track.length
ggplot(cell.move.data,aes(x=clone,y=track.length,colour=clone))+
    geom_violin(alpha=0.5)+
    facet_grid(rows = vars(clone))+
    facet_grid(cols = vars(replicate))+
    stat_compare_means()

#lets see which of these are correlated
#the data are certainly **not** normally distributed, so we will use  method="spearman"

#generate all the correlations
cell.move.data.cor <- cell.move.data |>
    correlate(method="spearman")

#this generates a new object, let's look at it
view(cell.move.data.cor)

#tidy up the data
cell.move.data.cor <- rearrange(cell.move.data.cor, absolute = FALSE)
cell.move.data.cor <- shave(cell.move.data.cor)

#now we have this simpler correlatino data:
view(cell.move.data.cor)

#and a plot is very tidy
rplot(cell.move.data.cor)

#what does this show??

#also, lets output this plot as a jpeg
cell.move.data.cor.plot<-rplot(cell.move.data.cor)+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#this bit: theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#rotates the axis text on the x axis
ggsave(cell.move.data.cor.plot,file="plots/movement.data.cor.plot.jpeg")

##Amanda:
#the p-values in this analysis shows that there **is** plenty of discriminating power
#between clones in the movement data!
#this is good

########################################################
#PCA: TRIAL
########################################################
##Amanda
#I tried to separate the clones by principal componets clustering
#It was not very convincing
#See pca-trial-2024-01-20.R and arranged.pca.plot.2024.01.20.jpeg

########################################################
#USING MANUALLY-DETERMINED CELL MOVEMENT DATA
########################################################

#We do not trust the automatically-measured movement data in the cells object
#that we loaded from
#[this link](https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv)

##Amanda:
#why don't we trust it?

########################################################
#LOADING AND FIRST DATA EXPLORATION
########################################################

##LOAD DATA
#So we have manually curated some cell tracking data 
#You can obtain this data here:
track <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/A1-and-B2-tracking.data.tsv"))

##EXPLORE DATA

#student exercise:
#use names(), view(), nrow(), ncol(), dim() and glimpse() to explore this data
names(track)
dim(track)
summary(track)
view(track)

#note that here, we only have two cell types
unique(track$cell.line)

##PLOT DATA

#looks like these columns would be good to plot:
#track.length, meandering.index, euclidean.distance, mean.speed, track.duration

#track.length: a LITTLE DIFFERENT
ggplot(track, aes(x=cell.line,y=track.length))+
    geom_violin()+
    facet_wrap(~track.present.at.start.or.end)+
    stat_compare_means()
    
#meandering.index: NOT SIGNIF DIFF
ggplot(track, aes(x=cell.line,y=meandering.index))+
    geom_violin()+
    facet_wrap(~track.present.at.start.or.end)+
    stat_compare_means()

#euclidean.distance: VERY DIFFERENT
ggplot(track, aes(x=cell.line,y=euclidean.distance))+
    geom_violin()+
    facet_wrap(~track.present.at.start.or.end)+
    stat_compare_means()

#mean.speed: VERY DIFFERENT
ggplot(track, aes(x=cell.line,y=mean.speed))+
    geom_violin()+
    facet_wrap(~track.present.at.start.or.end)+
    stat_compare_means()

#track.duration: NOT SIGNIF DIFF
ggplot(track, aes(x=cell.line,y=track.duration))+
    geom_violin()+
    facet_wrap(~track.present.at.start.or.end)+
    stat_compare_means()

##Amanda
#here is use track.never.divides as a sort of 'replicate'
#is this a good idea?
#we could also use: CID or TID 

#mean.speed: with CID
#we try a new theme this time!
ggplot(track, aes(x=cell.line,y=mean.speed))+
    geom_violin()+
    facet_wrap(~CID)+
    stat_compare_means()+
    theme_pubclean()

#mean.speed: with TID
#we try a new theme this time too
track |>
    filter(TID < 13) |>
    ggplot(aes(x=cell.line,y=mean.speed))+
    geom_violin()+
    facet_wrap(~TID)+
    stat_compare_means()+
    theme_gray()


#conclusion
#We can see two strong differences between the movement of these clones:
#euclidean.distance: VERY DIFFERENT
wilcox.test(euclidean.distance ~ cell.line, data = track)

#mean.speed: VERY DIFFERENT
wilcox.test(mean.speed ~ cell.line, data = track)

#these are correlated
cor.test(track$euclidean.distance,track$mean.speed)

#make plot to show this:
plot1<-ggplot(track, aes(x=mean.speed,y=euclidean.distance,colour=cell.line,shape=cell.line))+
    geom_point(size=4)+
    theme_minimal()

#and also show track.length vs track.length
plot2<-ggplot(track, aes(x=mean.speed,y=track.length,colour=cell.line,shape=cell.line))+
    geom_point(size=4)+
    theme_minimal()
both.plots<-ggarrange(plot1,plot2)
ggsave(both.plots,file="plots/track.movement.scatterplots.jpeg")

#these would be a useful way to categorise cell lineages

#student exercise:
#what did we learn about the two cell types, from this workshop
#and also the previous workshop

########################################################
#END
########################################################



