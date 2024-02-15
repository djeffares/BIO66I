#BIO00066I-workshop2-replicates-and-size-metrics.R
#2024-01-19

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
#NB: this data is filtered to remove the top end and low end of area, dry.mass and volume
#as described in filtering-FFT-data-2024-01-19.R
cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv"))

#the unfiltered FFT data is:
unfilt.cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.tsv"))

#make replicates factors
cells$replicate<-as.factor(cells$replicate)

############################
#UNDERSTANDING WHAT WE HAVE
############################

#look at the data, like an excel table:
view(cells)

#what columns do we have?
names(cells)

##Amanda: should we move some of the less-useful columns?
#This will keep it simple for the students
#Are the different measurements of cell size & shape *independent* measures?

#how many rows and columns?
nrow(cells)
ncol(cells)
dim(cells)

#other ways to peek at data:
summary(cells)
glimpse(cells)

#make a summary table of *all* the cell shape metrics:
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
        perimeter=median(perimeter),
        length.to.width=median(length.to.width)
)

#look at this summary table:
view(summary.table)

#output it to a file:
write_tsv(summary.table,file="data/summary.table.tsv")

##############################################
#FIRST DATA EXPLORATION WITH PLOTS AND STATS
##############################################

#It looks like many of the cell shape metrics from above might differ between clones
#So let's look deeper

#start with cell width

#with a box and whisker plot
#we do a small 'trick' here, 
#by including fill=replicate we force R to make different plots for each replicate
#plot 1:
ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_boxplot()

#if we exclude fill=replicate, we get this:
#plot 2:
ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_boxplot()

#plot 3:
#we can add a statistical test (wilcox.test)
#with stat_compare_means()
ggplot(cells,aes(x=clone,y=width))+
    geom_boxplot()+
    stat_compare_means()

#now try with  a violin plot
#plot 4:
ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_violin()

#exercise for students:
#replace 'width' in plot 1, with some other metric
#choose one from the summary.table

##Amanda:
#ALL the size/shape metrics are significantly different between clones
#These metrics look *very* different: sphericity, mean.thickness, width
#These look quite different: radius, area, length, dry.mass


##############################################
#RECAP: SAVING PLOTS AND DATA
##############################################

#save your script regularly!
#know the hot key for this!

#to save your work, and all your variables
save.image("data/BIO00066I-workshop2-replicates-and-size-metrics.Rda")

#to load it again:
load("BIO00066I.all.data.Rda")

#to save plots as images
#first, 'push' the plot into an object
width.violin.plot <- ggplot(cells,aes(x=clone,y=width,colour=replicate))+
    geom_violin()

#then use ggsave:
ggsave(width.violin.plot,file="plots/agoodnameforthis-plot.pdf")

#ggsave knows when to make it a jpeg, using like so:
ggsave(width.violin.plot,file="plots/now-this-plot-is-a-jpeg.jpeg")

#this is much better than clicking the export button
#because the output of the plot is *in your script* !


##############################################
#WORKING WITH MORE THAN ONE METRIC
##############################################


#If we want to use different metrics to classify something
#it is important to know that they are not too strongly correlated
#eg: using length and width to classify balloons, probably wont help
#because large balloons will be wider and longer
#using length and colour might be better!


#In R, we can check if two metrics are correlated like this:
cor.test(cells$area, cells$radius)

#and plot them like this:
ggplot(cells,aes(x=area,y=radius))+
    geom_point()


#as we have many shap metrics:
#sphericity, mean.thickness, width, radius, area, length, dry.mass
#it would be tedious to generate *all* possible pairwise correlations
#but in R we don't have to
#we can mak all posible pairwise correlations:

#first install a package
install.packages("corrr")
##Amanda
#we can get IT Support to make sure this is installed ahead of time

#and load the library
library(corrr)

#now we generate all the correlations
#I use method="spearman" because our values are *not* normally distributed
cells.cor <- cells |>
    select(sphericity, mean.thickness, width, radius, area, length, dry.mass) |>
    correlate(method="spearman")
#this generates a new object, let's look at it
view(cells.cor)

#now we can make a plot of all these:
rplot(cells.cor)

#we can improve this with:
#grouping highly correlated variables closer together with rearrange
cells.cor <- rearrange(cells.cor, absolute = FALSE)

#'shaving off' upper triangle 
cells.cor <- shave(cells.cor)

#now we have this simpler correlatino data:
view(cells.cor)

#and a plot is very tidy
rplot(x)

#exercise for students: what does this plot mean?


##############################################
#CAN WE CLASSIFY WITH TWO STATS?
##############################################

#We can also plot populations of cells on two axes using two metrics
#This *might* allow us to classify them

#plot 6a:
ggplot(cells,aes(x=mean.thickness,y=length,colour=clone))+
    geom_point()

#this is hard to see, because all the dots overlap
#so tidy up, with several tricks:

#plot 6b
#using only 10% of the data
cells |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(x=mean.thickness,y=length,colour=clone))+
    geom_point()

#plot 6c
#geom_point(alpha=0.1), makes the dots shaded
#and use a totally white background with:
#theme_classic()
cells |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(x=mean.thickness,y=length,colour=clone))+
    geom_point(alpha=0.1)+
    theme_classic()

#finally, lets show all the replicates
#with facet_wrap(~replicate)
cells |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(x=mean.thickness,y=length,colour=clone))+
    geom_point(alpha=0.1)+
    theme_classic()+
    facet_wrap(~replicate)

    
#let's try the same plot, with wto different metrics:
#sphericity and area
cells |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(x=sphericity,y=dry.mass,colour=clone))+
    geom_point(alpha=0.1)+
    facet_wrap(~replicate)+
    theme_classic()
    

