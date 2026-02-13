########################################################
#WORKSHOP 3: CELL MOVEMENT DATA
########################################################



#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the corr library: this is for examining correlations between many metrics
library(corrr)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)


# Read the automated Livecyte data
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)

#Load from local file for rendering
cells <-read_tsv("data/all-cell-data-FFT.filtered.2024-02-22.tsv",
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)

names(cells)


#select only the columns we need
cell.move.data <- select(cells,
        clone,
        replicate,
        displacement, 
        track.length, 
        instantaneous.velocity
)
#check that we have
names(cell.move.data)

#get a simple summaru, using summary and also glimpse
summary(cell.move.data)
glimpse(cell.move.data)

#lets save our data
save.image("BIO00066I-workshop3-cell-movement-metrics.Rda")

#you can load this any time later with:
load("BIO00066I-workshop3-cell-movement-metrics.Rda")

#instantaneous.velocity - geom_violin
ggplot(cell.move.data,aes(x=clone,y=instantaneous.velocity,colour=clone))+
    geom_violin(alpha=0.5)+
    stat_compare_means()


ggplot(cell.move.data,aes(x=clone,y=log10(instantaneous.velocity),colour=clone))+
    geom_violin(alpha=0.5)+
    facet_wrap(~replicate)+
    stat_compare_means()

#load the manual tracking data
track <-read_tsv(url("https://djeffares.github.io/BIO66I/data/A1-and-B2-tracking.data.2025-02-27.tsv"))

#check it out
glimpse(track)
names(track)

#Load from local file for rendering
track <-read_tsv("data/A1-and-B2-tracking.data.2025-02-27.tsv")
glimpse(track)
names(track)

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

write_tsv(track.correlations, "track.correlations.tsv")

#Adjust the name of the first column to "Variable1"
names(track.correlations)[1]="Variable1"

#simplify the data with pivot_longer
track.correlations.pivot <- 
  track.correlations |> 
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "corr.coeff")

#examine what we have
head(track.correlations.pivot)

#plot data in the track.correlations.pivot table
#using geom_tile (for coloured boxes) and geom_text (to show the correlation coefficient values) 
ggplot(track.correlations.pivot, aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#pipe track.correlations.pivot data frame into filter
track.correlations.pivot |>

  #filter out the TID and LID columns
  filter(Variable1 != 'LID') |>
  filter(Variable2 != 'LID') |>
  filter(Variable1 != 'TID') |>
  filter(Variable2 != 'TID') |>
  
  #now we put the plotting code here
  ggplot(aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#and voila! no more TID and LID columns

#track.length without the filter
no.filter.plot<- track |> 
  ggplot(aes(x=cell.line, y = track.length))+
  geom_violin()+
  stat_compare_means()+
  ggtitle("no filter")

#track.length WITH the filter
filter.plot<- track |> 
  filter(track.present.at.start.or.end != TRUE) |> 
  ggplot(aes(x=cell.line, y = track.length))+
  geom_violin()+
  stat_compare_means()+
  ggtitle("with filter")

#show plots side by side
ggarrange(no.filter.plot,filter.plot)


#filter the data
track.correlations.pivot |>
  filter(abs(corr.coeff) > 0.25) |>
#now we put the plotting code here 
  ggplot(aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# track.correlations <-read_tsv(url("https://djeffares.github.io/BIO66I/track.correlations.tsv"))
