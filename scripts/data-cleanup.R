#data clean up
#date: 20231130

#clear th decks
rm(list=ls())

#set the working directory
setwd("/Users/dj757/gd/modules/BIO00066I")

#load libraries
library(tidyverse)

#read in the data into a Tibble called "df"
df <- read_csv("raw-data/Cell-Biology-Full-Featuretable-A2.csv",skip=1)

#make the names simpler:
  #to lowercase
  names(df)<-tolower(names(df))
  #replace spaces with dots
  names(df)<-gsub(" ",".",names(df))
  names(df)<-gsub("[()]","",names(df))
  names(df)<-gsub("µ","u",names(df))
  names(df)<-gsub("[/]",".per.",names(df))


#check

#output
write_tsv(df, "data/Cell-Biology-Full-Featuretable-A2-clean.tsv")

#edit out the odd characters using excel    
df2 <- read_tsv("raw-data/Cell-Biology-Full-Featuretable-A2-clean2.txt")
names(df2)

#make the names simpler, removing .um and .pg
    names(df2)<-gsub(".um","",names(df2)) 
    names(df2)<-gsub(".pg","",names(df2)) 
names(df2)
    
#rename to A1
A1<-df2
write_tsv(A1, "raw-data/Cell-Biology-Full-Featuretable-A2-clean3.tsv")

# 2023-12-17
# fix the other files:
rm(list=ls())

#set the working directory
setwd("/Users/dj757/gd/modules/BIO00066I")

#load libraries
library(tidyverse)

#load the cells tibble
load("data/cells.Rda")

#load another file
A1<-read_csv('data/A1-FFT.csv',skip=1)
ls()
names(cells)[11:20]
names(A1)[11:20]
view(A1)

# column names are the same so update A1
names(A1)<-names(cells)
write_tsv(A1, file='data/A1-FFT.tsv')

#repeat for all A2, A3, B1-B3
A2<-read_csv('data/A2-FFT.csv',skip=1)
names(A2)<-names(cells)
write_tsv(A2, file='data/A2-FFT.tsv')

A3<-read_csv('data/A3-FFT.csv',skip=1)
names(A3)<-names(cells)
write_tsv(A3, file='data/A3-FFT.tsv')

B1<-read_csv('data/B1-FFT.csv',skip=1)
names(B1)<-names(cells)
write_tsv(B1, file='data/B1-FFT.tsv')

B2<-read_csv('raw-data/B2-FFT.csv',skip=1)
names(B2)<-names(cells)
write_tsv(B2, file='data/B2-FFT.tsv')

B3<-read_csv('data/B3-FFT.csv',skip=1)
names(B3)<-names(cells)
write_tsv(B3, file='data/B3-FFT.tsv')

# compare Cell-Biology-Full-Featuretable-A2-clean3.tsv with A2-FFT.tsv
# they may be the same

A2<-read_tsv('data/A2-FFT.tsv')
fft<-read_tsv('data/Cell-Biology-Full-Featuretable-A2-clean3.tsv')

#check out the data
glimpse(A2)
cor.test(A2$frame,fft$frame)    #same
cor.test(A2$displacement.um,fft$displacement.um)    #same
cor.test(A2$orientation,fft$orientation)    #same
# OK so Cell-Biology-Full-Featuretable-A2-clean3.tsv  == A2-FFT.tsv


#another clean up 2023-12-18
all<-read_tsv("data/all-cell-data-FFT.tsv")

#remove units (.um and .pg, etc) from the all data table
    names(all)<-gsub(".um","",names(all)) 
    names(all)<-gsub(".pg","",names(all)) 
    names(all)<-gsub(".per.s","",names(all)) 
    names(all)<-gsub(".pixels","",names(all)) 

#rename cell.type to clone
all<- all |> rename(clone = cell.type)
names(all)
        
#reorder and remove untracked
all<-all |>
    select(-untracked)|>
    select(clone, replicate, everything())

#make replicate a factor
all$replicate<-as.factor(all$replicate)

#calculate length.to.width ratio
all<-all |> mutate(length.to.width = length/width)

names(all)
#output to tsv
write_tsv(all,"data/all-cell-data-FFT.tsv")


#check out some values
summary.table<-all |> 
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
summary.table

#2024-01-12
#clean up data in 
#as volume column name was set to 'voe'

library(tidyverse)
dat<-read_tsv('/Users/dj757/gd/github/djeffares.github.io/data/BIO00066I/all-cell-data-FFT.tsv')
names(dat)[10]="volume"
write_tsv(dat, file="/Users/dj757/gd/github/djeffares.github.io/data/BIO00066I/all-cell-data-FFT.tsv")


#end
