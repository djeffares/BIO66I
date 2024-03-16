#fixing up w2 data with new 'gated' data
# 2024-02-21

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the ggpubr package for multi-part plots
library(ggpubr)

#existing data - pre gating
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/all-cell-data-FFT.filtered.2024-01-19.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)

#processing the new 'data
a1<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-A1_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)
a2<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-A2_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)
a3<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-A3_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)

b1<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-B1_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)
b2<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-B2_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)
b3<-read_csv("raw-data/2016-08-26_14-47-36_P-1_W-B3_ROI-1_DB_ROI1_Phase-FullFeatureTable.csv", skip=1)


names(a1)
names(a2)
names(a3)

#replicate names, then merge
a1$replicate = 1
a2$replicate = 2
a3$replicate = 3

b1$replicate = 1
b2$replicate = 2
b3$replicate = 3

#bind
A<-bind_rows(list(a1, a2, a3))
A$clone="cloneA"

B<-bind_rows(list(b1, b2, b3))
B$clone="cloneB"

#clean up names
A <- janitor::clean_names(A)
A<-relocate(A, clone, replicate)
B <- janitor::clean_names(B)
B<-relocate(B, clone, replicate)

#check columns: most are the same 
#except these two are missing from the gates data
#perimeter
#length.to.width
names(A)[1:23]
names(cells)[1:23]

#update col names for gated
names(A) <- names(cells)[1:23]
names(B) <- names(cells)[1:23]

#bind again
cells.gated<-bind_rows(A,B)

#calculate length.to.width in the gates data

cells.gated$length.to.width<-cells.gated$length/cells.gated$width
cells.gated$replicate<-as.factor(cells.gated$replicate)

summary(cells)
summary(cells.gated)

#see how the data compares
#looks fine
plot1<-ggplot(cells,aes(x=clone,y=length.to.width))+
  geom_violin()+
  #facet_wrap(~replicate)+
  stat_compare_means()
                
plot2<-ggplot(cells.gated,aes(x=clone,y=length.to.width))+
  geom_violin()+
  #facet_wrap(~replicate)+
  stat_compare_means()

ggarrange(plot1,plot2)

#export the data
write_tsv(cells.gated, file="data/all-cell-data-FFT.filtered.2024-02-22.tsv")


#now fix the workshop, so it works with this data

#old  2024-01-19
#new  2024-02-22

  

              


