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
summary(a1)

#remove all the NAs
a |> 
  

              


