#further analysis of the all-cell-data-FFT.tsv data

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

#load data
cells <-read_tsv(url("https://djeffares.github.io/data/BIO00066I/all-cell-data-FFT.tsv"))

#clone A vs clone B
unique(cells$clone)