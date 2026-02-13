# INTRODUCTION ----
# Title: BIO00066I (BABS4) workshop 1
# Date: 2026-02-04
# Summary: Exercises to get us started with R in BABS4
# Source: https://3mmarand.github.io/R4BABS/r4babs4/week-1/workshop.html

# DATA  DESCRIPTION ----

#We will be working with the following data files in this workshop

# All these data files are in modules/BIO66I/data-raw

#biotech-cts.txt. 
#These are RNA-Seq data from three replicates for each of two of wheat varieties 
#(Susceptible and Tolerant) grown at two potassium conditions (Control K and Low K). 
#The data are counts of the number of reads.

#cell-bio.tsv. 
#These are measurements from a Livecyte microscope, 
#which performs live cell imaging, tracking individual cells, and measuring 
#cell shape and size parameters for thousands of cells as they grow and divide. 
#Measurements are taken for three replicates from each of two cell lines A and B.

# immuno.csv. 
#These are flow cytometry data for three treatments with each of 
#two antibodies. The measures are Forward scatter, side scatter, red 
#fluorescence and green fluorescence.

#xlaevis_counts_S20.csv. 
#These are RNA-Seq data from frogs. There are 2 siblings from each of three 
#fertilisations and one sibling was treated with FGF and the other was a control. 
#The data are counts of the number of reads.

# SET UP AND LIBRARIES ----

#clear th previous data
rm(list=ls())

#load libraries
library(tidyverse)

# CODE ----

## Import the data ----

biotech <- read_tsv("data-raw/biotech-cts.txt")
cell_bio <- read_tsv("data-raw/cell-bio.tsv")
immuno <- read_csv("data-raw/immuno.csv")
frogs <- read_csv("data-raw/xlaevis_counts_S20.csv")

# examine biotech the data, and tidy up
view(biotech)
glimpse(biotech)
summary(biotech)

#Name of the first column in the biotech dataframe:
names(biotech)[1] <- "transcript"
  
# examine cell_bio the data, and tidy up
view(cell_bio)
glimpse(cell_bio)
summary(cell_bio)
cell_bio <- janitor::clean_names(cell_bio)
glimpse(cell_bio)

# examine immuno data, and tidy up
view(immuno)

# examine frogs data, and tidy up
view(frogs)

## Quality control ----

# Filtering rows
cell_bio_with_perimeter <- cell_bio |> 
  filter(!is.na(perimeter))
# or even simpler:
cell_bio <- cell_bio |> drop_na(perimeter)
# another example
immuno_media <- immuno |> 
  filter(treatment == "MEDIA")

# Create a new dataframe called immuno_media_live that contains only the 
#rows where treatment is “MEDIA” and FS_Lin is between 7500 and 28000:

immuno_media_live <- immuno |>
  filter(treatment == "MEDIA") |>
  filter(between(FS_Lin, 7500, 28000))

# Create a dataframe called immuno_live that contains only the rows where 
#FS_Lin is between 7500 and 28000 and SS_Lin is between 15000 and 35000:

immuno_live <- immuno |>
  filter(between(FS_Lin, 7500, 28000)) |>
  filter(between(SS_Lin, 15000, 35000))

#check it worked
summary(immuno$FS_Lin)
summary(immuno_live$FS_Lin)

## Selecting columns ----


# Select columns starting with S
biotech_susceptible <- biotech |> 
  select(starts_with("S"))
 
# select a range of columns: rom SCK14_1 to SLK14_3
biotech_susceptible <- biotech |> 
  select(SCK14_1:SLK14_3)

## Visualisation ----

# plot a histogram of S20_C_5 in the frogs dataframe:
frogs |> 
  ggplot(aes(x = S20_C_5)) +
  geom_histogram() 

# now same plot on a log scale
frogs |> 
  ggplot(aes(x = log10(S20_C_5))) +
  geom_histogram()

# or use scale_x_log10()   
frogs |> 
  ggplot(aes(x = S20_C_5)) +
  geom_histogram() +
  scale_x_log10()+
  xlab("S20_C_5 (log10 scale)")
  
# or density plot
frogs |> 
  ggplot(aes(x = log10(S20_C_5))) +
  geom_density()

# plot the distribution of area in the cell_bio dataframe as a density plot, 
#with the clone variable as the fill:
cell_bio |> 
  ggplot(aes(x = area, fill = clone)) +
  geom_density(alpha = 0.5)

# plot the distribution of area in the cell_bio dataframe as a boxplot, 
#with the clone variable on the x-axis:
cell_bio |> 
  ggplot(aes(x = clone, y = area)) +
  geom_boxplot()

# Plot the distribution of the logged counts in the biotech dataframe 
#as a boxplot, the samples on the x axis:

biotech_pivot <-
  biotech |> pivot_longer(cols = -transcript,
    names_to = "sample",
  values_to = "count") 

# plot the gene expression values: boxplot log10
biotech_pivot |> 
  ggplot(aes(x = sample, y = log10(count))) +
  geom_boxplot()

# plot the gene expression values: violin log10
biotech_pivot |> 
  ggplot(aes(x = sample, y = log10(count))) +
  geom_violin()


# MY ADDITIONAL CODE ----

## checking perimeter effects

#add new column has_perimeter
cell_bio <- cell_bio |> 
  mutate(has_perimeter = ifelse(is.na(perimeter), "no", "yes"))


#summarise mean and medians of all numeric columns in cell_bio by has_perimeter
cell_bio_summary <- cell_bio |>
  group_by(has_perimeter) |>
  summarise(across(where(is.numeric), list(mean = mean, median = median), na.rm = TRUE))
view(cell_bio_summary)

names(cell_bio)
ggplot(cell_bio, aes(x = has_perimeter, y = instantaneous_velocity)) +
  geom_violin() +
  theme_minimal()

# END OF CODE ----


