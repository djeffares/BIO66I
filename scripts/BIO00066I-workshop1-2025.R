################################################################################
#BIO00066I workshop 1 script
#from: https://3mmarand.github.io/R4BABS/r4babs4/week-1/workshop.html
#date 2024-02-11
#author: daniel jeffares
#here I will run through the entire workshop
################################################################################

################################################################################
#LOAD PACKAGES
#the tidyverse
library(tidyverse)
################################################################################

################################################################################
#LOAD DATA ANDSAVE LOCALLY
#We could either download manually
#Or load them directly from the web:
#using something like: data <-read_tsv(url("www.this.com"))

##biotech data
#load
biotech <- read_tsv(url("https://3mmarand.github.io/R4BABS/r4babs4/week-1/data-raw/biotech-cts.txt"))
#save locally
write_tsv(biotech, file="data-raw/biotech-cts.txt")

##cell bio data
#load
cell_bio <- read_tsv(url("https://3mmarand.github.io/R4BABS/r4babs4/week-1/data-raw/cell-bio.tsv"))
#save
write_tsv(cell_bio, file="data-raw/cell-bio.tsv")

##immuno
#load
immuno <- read_csv(url("https://3mmarand.github.io/R4BABS/r4babs4/week-1/data-raw/immuno.csv"))
#save
write_tsv(immuno, file="data-raw/immuno.csv")

##frogs!
#load
frogs <- read_csv(url("https://3mmarand.github.io/R4BABS/r4babs4/week-1/data-raw/xlaevis_counts_S20.csv"))
#save
write_tsv(frogs, file="data-raw/xlaevis_counts_S20.csv")

#save ALL my work
save.image("BIO00066I-workshop1-data.Rda")

################################################################################
#FIRST STEPS
#First looks at the data, and tidying up

#Use View to see the data in an excel like table
View(cell_bio)

#use names to see and change the name of a column
#see the names
names(biotech)
#rename column one 
names(biotech)[1] <- "transcript"
#and save it again
write_tsv(biotech, file="data-raw/biotech-cts.renamed.txt")

#clean up the names for the cell biology data
cell_bio <- janitor::clean_names(cell_bio)
#check the names: are they really better Emma??
names(cell_bio)

#NB:I had to install janitor first:
#install.packages("janitor")

################################################################################
#DATA OVERVIEW
#Start to summarise the data

#biotech
summary(biotech)
glimpse(biotech)

#immuno
summary(immuno)
glimpse(immuno)

#cell_bio
summary(cell_bio)
glimpse(cell_bio)

#frogs
summary(frogs)
glimpse(frogs)

################################################################################
#QUALITY CONTROL 1
#use filter to remove rows we don't want
  #count the rows first
  nrow(cell_bio) #91885
  #filter
  cell_bio <- cell_bio |> filter(!is.na(perimeter))
  #count again
  nrow(cell_bio) #71937
  #the count has changed a lot!
  #or we could use: cell_bio <- cell_bio |> drop_na(perimeter)

#make a subset of the immuno data
  #filter
  immuno_media <- immuno |> 
    filter(treatment == "MEDIA")
  #count the rows before and after
  nrow(immuno)        #171245
  nrow(immuno_media)  #41864 a lot less!

#make another subset of the immuno data
  immuno_media_live <- immuno |>
    filter(treatment == "MEDIA") |>
    filter(between(FS_Lin, 7500, 28000))
  #count again
  nrow(immuno_media_live) #35075 less, but not by much
  
#now create a dataframe called immuno_live that contains only the rows:
  #where FS_Lin is between 7500 and 28000 
  #and SS_Lin is between 15000 and 35000
  #code
  immuno_live <- immuno |>
    filter(between(FS_Lin, 7500, 28000)) |>
    filter(between(SS_Lin, 15000, 35000))
  #count rows
  nrow(immuno_live) #50324
  
################################################################################
#QUALITY CONTROL 2
#Selecting columns: select() function help us here.

  #select named columns of the biotech tibble
  biotech_susceptible <- biotech |> 
    select(SCK14_1,
           SCK14_2,
           SCK14_3,
           SLK14_1,
           SLK14_2,
           SLK14_3)
  #check the number of columns before and after:
  ncol(biotech) #13
  ncol(biotech_susceptible) #6
  
  #Select columns starting with S
  biotech_susceptible <- biotech |> 
    select(starts_with("S"))
  
  # select a range of columns, eg: if column names ate 1..10
  #we select cols 2..6 with select(2:6)
  biotech_susceptible <- biotech |> 
    select(SCK14_1:SLK14_3)
  
  #use select and filter together!
  #create a dataframe from frogs which has
  #only the columns from sibling “_A” 
  #and only the rows where S20_C_5 is above 20.
  
  #first, examine the names of the columns
  names(frogs) #some have _A at the end, select these
  #and look at the data a little
  glimpse(frogs)
  View(frogs)
  
  #now select and filter
  #this time it matters which *order* we do the code!
  #nasty trick Emma Rand ;)
  frogs_S20_C_5_20_siblingA <- frogs |> 
    filter(S20_C_5 > 20) |> 
    select(ends_with("_A"))
    

################################################################################
#VISUALISATION:
#plots! 
  
#pivot longer:
biotech_pivot <- biotech |> 
    pivot_longer(
      cols = -transcript,
      names_to = "sample",
      values_to = "count")  


#what did pivot_longer do??
#check it out
glimpse(biotech)
glimpse(biotech_pivot)
view(biotech)
view(biotech_pivot)

#and my fav: dim
dim(biotech) #87634 rows and 13 columns
dim(biotech_pivot) #1051608 rows (!!) and only 3 columns!

biotech_pivot |>
  ggplot(aes(x = sample, y = log10(count))) +
  geom_boxplot()

################################################################################
#PACKAGE REFERENCES 
################################################################################
#get the citations:
print(citation("tidyverse"), style = "text")
print(citation("janitor"), style = "text")

#tidyverse
#Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G, Hayes A, Henry L, Hester
#J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi
#K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the tidyverse.” _Journal of Open Source
#Software_, *4*(43), 1686. doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

#janitor:
#Firke S (2024). _janitor: Simple Tools for Examining and Cleaning Dirty Data_. R package version
#2.2.1, <https://CRAN.R-project.org/package=janitor>.

