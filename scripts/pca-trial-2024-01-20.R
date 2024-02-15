#from: https://bjnnowak.netlify.app/2021/09/15/r-pca-with-tidyverse/

rm(list=ls())
library(tidyverse)

############################################################
#with boring data data
############################################################

# Load data 
# Billbord ranking
billboard <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/billboard.csv')
# Songs features based on Spotify API
features <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-14/audio_features.csv')

head(features)
view(features)

#For the next part of our analysis we will look at the evolution of songs’ features 
#over the years. To do this, we need to create a column with the year of songs’ 
#creation, using {tidyverse} tools. Then, we will add this new column to songs’ features data.


bill_prep<-billboard%>%
    # Keep only 1st appearance on Billboard
    filter(
        (weeks_on_chart==1)&(instance==1)
    )%>%
    # Add Year column
    mutate(year=format(
        as.Date(week_id,"%m/%d/%Y"),format="%Y")
    )%>%
    # Set year as numeric
    mutate(year=as.numeric(year))

# Add year to songs' features data
features_prep<-features%>%
    left_join(bill_prep,by="song_id")


#First, we will select the variables on interest for the PCA
#plus one additional variable (year of creation of the song).
PCA_data<-features_prep |>  
    select(c(danceability,energy,instrumentalness,key,acousticness,mode,valence,tempo,
          time_signature,speechiness,loudness,liveness,year)) |> 
    drop_na()

PCA <-PCA_data |> 
    select(-year)|> 
    prcomp(scale = TRUE)
dim(PCA)

#Now, we need the {broom} extension to access the results of prcomp() 
#with the {tidyverse} syntax. After loading {broom}
#you can use the tidy() function to access the results of the PCA such as eigenvalues.

#
library(broom)

#to see the eigen values
tidy(PCA, matrix = "eigenvalues")
PCA_indiv <-PCA |> augment(PCA_data)

dim(PCA_indiv)
PCA_indiv
view(PCA_indiv)
names(PCA_indiv)
ggplot(
    data=PCA_indiv,
    aes(.fittedPC1, .fittedPC2,color=loudness))+
    geom_point()+
    labs(
        title = 'Plot of individuals',
        subtitle = 'Color shows year of song creation',
        x='PC1 (22%)',
        y='PC2 (11%)',
        color='loudness'
    )+
    theme_minimal()


############################################################
#with cell data
############################################################

cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv"))
unfilt.cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.tsv"))

#make replicates factors
cells$replicate<-as.factor(cells$replicate)
unfilt.cells$replicate<-as.factor(unfilt.cells$replicate)

names(cells)
glimpse(cells)

PCA_data<-cells|>
    select(-frame,-tracking.id,-lineage.id) |> 
    drop_na()
glimpse(PCA_data)

PCA <-PCA_data |> 
    select(-clone,-replicate) |> 
    prcomp(scale = TRUE)

PCA_indiv<-PCA |> 
    augment(PCA_data)


# Plot of clones
pca.plot.12<-
    PCA_indiv |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(.fittedPC1, .fittedPC2, color=clone))+
    geom_point(alpha=0.1)+
    theme_classic2()+
    facet_wrap(~replicate)

pca.plot.34<-
    PCA_indiv |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(.fittedPC3, .fittedPC4, color=clone))+
    geom_point(alpha=0.1)+
    theme_classic2()+
    facet_wrap(~replicate)

pca.plot.15<-
    PCA_indiv |> 
    slice_sample(prop=0.1) |>
    ggplot(aes(.fittedPC1, .fittedPC5, color=clone))+
    geom_point(alpha=0.1)+
    theme_classic2()+
    facet_wrap(~replicate)

#add PC contribs
pca.contribs<-tidy(PCA,matrix = "eigenvalues")
pca.contribs.plot<-ggplot(pca.contribs, aes(x=PC,y=percent))+
    geom_col()+
    theme_classic2()

arranged.pca.plot.2024.01.20<-ggarrange(
    pca.plot.12,
    pca.plot.34,
    pca.plot.15,
    pca.contribs.plot)


ggsave(arranged.pca.plot.2024.01.20,file="plots/arranged.pca.plot.2024.01.20.jpeg")

