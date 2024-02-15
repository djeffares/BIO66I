#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)
library(ggpubr)

cells <-read_tsv(url("https://djeffares.github.io/teaching/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv"),
                 col_types = cols(
                     clone = col_factor(),
                     replicate = col_factor(),
                     tracking.id=col_factor(),
                     lineage.id=col_factor()
                 )
)

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

#take the mean of each unique tracking id
trackingid.summary.table<-cells |>
    group_by(clone, replicate, tracking.id) |>
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

#how many rows
row.count<-nrow(trackingid.summary.table)
13688/100

#subset
#sample_n samples a number of rows at random
#If the data frame is grouped, the number applies to each group
#Our sample is group by 
trackingid.small.data <- sample_n(trackingid.summary.table, 5)

glimpse(trackingid.summary.table)

nrow(trackingid.small.data)



glimpse(trackingid.summary.table)