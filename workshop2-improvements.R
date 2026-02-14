#Data Analysis 2: Cell Biology
#Date 2026-02-13


#clear previous data
rm(list=ls())


## SET UP AND LOAD DATA ----
#load the tidyverse
library(tidyverse)


#load the ggpubr package for multi-part plots
library(ggpubr)

cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)

#look at the data, like an excel table:
view(cells)

#what are the names of the columns?
names(cells)

#how many rows and columns you we have?
nrow(cells)
ncol(cells)
dim(cells)

#other ways to peek at data:
summary(cells)
glimpse(cells)

#see what we have
names(cells)

## CLEAN DATA ----

#remove all the movement metrics (which are not reliable)
cells <- select(cells, 
                -position.x,
                -position.y, 
                -pixel.position.x, 
                -pixel.position.y,
                -displacement,
                -instantaneous.velocity,
                -instantaneous.velocity.x,
                -instantaneous.velocity.y,
                -track.length
)



#check that our data is simpler
names(cells)

#save all my stuff
save.image("BIO00066I-workshop2.Rda")

#load all my stuff from last time
load("BIO00066I-workshop2.Rda")


# make a summary
names(cells)

#loop though 

#ggplot clone by orientation
ggplot(cells, aes(x = clone, y = area)) +
  geom_boxplot()+
  stat_compare_means()
  
#data that differs between clones:
#volume, mean.thickness, radius, area


names(summary.table)
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
        length.to.width=median(length.to.width)
)

view(summary.table)

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_boxplot()

names(cells)

## PLOTS ----

##better: show density
ggplot(cells, aes(x = width,colour = replicate)) +
  geom_density()+
  facet_wrap(~clone)


##better: show density
ggplot(cells, aes(x = mean.thickness,colour = replicate)) +
  geom_density()+
  facet_wrap(~clone)+
  scale_x_log10()+
  geom_vline(xintercept = median(cells$mean.thickness), linetype = "dashed", color = "red")



#Wilcoxon rank sum test
#To test if cloneA and cloneB have statistically different widths
wilcox.test(width ~ clone, data = cells)

ggplot(cells,aes(x=clone,y=width))+
    geom_boxplot()+
    stat_compare_means()

names(cells)

## COMPARE ALL NUMERIC VARIABLES BETWEEN CLONES ----

#find out which numeric variables differ between clones, using wilcox.test

#Step 1: See which columns are numeric
names(cells)
str(cells)  #shows the type of each column

#Step 2: Separate the data for clone A and clone B
cloneA.data <- cells |> filter(clone == "cloneA")
cloneB.data <- cells |> filter(clone == "cloneB")

#Step 3: Get list of numeric column names (exclude factors)
numeric.columns <- cells |> 
  select(where(is.numeric)) |> 
  names()

#see what we have
numeric.columns

#Step 4: Create an empty table to store results
clone.comparisons <- data.frame(
  variable = character(),
  cloneA.median = numeric(),
  cloneB.median = numeric(),
  median.ratio = numeric(),
  p.value = numeric()
)

#Step 5: Loop through each numeric variable
#For each one, calculate medians and run wilcox.test
for(column.name in numeric.columns) {
  
  #calculate median for clone A
  cloneA.median <- median(cloneA.data[[column.name]], na.rm = TRUE)
  
  #calculate median for clone B  
  cloneB.median <- median(cloneB.data[[column.name]], na.rm = TRUE)
  
  #calculate the median ratio (cloneA.median / cloneB.median)
  ratio <- cloneA.median / cloneB.median
  
  #run wilcox test comparing the two clones for this variable
  test.result <- wilcox.test(cells[[column.name]] ~ cells$clone)
  
  #extract the p-value
  p.val <- test.result$p.value
  
  test.result$statistic
  #add this row to our results table
  new.row <- data.frame(
    variable = column.name,
    cloneA.median = signif(cloneA.median,2),
    cloneB.median = signif(cloneB.median,2),
    median.ratio = signif(ratio,2),
    p.value = p.val
  )
  
  clone.comparisons <- rbind(clone.comparisons, new.row)
}

#Step 6: Look at the results!
view(clone.comparisons)



#make barplot of clone.comparisons, y is  median.ratio using variable as the names
ggplot(clone.comparisons, aes(x = reorder(variable, median.ratio), y = median.ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  labs(title = "Median Ratio (Clone A / Clone B) for Numeric Variables",
       x = "Variable",
       y = "Median Ratio (Clone A / Clone B)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_hline(yintercept = 1, linetype = "dashed", color = "red")  # Add a reference line at ratio = 1


#read in some data from a website
small.tracking.data <-read_tsv(url("https://djeffares.github.io/BIO66I/data/trackingid.small.data.tsv"),
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)

#check what we have
glimpse(small.tracking.data)
summary(small.tracking.data)

#Load from local file for rendering
small.tracking.data <-read_tsv("data/trackingid.small.data.tsv",
    col_types = cols(
        clone = col_factor(),
        replicate = col_factor(),
        tracking.id=col_factor(),
        lineage.id=col_factor()
    )
)
glimpse(small.tracking.data)
summary(small.tracking.data)

ggplot(small.tracking.data, aes(x=clone, y=width))+
    geom_boxplot(fill=NA)+
    geom_jitter(width = 0.2,size=3,pch=1)+
    theme_minimal()

names(small.tracking.data)

#test with small.tracking.data
wilcox.test(width ~ clone, data = small.tracking.data)

#test with 'cells' data frame - a *much* larger data set  
wilcox.test(width ~ clone, data = cells)

#plot with the large data
#storing the plot in an object called large.data.plot
large.data.plot <- ggplot(cells, aes(x=clone, y=width))+
    geom_boxplot()+
    geom_jitter(width = 0.2,size=3,pch=1)+
    stat_compare_means()+
    ylim(0,200)+
    ggtitle("large data")

#plot with the small data
#storing the plot in an object called small.data.plot
small.data.plot <- ggplot(small.tracking.data, aes(x=clone, y=width))+
    geom_boxplot()+
    geom_jitter(width = 0.2,size=3,pch=1)+
    stat_compare_means()+
    ylim(0,200)+
    ggtitle("small data")

#create a two panel plot, with large.data.plot and small.data.plot
ggarrange(large.data.plot,small.data.plot)

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_violin()


ggplot(cells,aes(x=clone,y=length,fill=replicate))+
    geom_violin()+
    ggtitle("put your title here!")+
    xlab("my X axis label")+
    ylab("my Y axis label")+
    theme_classic()


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
        width=median(width),
        dry.mass=median(dry.mass),
        length.to.width=median(length.to.width)
    )



#collect a random subset, of 5 rows for each tracking id
trackingid.small.data <- sample_n(trackingid.summary.table, 5)

#check that we have
nrow(trackingid.small.data)
glimpse(trackingid.small.data)

#output trackingid.small.data as a tab-separated value (tsv) file
write_tsv(trackingid.small.data, file ="/Users/dj757/gd/modules/BIO66I/data/trackingid.small.data.tsv")

#how many rows do we have?
nrow(trackingid.small.data)

#what does the data look like?
view(trackingid.small.data)

save.image("BIO00066I-workshop2.Rda")

## PCA ----

# Prepare data for PCA
# Select numeric columns and remove any rows with NA values
names(cells)
cells_for_pca <- cells |>
  select(clone, replicate, volume, mean.thickness, radius, area, 
         sphericity, length, width, orientation, dry.mass, length.to.width) |>
  drop_na()

# Perform PCA on scaled numeric data
pca_result <- cells_for_pca |>
  select(-clone, -replicate) |>  # Remove grouping variables
  prcomp()

# View summary of PCA
summary(pca_result)

# Extract PC scores and combine with clone information
pca_scores <- as.data.frame(pca_result$x) |>
  bind_cols(cells_for_pca |> select(clone, replicate))

# Plot PC1 vs PC2 colored by clone
ggplot(pca_scores, aes(x = PC1, y = PC2, color = clone)) +
  geom_point(alpha = 0.1, size = 1) +
  stat_ellipse() +  # Add confidence ellipses
  theme_classic() +
  labs(title = "PCA: Clone A vs Clone B",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))+
  

## better: plot PCAs
ggplot(pca_scores, aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4) +
  scale_color_manual(values = c("cloneA" = "#E41A1C", "cloneB" = "#377EB8")) +
  theme_classic()

pca.plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4)
  
  

#so downsample:

# Downsample to 3000 cells per clone for better visualization
pca_scores_sample <- pca_scores |>
  group_by(clone) |>
  slice_sample(n = 3000) |>
  ungroup()

pca.plot <- ggplot(pca_scores_sample, 
  aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4)
pca.plot

#improve the plot
pca.plot +
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("cloneA" = "red", "cloneB" = "blue"))+
  facet_wrap(~replicate)+
  theme_classic2()




#plot again
ggplot(pca_scores_sample, aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.4, size = 2) +
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("cloneA" = "#E41A1C", "cloneB" = "#377EB8")) +
  scale_fill_manual(values = c("cloneA" = "#E41A1C", "cloneB" = "#377EB8")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(title = "PCA: Clone A vs Clone B (3000 cells each)",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))



# View variable loadings (which variables contribute most to separation)
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2]) |>
  rownames_to_column("variable")

ggplot(pca_loadings, aes(x = PC1, y = PC2)) +
  geom_segment(aes(xend = PC1, yend = PC2), 
               x = 0, y = 0, arrow = arrow(length = unit(0.3, "cm"))) +
  geom_text(aes(label = variable), hjust = 0, nudge_x = 0.01) +
  theme_classic() +
  labs(title = "PCA Loadings - Variable Contributions")


pca_scores_sample <- pca_scores |>
  group_by(clone) |>
  slice_sample(n = 3000) |>
  ungroup()

ggplot(pca_scores_sample, aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.4, size = 2) +
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("cloneA" = "#E41A1C", "cloneB" = "#377EB8")) +
  scale_fill_manual(values = c("cloneA" = "#E41A1C", "cloneB" = "#377EB8")) +
  theme_classic() +
  theme(legend.position = "top") +
  labs(title = "PCA: Clone A vs Clone B (3000 cells each)",
       x = paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100, 1), "%)"),
       y = paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100, 1), "%)"))
