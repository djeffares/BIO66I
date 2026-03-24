#Data Analysis 2: Cell Biology
#Date 2026-02-13


#clear previous data
rm(list=ls())

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

#how many rows and columns do we have?
nrow(cells)
ncol(cells)
dim(cells)

#other ways to peek at data:
summary(cells)
glimpse(cells)

#see what we have
names(cells)

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

# str(cells)

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_boxplot()

ggplot(cells,aes(x=clone,y=sphericity,fill=replicate))+
  geom_boxplot()



# make a density plot of the width data
# using colour = replicate makes separate lines for each replicate

ggplot(cells, aes(x = width,colour = replicate)) +
  geom_density()+
  facet_wrap(~clone)+ #facet_wrap makes separate plots for each clone
  scale_x_log10()     #the log10 scale makes this plot easier to read


# Test if cloneA and cloneB have statistically different widths
# Use  Wilcoxon rank sum test, which does not assume normal distribution
wilcox.test(width ~ clone, data = cells)

ggplot(cells,aes(x=clone,y=width))+
    geom_boxplot()+
    stat_compare_means()

str(cells)  #shows the type of each column

# make cloneA and cloneB data frames
cloneA.data <- cells |> filter(clone == "cloneA")
cloneB.data <- cells |> filter(clone == "cloneB")

#get the names of the numeric columns
numeric.columns <- cells |> 
  select(where(is.numeric)) |> 
  names()

#see what we have
numeric.columns

#create empty data frame
clone.comparisons <- data.frame(
  variable = character(),
  cloneA.median = numeric(),
  cloneB.median = numeric(),
  median.ratio = numeric(),
  p.value = numeric()
)

#loop through each numeric column
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
  
  #add this row to our results table
  new.row <- data.frame(
    variable = column.name,
    cloneA.median = signif(cloneA.median,2),
    cloneB.median = signif(cloneB.median,2),
    median.ratio = signif(ratio,2),
    p.value = p.val
  )
  
  #add the new.row to the clone.comparisons data frame
  clone.comparisons <- rbind(clone.comparisons, new.row)
}

# prepare data for PCA

cells_for_pca <- cells |>
  #select the numeric columns
  select(clone, replicate, volume, mean.thickness, radius, area, 
         sphericity, length, width, orientation, dry.mass, length.to.width) |>
  # remove rows with NA values
  drop_na()

pca_result <- cells_for_pca |>
  select(-clone, -replicate) |>  # remove the group categories
  prcomp()                        # performs the PCA

# View summary of PCA
summary(pca_result)

# extract PCA scores and combine with clone and replicate info
pca_scores <- as.data.frame(pca_result$x) |>
  bind_cols(cells_for_pca |> select(clone, replicate))

# plot PCA scores with ggplot, coloring by clone
# use alpha = 0.1 to make points semi-transparent
# use size = 4 to make points larger
# save the plot as pca.plot
pca.plot <- ggplot(pca_scores, aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4) 

# view the plot
pca.plot

# downsample the PCA scores to 3000 cells per clone
pca_scores_sample <- pca_scores |>
  group_by(clone) |>
  slice_sample(n = 3000) |>
  ungroup()

# make the plot
pca.plot <- ggplot(pca_scores_sample, 
  aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4)
# view the plot
pca.plot

# make the plot
# stat_ellipse adds an ellipse to show the shape of the data
# scale_color_manual sets the colors for the clones
# facet_wrap makes separate plots for each replicate
# theme_classic() makes the plot look nicer
pca.plot +
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("cloneA" = "red", "cloneB" = "blue"))+
  #facet_wrap(~replicate)+
  theme_classic2()

ggplot(cells,aes(x=clone,y=width,fill=replicate))+
    geom_violin()

# a violin plot with title, axis labels and a theme
ggplot(cells,aes(x=clone,y=length,fill=replicate))+
    geom_violin()+
    ggtitle("put your title here!")+
    xlab("my X axis label")+
    ylab("my Y axis label")+
    theme_classic()

