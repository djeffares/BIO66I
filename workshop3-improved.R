# WORKSHOP 3: CELL MOVEMENT DATA ----


# SET UP ----

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the corr library: this is for examining correlations between many metrics
library(corrr)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

# READ LIVECYTE DATA ----

# Read the automated Livecyte data
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)




# select only the columns we need
# including the cell shape data and also the cell movement data
# from cell movement data, we keep displacement, track.length and instantaneous.velocity
# simplest to do this by removing some columns

cell.movement.data <- select(cells, 
                -position.x,
                -position.y, 
                -pixel.position.x, 
                -pixel.position.y,
                -instantaneous.velocity.x,
                -instantaneous.velocity.y,
                -orientation
)


## SUMMARY TABLE FOR CELL MOVEMENT AND SHAPE DATA ----

#this is exactly the same method we used for the cell shape data

# make cloneA and cloneB data frames
cloneA.data <- cell.movement.data |> filter(clone == "cloneA")
cloneB.data <- cell.movement.data |> filter(clone == "cloneB")

#get the names of the numeric columns
numeric.columns <- cell.movement.data |> 
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

view(clone.comparisons)

#you can plot any of these for your report


# EXAMPLE PLOT FOR MOVEMENT DATA ----

#check that we have
names(cell.movement.data)

#lets save our data
save.image("BIO00066I-workshop3.Rda")

#you can load this any time later with:
load("BIO00066I-workshop3.Rda")

#track.length- geom_boxplot
ggplot(cell.movement.data,aes(x=clone,y=track.length,colour=clone))+
    geom_boxplot()+
    stat_compare_means()

#imrpoved plot
ggplot(cell.movement.data,aes(x=clone,y=log10(track.length),colour=clone))+
    geom_boxplot()+
    facet_wrap(~replicate)+
    stat_compare_means()

#save the plot
ggsave("BIO00066I-workshop3-track-length-plot.png", width = 6, height = 4)



# PCA WITH CELL MOVEMENT LIVECYE DATA ----

# Now we make a PCA plot with only the cell movement data 
#(displacement, track.length and instantaneous.velocity)

# select only the cell movement data
cell.movement.data <- select(cells, 
                clone,
                replicate,
                displacement, 
                track.length, 
                instantaneous.velocity
)

# prepare data for PCA

cell.data.for.pca <- cell.movement.data |>
  # remove rows with NA values
  drop_na()

# Now we calculate PCA coordinates with the prcomp() function:

pca.result <- cell.data.for.pca |>
  select(-clone, -replicate) |>  # remove the group categories
  prcomp()                        # performs the PCA

# View summary of PCA
# note, that there are only 3 possible PCA corrdinates with this data
summary(pca.result)

# extract PCA scores and combine with clone and replicate info
pca.scores <- as.data.frame(pca.result$x) |>
  bind_cols(cell.data.for.pca |> select(clone, replicate))

# downsample the PCA scores to 3000 cells per clone
pca.scores.sample <- pca.scores |>
  group_by(clone) |>
  slice_sample(n = 300) |>
  ungroup()

# make the plot
pca.plot <- ggplot(pca.scores.sample, 
  aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4)+
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  scale_color_manual(values = c("cloneA" = "red", "cloneB" = "blue"))+
  facet_wrap(~replicate)+
  theme_classic2()


# view the plot
pca.plot

#save this plot
ggsave("BIO00066I-workshop3-movement-pca-plot.png", width = 6, height = 4)

# NOTE: this does not work well
# We should include ALL the data: cell shape and movement data for the PCA, 
#not just the movement data.

# MANUAL TRACK DATA ----

## LOAD AND EXAMINE MANUAL TRACK DATA ----

#load the manual tracking data
track <-read_tsv(url("https://djeffares.github.io/BIO66I/data/A1-and-B2-tracking.data.2025-02-27.tsv"))

#check it out
glimpse(track)
summary(track)
names(track)
view(track)
unique(track$cell.line)

#compare mean.speed between cell lines
ggplot(track, aes(x=cell.line,y=mean.speed))+
  geom_boxplot()+
  stat_compare_means()

# CORRELATIONS IN MANUAL TRACK DATA ----

#examine whether track.length and mean.speed are correlated
cor.test(track$track.length,track$mean.speed,method="spearman")

#calculate all pairwise correlations
track.correlations <- 
  track |>
  correlate(method="spearman")
#see what we have
head(track.correlations)

#Adjust the name of the first column to "Variable1"
names(track.correlations)[1]="Variable1"

#simplify the data with pivot_longer
track.correlations.pivot <- 
  track.correlations |> 
  pivot_longer(-Variable1, names_to = "Variable2", values_to = "corr.coeff")

#examine what we have
head(track.correlations.pivot)

#plot data in the track.correlations.pivot table
#using geom_tile (for coloured boxes) and geom_text (to show the correlation coefficient values) 
ggplot(track.correlations.pivot, aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#pipe track.correlations.pivot data frame into filter
track.correlations.pivot |>

  #filter out the TID and LID columns
  filter(Variable1 != 'LID') |>
  filter(Variable2 != 'LID') |>
  filter(Variable1 != 'TID') |>
  filter(Variable2 != 'TID') |>
  
  #now we put the plotting code here
  ggplot(aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#and voila! no more TID and LID columns


#filter the data to only show correlations with an absolute value greater than 0.25
track.correlations.pivot |>
  filter(abs(corr.coeff) > 0.25) |>
#now we put the plotting code here 
  ggplot(aes(Variable1, Variable2)) +
  geom_tile(aes(fill = corr.coeff)) +
  geom_text(aes(label = round(corr.coeff, 1))) +
  scale_fill_gradient(low = "white", high = "red")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# PCA PLOT FOR MANUAL TRACK DATA ----

# prepare data for PCA

track.data.for.pca <- track |>
  #select the numeric columns
  select(cell.line, track.duration, track.length, euclidean.distance, meandering.index, mean.speed) |>
  # remove rows with NA values
  drop_na()


#Now we calculate PCA coordinates with the prcomp() function:

pca.result.track <- track.data.for.pca |>
  select(-cell.line) |>  # remove the group categories
  prcomp()               # performs the PCA

# extract PCA scores and combine with clone and replicate info
pca.scores.track <- as.data.frame(pca.result.track$x) |>
  bind_cols(track.data.for.pca |> select(cell.line))

# no need to downsample
nrow(pca.scores.track)

# make the plot
pca.plot.track <- ggplot(pca.scores.track, 
  aes(x = PC1, y = PC2, color = cell.line, fill = cell.line)) +
  geom_point(size = 4)+
  #stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  theme_classic2()

# view and then save the plot
pca.plot.track
ggsave("BIO00066I-workshop3-track-pca-plot.png", width = 6, height = 4)


# QUESTIONS ----

# Which is the most useful PCA plot?
# Cell shape data? Livecyte cell movement data? Manual track data?
# Should we make a PCA plot with Livecyte cell movement data and cell shape data together? 

## EXTRA: INVESTIGATING PCA RESULTS ----

### EXTRA: EXAMINETHE VARIANCE EXPLAINED BY EACH PC ----

# Calculate manually from standard deviations
variance <- pca.result.track$sdev^2
percent.variance <- variance / sum(variance) * 100

# View the percentages
percent.variance

# Or create a nice data frame
variance.explained <- data.frame(
  PC = paste0("PC", 1:length(percent.variance)),
  Variance = variance,
  Percent = percent.variance,
  Cumulative = cumsum(percent.variance)
)
view(variance.explained)

# Visualize with a scree plot
barplot(percent.variance, 
        names.arg = paste0("PC", 1:length(percent.variance)),
        xlab = "Principal Component",
        ylab = "% Variance Explained")


### EXTRA: VIEW PCA LOADINGS ----

# View the loadings
# PCA loadings show the contribution of each original variable to each principal component
# Each column is a PC, each row is an original variable (track.duration, track.length, etc.)
# The values are weights/coefficients that tell you:
#   - HOW MUCH each variable contributes to that PC
#   - DIRECTION: positive or negative relationship
#   - MAGNITUDE: larger absolute values = stronger contribution
# For example, if track.length has a loading of 0.8 on PC1, 
# it means track.length strongly influences PC1 in a positive direction
pca.result.track$rotation

# Convert to data frame for easier viewing
loadings.df <- as.data.frame(pca.result.track$rotation)
view(loadings.df)

# Visualize loadings with a heatmap
# The heatmap shows which variables (rows) contribute most to each PC (columns)
# Colours indicate strength and direction:
#   - Red/dark = strong positive loading (variable increases as PC increases)
#   - Blue/light = strong negative loading (variable decreases as PC increases)
#   - White/neutral = weak loading (variable doesn't contribute much to that PC)
# This helps you interpret what each PC represents biologically

# ggplot alternative for loadings heatmap
loadings.for.plot <- as.data.frame(pca.result.track$rotation) |>
  rownames_to_column("Variable")  # convert row names to a column

# Reshape for ggplot
loadings.long <- loadings.for.plot |>
  pivot_longer(-Variable, names_to = "PC", values_to = "Loading")

# Plot with ggplot
loadings.plot <- ggplot(loadings.long, aes(x = PC, y = Variable, fill = Loading)) +
  geom_tile(color = "white") +
  geom_text(aes(label = round(Loading, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0) +
  theme_minimal() +
  labs(title = "PCA Loadings Heatmap",
       x = "Principal Component",
       y = "Variable")

loadings.plot

# Alternative: use pheatmap package (uncomment if preferred)
#install.packages("pheatmap")  # if you don't have it already
#library(pheatmap)
#pheatmap(pca.result.track$rotation[, 1:2])  # first 2 PCs

# View summary of PCA
summary(pca.result.track)






