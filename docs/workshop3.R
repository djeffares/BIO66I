################################################################################
# WORKSHOP 3: CELL MOVEMENT DATA -----------------------------------------------
################################################################################

# SET UP AND LIBRARY LOADING ---------------------------------------------------

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#load the corr library: this is for examining correlations between many metrics
library(corrr)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)


# LOADING LIVECYTE DATA --------------------------------------------------------

# This is data from the Livecyte microscope that contains both cell shape and cell movement data. We will focus on the movement data for this workshop, but you can explore the shape data if you like.

# Read the automated Livecyte data
cells <-read_tsv(
  url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
    col_types = cols(
    clone = col_factor(),
    replicate = col_factor(),
    tracking.id=col_factor(),
  lineage.id=col_factor()
  )
)

#examine what we have in the data frame
names(cells)

#select only the columns we need
cell.movement.data <- select(cells,
        clone,
        replicate,
        displacement, 
        track.length, 
        instantaneous.velocity
)

#check that we have
names(cell.movement.data)
 
#get a simple summary, using summary and also glimpse
# summary(cell.movement.data)
# glimpse(cell.movement.data)

# PLOTTING LIVECYTE DATA -------------------------------------------------------

names(cell.movement.data)
#instantaneous.velocity - geom_violin

# how to output a citation
print(citation("tidyverse"), style = "text")

instantaneous.velocity.plot <- 
  ggplot(cell.movement.data,aes(
    x=clone,y=instantaneous.velocity,colour=clone))+
    geom_violin(alpha=0.5)+
    stat_compare_means()


track.length.plot <- 
  ggplot(cell.movement.data,
  aes(x=clone,y=log10(track.length),colour=clone))+
    geom_violin(alpha=0.5)+
    stat_compare_means()

track.length.and.instantaneous.velocity.plots <- 
  ggarrange(plot1,plot2,plot3,plot4
          track.length.plot, 
          ncol=2, nrow=2)
track.length.and.instantaneous.velocity.plots

ggsave("BIO00066I-workshop3-livecyte-movement-plots.png", 
       track.length.and.instantaneous.velocity.plots, 
       width = 8, height = 4)

# make the plot
pca.plot <- ggplot(pca.scores.sample, 
  aes(x = PC1, y = PC2, color = clone, fill = clone)) +
  geom_point(alpha = 0.1, size = 4)+
  stat_ellipse(geom = "polygon", 
  alpha = 0.1, linewidth = 1.2, level = 0.99) +
  scale_color_manual(values = c("cloneA" = "red", "cloneB" = "blue"))+
  theme_classic2()
pca.plot


# SUMMARY OF LIVECYTE DATA ----

# make clone A and clone B data frames
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
clone.comparisons
# PCA WITH LIVECYTE DATA ----

# prepare data for PCA

cell.data.for.pca <- cell.movement.data |>
  drop_na() # remove rows with NA values

# calculate PCA coordinates 
pca.result <- cell.data.for.pca |>
  select(-clone, -replicate) |>   # removes the group categories
  prcomp()                        # performs the PCA

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

# LOAD MANUAL TRACKING DATA ----

#load the manual tracking data
track <-read_tsv(url("https://djeffares.github.io/BIO66I/data/A1-and-B2-tracking.data.2025-02-27.tsv"))

# PLOTTING MANUAL TRACKING DATA ----

#compare mean.speed between cell lines
ggplot(track, aes(x=cell.line,y=mean.speed))+
  geom_boxplot()+
  stat_compare_means()

#examine whether track.length and mean.speed are correlated
cor.test(track$track.length,track$mean.speed,method="spearman")

# CORRELATIONS WITHIN MANUAL TRACKING DATA ----

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

# prepare data for PCA
track.data.for.pca <- track |>
  #select the numeric columns
  select(cell.line, track.duration, track.length, euclidean.distance, meandering.index, mean.speed) |>
  # remove rows with NA values
  drop_na()


# calculate PCA coordinates
pca.result.track <- track.data.for.pca |>
  select(-cell.line) |>  # remove the group categories
  prcomp()               # performs the PCA

# extract PCA scores and combine with clone and replicate info
pca.scores.track <- as.data.frame(pca.result.track$x) |>
  bind_cols(track.data.for.pca |> select(cell.line))

# make the plot
pca.plot.track <- ggplot(pca.scores.track, 
  aes(x = PC1, y = PC2, color = cell.line, fill = cell.line)) +
  geom_point(size = 4)+
  stat_ellipse(geom = "polygon", alpha = 0.1, linewidth = 1.2) +
  theme_classic2()

# view and then save the plot
pca.plot.track
ggsave("BIO00066I-workshop3-track-pca-plot.png", width = 6, height = 4)

# multi part livecyte data plot
# plot 1: displacement
displacement.plot <- cells |> 
  ggplot(aes(x=clone,y=log10(displacement)))+
  geom_violin(alpha=0.5)+
  stat_compare_means()

# plot 2: track.length
track.length.plot <- cells |> 
  ggplot(aes(x=clone,y=log10(track.length)))+
  geom_violin(alpha=0.5)+
  stat_compare_means()

# plot 3: width
width.plot <- cells |> 
  ggplot(aes(x=clone,y=log10(width)))+
  geom_violin(alpha=0.5)+
  stat_compare_means()

# plot 4: sphericity
sphericity.plot <- cells |> 
  ggplot(aes(x=clone,y=log10(sphericity)))+
  geom_violin(alpha=0.5)+
  stat_compare_means()

# arrange the plots into a 2x2 grid
combined.livecyte.plot <- ggarrange(
  displacement.plot, 
  track.length.plot, 
  width.plot, 
  sphericity.plot, 
  ncol=2, nrow=2,
  labels = c("A", "B", "C", "D"))

# save the combined plot
ggsave("BIO00066I-workshop3-combined-livecyte-plot.png", 
       combined.livecyte.plot, width = 8, height = 6)


# # Read the automated Livecyte data
# cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
#                  col_types = cols(
#                    clone = col_factor(),
#                    replicate = col_factor(),
#                    tracking.id=col_factor(),
#                    lineage.id=col_factor()
#                  )
# )
# 
# # prepare data for PCA with both cell shape and movement data
# cells.for.pca.alldata <- cells |>
#   #select the numeric columns, including the cell shape data and also the cell movement data
#   select(clone, replicate,
#       # shape data
#       volume, mean.thickness, radius, area, sphericity, length, width, orientation, dry.mass,
#       # movement data
#       length.to.width, displacement, track.length, instantaneous.velocity) |>
#   # remove rows with NA values
#   drop_na()
# 
# 
# #run PCA
# pca.result <- cells.for.pca.alldata  |>
#   select(-clone, -replicate) |>  # remove the group categories
#   prcomp()                        # performs the PCA
# 
# # extract PCA scores and combine with clone and replicate info
# pca.scores <- as.data.frame(pca.result$x) |>
#   bind_cols(cells.for.pca.alldata |> select(clone, replicate))
# 
# # down sample the PCA scores to 3000 cells per clone
# pca.scores.sample <- pca.scores |>
#   group_by(clone) |>
#   slice_sample(n = 3000) |>
#   ungroup()
# 
# # create PCA1, PC2 plot
# pca.plot <- ggplot(pca.scores.sample,
#   aes(x = PC1, y = PC2, color = clone, fill = clone)) +
#   geom_point(alpha = 0.5, size = 2)+
#   stat_ellipse(geom = "polygon", alpha = 0.05, linewidth = 1.2)+
#   theme_classic2()
# 
# # view and save the plot
# pca.plot
# ggsave("BIO00066I-workshop3-pca-alldata-plot.png", width = 6, height = 4)

# # make a scree plot from pca.result
# screeplot(pca.result.track, type = "lines")
# 
# # Calculate manually from standard deviations
# variance <- pca.result.track$sdev^2
# percent.variance <- variance / sum(variance) * 100
# 
# # Or create a nice data frame
# variance.explained <- data.frame(
#   PC = paste0("PC", 1:length(percent.variance)),
#   Variance = variance,
#   Percent = percent.variance,
#   Cumulative = cumsum(percent.variance)
# )
# 
# # Visualize with a scree plot
# barplot(percent.variance,
#         names.arg = paste0("PC", 1:length(percent.variance)),
#         xlab = "Principal Component",
#         ylab = "% Variance Explained")


#Citations
#Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R,
#K, Vaughan D, Wilke C, Woo K, Yutani H (2019). “Welcome to the
##Bache SM, Müller K, Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi
###Grolemund G, Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E,
#tidyverse.” _Journal of Open Source Software_, *4*(43), 1686.
##doi:10.21105/joss.01686 <https://doi.org/10.21105/joss.01686>.

