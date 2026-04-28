#  CELL SHAPE DATA ----
# Initial analysis ----
# load packages 
library(ggplot2)
library(ggpubr)
library(dplyr)
library(tidyverse)
library(corrr)
library(readxl)
install.packages("openxlsx")
library(openxlsx)
# load tsv file from a website 
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/data/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)
# exploring the data 
#look at the data, like an excel table:
view(cells)
#look at data 
summary(cells)
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
# display structure
str(cells)
# visualise the data in a plot 
width.boxplot <- ggplot(cells,aes(x=clone,y=width,fill=replicate))+
  geom_boxplot()
# Test if cloneA and cloneB have statistically different widths
# Use  Wilcoxon rank sum test, which does not assume normal distribution
wilcox.test(width ~ clone, data = cells)
ggplot(cells,aes(x=clone,y=width))+
  geom_boxplot()+
  stat_compare_means()
str(cells)  #shows the type of each column
# Ensure consistent clone labels
cells <- cells %>%
  mutate(clone = factor(clone,
                        levels = c("cloneA","cloneB"),
                        labels = c("Clone A","Clone B")))
# p-value formatting function - to round up my p value 
format_p_value <- function(p) {
  ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))
}

# summary function for median and IQR
median_iqr <- function(x) {
  med <- median(x, na.rm = TRUE)
  q1  <- quantile(x, 0.25, na.rm = TRUE)
  q3  <- quantile(x, 0.75, na.rm = TRUE)
  sprintf("%.2f (%.2f–%.2f)", med, q1, q3)
}
# Making clone comparison table ----
# make cloneA and cloneB data frames
cloneA.data <- cells |> filter(clone == "Clone A")
cloneB.data <- cells |> filter(clone == "Clone B")
#get the names of the numeric columns
numeric.columns <- cells |> 
  select(where(is.numeric)) |> 
  names()
# make a table to look at clone comparisons for shape parameters 
#create empty data frame
clone.comparisons <- data.frame(
  Variable = character(),
  `Clone A Median (IQR)` = character(),
  `Clone B Median (IQR)` = character(),
  `Clone A/Clone B Median Ratio` = numeric(),
  `p value` = numeric()
)

#loop through each numeric column
for(column.name in numeric.columns) {
  
  #calculate median for clone A
  cloneA.median <- median_iqr(cloneA.data[[column.name]])
  
  #calculate median for clone B  
  cloneB.median <- median_iqr(cloneB.data[[column.name]])
  
  #calculate the median ratio (cloneA.median / cloneB.median)
  ratio <- median(cloneA.data[[column.name]], na.rm = TRUE) / median(cloneB.data[[column.name]], na.rm = TRUE)
  
  #run wilcox test comparing the two clones for this variable
  test.result <- wilcox.test(cells[[column.name]] ~ cells$clone)
  
  #extract the p-value
  p.val <- format_p_value(test.result$p.value)
  
  #add this row to our results table
  new.row <- data.frame(
    Variable = column.name,
    `Clone A Median (IQR)` = cloneA.median,
    `Clone B Median (IQR)` = cloneB.median,
    `Clone A/Clone B Median Ratio` = signif(ratio,2),
    `p value` = p.val
  )
  
  #add the new.row to the clone.comparisons data frame
  clone.comparisons <- rbind(clone.comparisons, new.row)
}
view(clone.comparisons)
write.xlsx(clone.comparisons, "shape.plots/clone.comparisons.table.shape.xlsx")
# Shape parameters clone and replicate comparisons-----
# multi part livecyte data plot
# Define a custom color palette to use for all plots
# heres where i need help ------
# common theme (12 or 14 pt)
base_theme <- theme_classic(base_size = 14)


my_colors <- c(
  "Clone A.1" = "#F4A6B8",
  "Clone A.2" = "#EC6A8A",
  "Clone A.3" = "#D7265E",
  "Clone B.1" = "#A6C8FF",
  "Clone B.2" = "#5DA9E9",
  "Clone B.3" = "#1E5AA8"
)

# plot 1: length to width
clone_colors <- c("Clone A" = "#E78AC3", "Clone B" = "#6BAED6")

cells$clone_rep <- interaction(cells$clone, cells$replicate)

length.to.width.png <- ggplot(cells, aes(x = clone, y = length.to.width, fill = clone_rep)) +
  geom_violin(alpha = 0.7) +
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA,
               position = position_dodge(width=0.9)) +
  ylab("Length to Width Ratio") +
  scale_fill_manual(values = my_colors) +
  theme_classic(base_size = 14) +
  annotate(
    "text",
    x = 1.5,
    y = max(cells$length.to.width, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", clone.comparisons$p.value[clone.comparisons$Variable=="length.to.width"]),
    size = 4
  )

# plot 2: cell area
area.png <- ggplot(cells,aes(x=clone,y=area,fill=clone_rep))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA, position = position_dodge(width=0.9))+
  ylab("Area (μm2) ")+
  scale_fill_manual(values = my_colors) + # Added custom colors
  base_theme +
  theme(axis.title.x = element_blank()) +
  annotate(
    "text",
    x = 1.5,
    y = max(cells$area, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", clone.comparisons$p.value[clone.comparisons$Variable=="area"]),
    size = 4
  )

# plot 3: mean thickness
mean.thickness.png <- ggplot(cells,aes(x=clone,y=mean.thickness,fill=clone_rep))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA, position = position_dodge(width=0.9))+
  ylab("Mean Thickness (μm)")+
  scale_fill_manual(values = my_colors) + # Added custom colors
  base_theme +
  theme(axis.title.x = element_blank()) +
  annotate(
    "text",
    x = 1.5,
    y = max(cells$mean.thickness, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", clone.comparisons$p.value[clone.comparisons$Variable=="mean.thickness"]),
    size = 4
  )

# plot 4: width
width.png <- ggplot(cells,aes(x=clone,y=width,fill=clone_rep))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA, position = position_dodge(width=0.9))+
  ylab("Width (μm) ")+
  scale_fill_manual(values = my_colors) + # Added custom colors
  base_theme +
  theme(axis.title.x = element_blank()) +
  annotate(
    "text",
    x = 1.5,
    y = max(cells$width, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", clone.comparisons$p.value[clone.comparisons$Variable=="width"]),
    size = 4
  )

# arrange the plots into a 2x2 grid
combined.shape.png <- ggarrange(
  length.to.width.png,
  area.png, 
  mean.thickness.png, 
  width.png, 
  ncol=2, nrow=2,
  labels = c("A", "B", "C", "D"),
  common.legend = TRUE,      # This creates one shared key
  legend = "right"           # This places the key on the right
)

# add shared x-axis label for the whole figure
combined.shape.png <- annotate_figure(
  combined.shape.png,
  bottom = text_grob("Clone", face = "plain", size = 14)
)


# View the final plot

combined.shape.png

# save as Pdf at 300 dpi
ggsave(
  filename = "shape.plots/combined.shape.png",
  plot = combined.shape.png,
  width = 10,
  height = 8,
  dpi = 300
)

# CELL MOVEMENT DATA ----
#sometimes the automated data is bad therfor we are using the manual tracking
#load the manual tracking data
track <-read_tsv(url("https://djeffares.github.io/BIO66I/data/A1-and-B2-tracking.data.2025-02-27.tsv"))
summary(track)

# Correlations within track movement data ----

#calculate all pairwise correlations -  help select parameters to investigate 

track.correlations <- 
  track %>%
  select(-TID, -LID) %>%
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

# remove the unneccessary stuff LID and TID 
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



# Making clone comparison table ----
# make cloneA and cloneB data frames
cell.line.A1.data <- track |> select(-TID, -LID) |> filter(cell.line == "A1")
cell.line.B2.data <- track |> select(-TID, -LID) |> filter(cell.line == "B2")
#Manual tracking data plot ----

track.numeric.columns <- cell.line.A1.data |> 
  select(where(is.numeric)) |> 
  names()

track.clone.comparisons <- data.frame(
  Variable = character(),
  `Cell Line A1 Median (IQR)` = character(),
  `Cell Line B2 Median (IQR)` = character(),
  `A1/B2 Median Ratio` = numeric(),
  `p value` = numeric()
)

#loop through each numeric column
for(column.name in track.numeric.columns) {
  
  #calculate median for clone A
  lineA1.median <- median_iqr(cell.line.A1.data[[column.name]])
  
  #calculate median for clone B  
  lineB2.median <- median_iqr(cell.line.B2.data[[column.name]])
  
  #calculate the median ratio (cloneA.median / cloneB.median)
  ratio <- median(cell.line.A1.data[[column.name]], na.rm = TRUE) / median(cell.line.B2.data[[column.name]], na.rm = TRUE)
  
  #run wilcox test comparing the two clones for this variable
  test.result <- wilcox.test(track[[column.name]] ~ track$cell.line)
  
  #extract the p-value
  p.val <- format_p_value(test.result$p.value)
  
  #add this row to our results table
  new.row <- data.frame(
    Variable = column.name,
    `Cell Line A1 Median (IQR)` = lineA1.median,
    `Cell Line B2 Median (IQR)` = lineB2.median,
    `A1/B2 Median Ratio` = signif(ratio,2),
    `p value` = p.val
  )
  
  #add the new.row to the clone.comparisons data frame
  track.clone.comparisons <- rbind(track.clone.comparisons, new.row)
}
view(track.clone.comparisons)

# could u help me get this to just say clone a and b instead of the a1 and b2----

line_colors <- c("A1" = "#F8766D", "B2" = "#00BFC4")

# plot 1: euclidean distance
euclidean.distance.png <- ggplot(track,aes(x=cell.line,y=euclidean.distance,fill=cell.line))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA)+
  ylab("Euclidean Distance (µm)")+
  scale_fill_manual(values = line_colors)+
  base_theme +
  theme(axis.title.x = element_blank())  +
  annotate(
    "text",
    x = 1.5,
    y = max(track$euclidean.distance, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", track.clone.comparisons$p.value[track.clone.comparisons$Variable=="euclidean.distance"]),
    size = 4
  )

# plot 2: track length
track.length.png <- ggplot(track,aes(x=cell.line,y=track.length,fill=cell.line))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA)+
  ylab("Track Length (µm)")+
  scale_fill_manual(values = line_colors)+
  base_theme +
  theme(axis.title.x = element_blank())  +
  annotate(
    "text",
    x = 1.5,
    y = max(track$track.length, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", track.clone.comparisons$p.value[track.clone.comparisons$Variable=="track.length"]),
    size = 4
)
# plot 3: mean speed
mean.speed.png <- ggplot(track,aes(x=cell.line,y=mean.speed,fill=cell.line))+
  geom_violin()+
  geom_boxplot(width=0.1, alpha=0.5, outlier.shape = NA)+
  ylab("Mean Speed (µm/sec)")+
  scale_fill_manual(values = line_colors)+
  base_theme +
  theme(axis.title.x = element_blank())  +
  annotate(
    "text",
    x = 1.5,
    y = max(track$mean.speed, na.rm = TRUE) * 1.1,
    label = paste0("Wilcoxon p = ", track.clone.comparisons$p.value[track.clone.comparisons$Variable=="mean.speed"]),
    size = 4
)
# arrange the plots into a 1x3 grid
combined.movement.png <- ggarrange(
  euclidean.distance.png,
  track.length.png, 
  mean.speed.png, 
  ncol=3, nrow=1,
  labels = c("A", "B", "C"),
  common.legend = TRUE,
  legend = "right"
)

combined.movement.png <- annotate_figure(
  combined.movement.png,
  bottom = text_grob("Clone", face = "plain", size = 14)
)
# save the combined plot
ggsave("movement.plots/combined.movement.png", width = 8, height = 6)


# OSTEOGENISIS EXPERIMENT ANALYSIS -----

# Makeing standard curve ------
# read the standard curve data
# skipping the first three lines, and specifying the first sheet
#pnp.calibration <- read_excel("data.raw/pNP-model-data.xlsx", sheet=1, skip=3)
#DJ
pnp.calibration <- read_excel("data.raw/pNP-model-data.xlsx", sheet=1, skip=3)


#check what we have
view(pnp.calibration)
getwd()

# now we will symplify the data 
pnp.calibration.pivot <- 
  pnp.calibration |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNP.conc, names_to = "rep", values_to = "absorbance")
view(pnp.calibration.pivot)

# now im reordering the columns 
pnp.calibration.pivot <- relocate(pnp.calibration.pivot, absorbance)
#plot, saving the plot in an object called 'pNP.plot'
pNP.plot <- pnp.calibration.pivot |>
  ggplot(aes(x=absorbance, y=pNP.conc))+
  geom_point()+
  geom_smooth(method="lm")

#show the plot
pNP.plot

#save as a png
#has a 'date stamp' (2025-03-21) in the name
ggsave("BIO66I-pNP-standard-curve.png",pNP.plot,width=7,height=7)

# Linear model so we can predict pNP concentration -------

# make a linear model so we can predict pNP concentration from absorbanceance values
linear_model <- lm(pNP.conc ~ absorbance , data = pnp.calibration.pivot)

# read in experimental pNP data for unknown samples, skipping the first line
#pnp.experimental <- read_excel("data.raw/pNP-model-data.xlsx", sheet=2, skip=1)
#DJ
pnp.experimental <- read_excel("data.raw/pNP-model-data.xlsx", sheet=2, skip=1)

#check what we have
glimpse(pnp.experimental)
pnp.experimental <- pnp.experimental |> 
  rowwise() |> 
  mutate(absorbance=mean(c(absorb.rep1,absorb.rep2,absorb.rep3),na.rm=T)) 
#calculate predicted pNP concentration using the linear model
predictions.from.lm <- predict(linear_model,pnp.experimental)

#Add these predictions from the linear model (predictions.from.lm)
#As a new column
pnp.experimental$predicted.pNP.concs = predictions.from.lm

#check what we have
glimpse(pnp.experimental)
##divide the predicted.pNP.concs column by the DNA_ug_per_ml column
pnp.experimental <- pnp.experimental |> 
  mutate(normalised.pNP.concs = predicted.pNP.concs / DNA_ug_per_ml)

# check what we have now
names(pnp.experimental)
pnp.experimental$day <-as.factor(pnp.experimental$day)
pnp.experimental$clone <-as.factor(pnp.experimental$clone)
pnp.experimental$differentiated <-as.factor(pnp.experimental$differentiated)

# Make the plot ------

pnp.experiment.plot <- ggplot(data=pnp.experimental, aes(x=day, y=predicted.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge()) +
  xlab("Time (Days)") +
  ylab("Predicted pNP Concentration") +
  
  labs(fill = "Clone / Differentiation Status") +
  
  theme_classic()
pnp.experiment.plot
ggsave("plots/pnp.experiment.plot.png",width = 6, height = 4)


#DJ

pnp.calibration.pivot

# make a plot of the absorbance for each clone
ggplot(pnp.calibration.pivot, aes(x = clone, y = absorbance, colour = day))+
  geom_boxplot()+
  theme_classic()
view(pnp.calibration.pivot)



# this is where i need help -------
#load the data
pnp.calibration<-read_excel("data.raw/pNP-model-data.xlsx",sheet=2)
view(pnp.calibration)
#rename the 'differentiated' column 'induced', which describes it better
names(pnp.calibration)[3]<-'induced'
#set day, clone and induced columns to be factors
pnp.calibration$day <-as.factor(pnp.calibration$day)
pnp.calibration$clone <-as.factor(pnp.calibration$clone)
pnp.calibration$induced <-as.factor(pnp.calibration$induced)

#this is what we have:
head(pnp.calibration)
pnp.calibration.pivot<-pnp.calibration |> 
  pivot_longer(cols=!c(day,clone,induced), names_to = "rep", values_to = "absorbance")

#this is what we have now
head(pnp.calibration.pivot)
# make a plot of the absorbance for each clone
ggplot(pnp.calibration.pivot, aes(x = clone, y = absorbance, colour = day))+
  geom_boxplot()+
  theme_classic()
ggplot(pnp.calibration.pivot, aes(x = clone, y = absorbance, colour = day))+
  geom_boxplot()+
  theme_classic()+
  facet_wrap(~induced)
#we save the result in an object called aov.result.additive
aov.result.add <- aov(absorbance ~ day + clone + induced, data = pnp.calibration.pivot)
summary(aov.result.add)
#run the ANOVA
aov.result.mult  <- aov(absorbance ~ day * clone * induced, data = pnp.calibration.pivot)

#to view the results
summary(aov.result.mult)
save.image(file in here .rda in quotes)