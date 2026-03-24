# #install new packages for the animated plots
# install.packages("gganimate")
# install.packages("gifski")
# 

# load libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)
library(gifski)

# read the standard curve data
# skipping the first three lines, and specifying the first sheet
pnp.calibration <- read_excel("raw-data/BIO00066I-pNP-example-data-2025-26.xlsx", sheet=1, skip=3)

#check what we have
view(pnp.calibration)

pnp.calibration.pivot <- 
  pnp.calibration |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNP.conc, names_to = "rep", values_to = "absorbance")
view(pnp.calibration.pivot)

pnp.calibration.pivot <- relocate(pnp.calibration.pivot, absorbance)


#plot, saving the plot in an object called 'pNP.plot'
pNP.plot <- pnp.calibration.pivot |>
  ggplot(aes(x=absorbance, y=pNP.conc))+
  geom_point()+
  geom_smooth(method="lm")

#show the plot
pNP.plot


#save as a jpeg
#has a 'date stamp' (2025-03-21) in the name
ggsave("BIO66I-pNP-standard-curve.jpeg",pNP.plot,width=7,height=7)


# make a linear model so we can predict pNP concentration from absorbance values
linear_model <- lm(pNP.conc ~ absorbance , data = pnp.calibration.pivot)


# read in experimental pNP data for unknown samples, skipping the first line
pnp.experimental <- read_excel("raw-data/BIO00066I-pNP-example-data-2025-26.xlsx", sheet=2, skip=1)

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

#make the plot
ggplot(data=pnp.experimental, aes(x=day, y=predicted.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())


# first check that data columns we have 
names(pnp.experimental)

#make the plot using the normalised.pNP.concs as y values
ggplot(data=pnp.experimental, aes(x=day, y=normalised.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())


#load data from a URL
points <- read_csv(url("https://djeffares.github.io/BIO66I/data/processed-points-data.csv"),
                 col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
)

#check what we have
glimpse(points)
head(points)


points |>
  ggplot(aes(x=clone,y=log10(velocity)))+
  geom_violin()+
  stat_compare_means()

?glimpse

points |>
  ggplot(aes(x=normalised.x,y=normalised.y,colour=clone))+
  geom_point(size=1,alpha=0.2)+
  facet_wrap(~LID)+
  geom_hline(yintercept = 0,colour = "grey",linetype = "dashed")+
  geom_vline(xintercept = 0,colour = "grey",linetype = "dashed")+
  theme_classic2()

#If we want to show just one clone add this line after 'points |>'
#filter(clone == "A")|>


# plot the paths taken by the cells in lineage 4 of clone A
#colouring by tracking ID (TID)
points |>
  filter(clone == "A" & LID==4) |>
  # Convert TID to factor with numeric ordering
  mutate(TID = factor(TID, levels = sort(as.numeric(as.character(unique(TID)))))) |>
  ggplot(aes(x=normalised.x,y=normalised.y,colour=time))+
  geom_point(size=1)+
  facet_wrap(~TID)


# make the static plot  
static.plot <- points |>
  filter(clone == "B", LID == 1) |>
  ggplot(aes(x=normalised.x, y=normalised.y, colour=as.numeric(TID)))+
  geom_point(size=1, pch=1, lwd=2)+
  geom_hline(yintercept = 0, colour = "grey", linetype = "dashed")+
  geom_vline(xintercept = 0, colour = "grey", linetype = "dashed")+
  theme_classic2()+
  labs(colour = "Tracking ID")
# view the static plot  
static.plot


#set up the animation 
animated.plot <- static.plot +
  transition_time(time) +
  shadow_mark(past = T, future=F, alpha=0.5)
animated.plot


# #render the animation, saving it in an object called 'rendered.animation'
# rendered.animation <- animate(
#   animated.plot,
#   width = 600,
#   height = 600,
#   renderer = gifski_renderer()
# )
# rendered.animation

# 
# #save the animation as a gif
# #make sure you use a sensible file name
# anim_save("cloneB.lineage2.gif", rendered.animation)

# ?glimpse

# #load the data
# pnp.calibration<-read_excel("raw-data/alkaline-phosphatase-activity-assay-real-data.xlsx",sheet=2)
# 
# #rename the 'differentiated' column 'induced', which describes it better
# names(pnp.calibration)[3]<-'induced'

# #set day, clone and induced columns to be factors
# pnp.calibration$day <-as.factor(pnp.calibration$day)
# pnp.calibration$clone <-as.factor(pnp.calibration$clone)
# pnp.calibration$induced <-as.factor(pnp.calibration$induced)
# 
# #this is what we have:
# head(pnp.calibration)

# pnp.calibration.pivot<-pnp.calibration |>
#   pivot_longer(cols=!c(day,clone,induced), names_to = "rep", values_to = "absorbance")
# 
# #this is what we have now
# head(pnp.calibration.pivot)

# # make a plot of the absorbance for each clone
# ggplot(pnp.calibration.pivot, aes(x = clone, y = absorbance, colour = day))+
#   geom_boxplot()+
#   theme_classic()

# ggplot(pnp.calibration.pivot, aes(x = clone, y = absorbance, colour = day))+
#   geom_boxplot()+
#   theme_classic()+
#   facet_wrap(~induced)

# #we save the result in an object called aov.result.additive
# aov.result.add <- aov(absorbance ~ day + clone + induced, data = pnp.calibration.pivot)

# summary(aov.result.add)

# #run the ANOVA
# aov.result.mult  <- aov(absorbance ~ day * clone * induced, data = pnp.calibration.pivot)
# 
# #to view the results
# summary(aov.result.mult)
