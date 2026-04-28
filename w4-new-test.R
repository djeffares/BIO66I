# load libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)
library(gifski)

# read the standard curve data
# skipping the first three lines, and specifying the first sheet
pnp.calibration.data <- read_excel("raw-data/BIO00066I-pNP-example-data-2025-26.xlsx", sheet=1, skip=3)

#check what we have
glimpse(pnp.calibration.data)

pnp.calibration.data.pivot <- 
  pnp.calibration.data |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNP.conc, names_to = "rep", values_to = "absorbance")


pnp.calibration.data.pivot <- relocate(pnp.calibration.data.pivot, absorbance)
glimpse(pnp.calibration.data.pivot)


#plot, saving the plot in an object called 'pnp.standard.curve.plot'
pnp.standard.curve.plot <- pnp.calibration.data.pivot |>
  ggplot(aes(x=absorbance, y=pNP.conc))+
  geom_point()+
  geom_smooth(method="lm")

#plot, saving the plot in an object called 'pnp.standard.curve.plot'
pnp.standard.curve.plot <- pnp.calibration.data.pivot |>
  ggplot(aes(x=absorbance, y=pNP.conc))+
  geom_point()+
  geom_smooth(method="lm")

#show the plot
pnp.standard.curve.plot

#save as a jpeg
#has a 'date stamp' (2025-03-21) in the name
ggsave("pnp-standard-curve.jpeg",pnp.standard.curve.plot,width=7,height=7)

# make a linear model so we can predict pNP concentration from absorbance values
pnp.linear.model <- lm(pNP.conc ~ absorbance , data = pnp.calibration.data.pivot)

# read in experimental pNP data for unknown samples, skipping the first line
pnp.experimental.data <- read_excel("raw-data/BIO00066I-pNP-example-data-2025-26.xlsx", sheet=2, skip=1)

#check what we have
glimpse(pnp.experimental.data)

pnp.experimental.data <- pnp.experimental.data |> 
  rowwise() |> 
  mutate(absorbance=mean(c(absorb.rep1,absorb.rep2,absorb.rep3),na.rm=T)) 


#calculate predicted pNP concentration using the linear model
predicted.pnp.conc <- predict(pnp.linear.model,pnp.experimental.data)

#add predicted pNP concentration as a new column
pnp.experimental.data$predicted.pnp.conc = predicted.pnp.conc

#check what we have
glimpse(pnp.experimental.data)

##divide the predicted.pnp.conc column by the DNA_ug_per_ml column
pnp.experimental.data <- pnp.experimental.data |> 
  mutate(normalised.pnp.conc = predicted.pnp.conc / DNA_ug_per_ml)

# check what we have now
names(pnp.experimental.data)

pnp.experimental.data$day <-as.factor(pnp.experimental.data$day)
pnp.experimental.data$clone <-as.factor(pnp.experimental.data$clone)
pnp.experimental.data$differentiated <-as.factor(pnp.experimental.data$differentiated)

#make the plot
ggplot(data=pnp.experimental.data, aes(x=day, y=predicted.pnp.conc,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())

#make the plot using the normalised.pnp.conc as y values
ggplot(data=pnp.experimental.data, aes(x=day, y=normalised.pnp.conc,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())+
  xlab("day")+
  ylab("normalised pNP concentration")

#now lets save this plot
ggsave("pnp_experimental_data_barplot.jpeg",width=7,height=7)


#output the pnp.experimental.data to a tab-separated file (TSV)
write_tsv(pnp.experimental.data, file = "pnp_experimental_data_normalised.tsv")


## supp ----
# first, we load our normalised experimental data again
# with our predicted values, normalised for DNA content
pnp.experimental.data.normalised <- read_tsv("pnp_experimental_data_normalised.tsv")

#rename the 'differentiated' column 'induced', which describes it better
names(pnp.experimental.data.normalised)[3]<-'induced'


#set day, clone and induced columns to be factors
pnp.experimental.data.normalised$day <-as.factor(pnp.experimental.data.normalised$day)
pnp.experimental.data.normalised$clone <-as.factor(pnp.experimental.data.normalised$clone)
pnp.experimental.data.normalised$induced <-as.factor(pnp.experimental.data.normalised$induced)

#this is what we have:
head(pnp.experimental.data.normalised)


# pivot the table to a longer format
# where everything is a category, except the normalised.pnp.conc
# which we added earlier with: mutate(normalised.pnp.conc = predicted.pnp.conc / DNA_ug_per_ml)
pnp.experimental.data.normalised.long <- pnp.experimental.data.normalised |> 
  pivot_longer(cols=!c(day,clone,induced), names_to = "replicate", values_to = "normalised.pnp.conc")

#this is what we have now
head(pnp.experimental.data.normalised.long)


# make a plot of the absorbance for each clone
# we make geom_boxplot, and then layer a geom_point on top
ggplot(pnp.experimental.data.normalised.long, aes(x = clone, y = normalised.pnp.conc, colour = day))+
  geom_boxplot(outliers = FALSE, position = position_dodge(width = 0.8))+
  theme_classic()+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
             alpha = 0.7, size = 5)

# make a plot of the absorbance for each clone
# using facet_wrap to differentiate the induced from non-induced data
# we make geom_boxplot, and then layer a geom_point on top, as before
ggplot(pnp.experimental.data.normalised.long, aes(x = clone, y = normalised.pnp.conc, colour = day))+
  geom_boxplot(outliers = FALSE, position = position_dodge(width = 0.8))+
  theme_classic()+
  geom_point(position = position_jitterdodge(jitter.width = 0.2, dodge.width = 0.8), 
             alpha = 0.7, size = 5) +
  facet_wrap(~induced)


#we save the result in an object called aov.result.additive
aov.result.additive <- aov(normalised.pnp.conc ~ day + clone + induced, data = pnp.experimental.data.normalised.long)
summary(aov.result.additive)

#run the ANOVA
aov.result.interaction  <- aov(normalised.pnp.conc ~ day * clone * induced, data = pnp.experimental.data.normalised.long)

#to view the results
summary(aov.result.interaction)




