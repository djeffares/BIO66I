#BIO00066I workshop 4 code
#Daniel Jeffares
#2024-04-16

#this code is just for the animate plots part
#5 Exercises (Part 2): Exploring MCS movement

################################################################
#NOTE: this is minimal code to get the animations working
#this will NOT be sufficient for your RStudio project.
################################################################

#load libraries
library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)

#load data
#load data from a URL
points<-read_csv(url("https://djeffares.github.io/BIO66I/points.data.2024-03-16.csv"),
                 col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
)

#make static plot
static.plot <-points |>
  filter(clone == "B", LID == 1) |>
  ggplot(aes(x=x.position,y=y.position, colour=TID))+
  geom_point(size=10, pch=1,lwd=2)
static.plot

#animated plot
#set up the animation code
animated.plot <- static.plot +
  transition_time(time) +
  shadow_mark(past = T, future=F, alpha=0.5)

#check the animation worked
animate(animated.plot, width =800, height = 800)

#save the animation as a gif
anim_save("cloneB.ineage2gif", animated.plot)

a = rnorm(1000)
b = a +1
wilcox.test.result <- wilcox.test(a,b)
wilcox.test.result$p.value

ap<-read_excel("raw-data/example_alkaline_phosphatase_activity_assay_DNA_Lab.xlsx", sheet=1, skip=3)

#check what we have
view(ap)

ap.pivot <- 
  ap |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNP.conc, names_to = "rep", values_to = "absorb")
view(ap.pivot)

ap.pivot <- relocate(ap.pivot, absorb)

#plot, saving thr plot in an object called 'pNP.plot'
pNP.plot <- ap.pivot |>
  ggplot(aes(x=absorb, y=pNP.conc))+
  geom_point()+
  geom_smooth(method="lm")+
  xlab("Absorbance")+
  ylab("pNP concentration")

#show the plot
pNP.plot

#save as a pdf
ggsave("pNP.plot.pdf",pNP.plot,width=7,height=7) 

#save as a jpeg, with a better name
ggsave("BIO66I-2025-03-21-pNP.plot.jpeg",pNP.plot,width=7,height=7)

linear_model <- lm(pNP.conc ~ absorb , data = ap.pivot)

mock<-read_excel("raw-data/example_alkaline_phosphatase_activity_assay_DNA_Lab.xlsx", sheet=2, skip=1)

#check what we have
view(mock)

mock <- mock |> 
  rowwise() |> 
  mutate(absorb=mean(c(absorb.rep1,absorb.rep2,absorb.rep3),na.rm=T)) 


#calculate predicted pNP concentration using the linear model
predictions.from.lm <- predict(linear_model,mock)

#Add these predictions from the linear model (predictions.from.lm)
#As a new column
mock$predicted.pNP.concs = predictions.from.lm

#check what we have
view(mock)

mock$DNA_ug_per_ml

#And save all our data (give your Rda file a sensible name!)
save.image("chocolate-fish.Rda")

mock$day <-as.factor(mock$day)
mock$clone <-as.factor(mock$clone)
mock$differentiated <-as.factor(mock$differentiated)

#make the plot
ggplot(data=mock, aes(x=day, y=predicted.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())

