# #install a new package
# install.packages("gganimate")

library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)

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
  geom_smooth(method="lm")

#show the plot
pNP.plot


#save as a pdf
ggsave("pNP.plot.pdf",pNP.plot,width=7,height=7) 

#save as a jpeg, with a better name
#easier to find in your files
#has a 'date stamp' (2025-03-21) in the name
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

#And save all our data
save.image("BIO00066I-workshop4-2025-03-21.Rda")

##divide the predicted.pNP.concs column by the DNA_ug_per_ml column
mock <- mock |> 
  mutate(normalised.pNP.concs = predicted.pNP.concs / DNA_ug_per_ml)

mock$day <-as.factor(mock$day)
mock$clone <-as.factor(mock$clone)
mock$differentiated <-as.factor(mock$differentiated)

#make the plot
ggplot(data=mock, aes(x=day, y=predicted.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())


# first check that data columns we have 
names(mock)

#make the plot using the normalised.pNP.concs as a y values
ggplot(data=mock, aes(x=day, y=normalised.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())


# #load data from a URL
# points<-read_csv(url("https://djeffares.github.io/BIO66I/points.data.2024-03-16.csv"),
#                  col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
# )
# 
# #check what we have
# glimpse(points)
# head(points)
# 

# points |>
#   ggplot(aes(x=clone,y=log10(velocity)))+
#   geom_violin()+
#   stat_compare_means()

# ?glimpse

# points |>
#   ggplot(aes(x=x.position,y=y.position,colour=clone))+
#   geom_point(size=1)+
#   facet_wrap(~LID)
# 
# #If we want to show just one clone add this line after 'points |>'
# #filter(clone == "A")|>

# 
# points |>
#   filter(clone == "B" & LID==4) |>
#   # Convert TID to factor with numeric ordering
#   mutate(TID = factor(TID, levels = sort(as.numeric(as.character(unique(TID)))))) |>
#   ggplot(aes(x=x.position, y=y.position, colour=TID)) +
#   geom_point(size=1)
# 

# #use mutate to make new columns
# points2<- points |>
#   group_by(LID) |>
#   mutate(med.x = mean(x.position, na.rm = TRUE))|>
#   mutate(adjusted.x =  x.position - med.x) |>
#   mutate(med.y = mean(y.position, na.rm = TRUE))|>
#   mutate(adjusted.y =  y.position - med.y)
# 
# #check our new data
# points2

# points2 |>
#   ggplot(aes(x=adjusted.x,y=adjusted.y,colour = time))+
#   geom_point(alpha=0.5, size=3)+
#   geom_hline(yintercept = 0)+
#   geom_vline(xintercept = 0)+
#   facet_wrap(~clone)

# static.plot <-points |>
#   filter(clone == "B", LID == 1) |>
#   ggplot(aes(x=x.position,y=y.position, colour=TID))+
#   geom_point(size=10, pch=1,lwd=2)
# static.plot

# #set up the animation code
# animated.plot <- static.plot +
#   transition_time(time) +
#   shadow_mark(past = T, future=F, alpha=0.5)
# 
# #check the animation worked
# animate(animated.plot, width =800, height = 800)

# #save the animation as a gif
# #make sure you use a sensible file name
# anim_save("cloneB.ineage2gif", animated.plot)

# ?glimpse

# #load the data
# ap<-read_excel("raw-data/alkaline-phosphatase-activity-assay-real-data.xlsx",sheet=2)
# 
# #rename the 'differentiated' column 'induced', which describes it better
# names(ap)[3]<-'induced'

# #set day, clone and induced columns to be factors
# ap$day <-as.factor(ap$day)
# ap$clone <-as.factor(ap$clone)
# ap$induced <-as.factor(ap$induced)
# 
# #this is what we have:
# head(ap)

# ap.pivot<-ap |>
#   pivot_longer(cols=!c(day,clone,induced), names_to = "rep", values_to = "absorb")
# 
# #this is what we have now
# head(ap.pivot)

# ggplot(ap.pivot, aes(x = clone, y = absorb, color = day))+
#   geom_boxplot()+
#   theme_classic()

# ggplot(ap.pivot, aes(x = clone, y = absorb, color = day))+
#   geom_boxplot()+
#   theme_classic()+
#   facet_wrap(~induced)

# #we save the result in an object called aov.result.additive
# aov.result.add <- aov(absorb ~ day + clone + induced, data = ap.pivot)

# summary(aov.result.add)

# 
# #run the ANOVA
# aov.result.mult  <- aov(absorb ~ day * clone * induced, data = ap.pivot)
# 
# #to view the results
# summary(aov.result.mult)

knitr::purl("workshop4.qmd", 
            output = "workshop4.R", 
            documentation = 0)
