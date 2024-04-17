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

