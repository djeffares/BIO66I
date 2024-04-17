#analysis for workshop 4

#analysis of the raw tracking data in files:
#"A1 mtrackj tracks.xls"
#"B2 mtrackj points.xls"
#stored in /Users/dj757/gd/modules/BIO66I/raw-data

##################################
#set up
##################################

rm(list=ls())
library(tidyverse)
library(readxl)
library(ggpubr)


####################################################################
#PART 1: STANDARD CURVE
####################################################################


##################################
#standard curve
##################################

#read in excel
ap<-read_excel("raw-data/Example Alkaline Phosphatase Activity assay .xlsx", sheet=2, skip=3)
view(ap)

#Adjust the name of the first column to "Variable1"
names(ap)

#simplify the data with pivot_longer
ap.pivot <- 
  ap |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNPP.conc, names_to = "rep", values_to = "absorb")
view(ap.pivot)

#reorder columns, so it is more intuitive 
ap.pivot <- relocate(ap.pivot, absorb)

#plot
pNPP.plot <- ap.pivot |>
  ggplot(aes(x=absorb, y=pNPP.conc))+
  geom_point()+
  geom_smooth(method="lm")

#how close are the reps? what if we use ggplot(aes(x=absorb, y=pNPP.conc,colour = rep))

#now save the plot with ggsave
ggsave("pNPP.plot.pdf",pNPP.plot)  
  
#make a linear model
linear_model <- lm(pNPP.conc ~ absorb , data = ap.pivot)
absorbance_values <- data.frame(absorb = c(1,2,3))

#given some absorbence_values, make a prediction of some pNPP concentrations
predicted.pnpp.concs <- predict(our_model,absorbance_values)
predicted.pnpp.concs

#what if we want to show this on the plot?
predicted.data<-data.frame(absorb=c(1,2,3),conc=predicted.pnpp.concs,rep="predicted")

###AMANDA: any point in doing this?



####################################################################
#PART 2: POINTS DATA
####################################################################


####################################################################
#POINTS DATA: reading and cleaning data
####################################################################

#NOTES FOM AB
#The difference is the files you attached to the email. 
#You've got the alb_A1_8bit-0.25 points.xlsx attached to the email rather than alb_A1_8bit-0.25 tracks.xlsx. 
#The points data is the raw data I obtained from cell tracking on imageJ. 
#Whereas the tracks file is the output from python which analysed the point file to give metrics.

#So I should use the tracks data

library(tidyverse)
library(readxl)
library(ggpubr)
library(gganimate)

ap<-read_excel("raw-data/example_alkaline_phosphatase_activity_assay-2024-03-21.xlsx", sheet=1, skip=3)

ap.pivot <- 
  ap |> 
  select(-mean.abs, -standard.deviation) |>
  pivot_longer(-pNP.conc, names_to = "rep", values_to = "absorb")
ap.pivot <- relocate(ap.pivot, absorb)
linear_model <- lm(pNP.conc ~ absorb , data = ap.pivot)

mock<-read_excel("raw-data/example_alkaline_phosphatase_activity_assay-2024-03-21.xlsx", sheet=2, skip=1)
mock$day <-as.factor(mock$day)
mock$clone <-as.factor(mock$clone)
mock$differentiated <-as.factor(mock$differentiated)
mock <- mock |> 
  rowwise() |> 
  mutate(absorb=mean(c(absorb.rep1,absorb.rep2,absorb.rep3)))

ggplot(data=mock, aes(x=day, y=predicted.pNP.concs,fill=clone:differentiated)) +
  geom_bar(stat="identity", position=position_dodge())



####################################################################
#DATA CLEAN UP - AMANDA YOU CAN SKIP THIS
####################################################################


#previous data:
cells <-read_tsv(url("https://djeffares.github.io/BIO66I/all-cell-data-FFT.filtered.2024-02-22.tsv"),
                 col_types = cols(
                   clone = col_factor(),
                   replicate = col_factor(),
                   tracking.id=col_factor(),
                   lineage.id=col_factor()
                 )
)
names(cells)
  
#read data
A.points <-read_excel("/Users/dj757/gd/modules/BIO66I/raw-data/alb_A1_8bit-0.25 points.xlsx",skip=1)
B.points <-read_excel("/Users/dj757/gd/modules/BIO66I/raw-data/alb_B2_8bit-0.25 points.xlsx", skip=1)

#clean up names
A.points <- janitor::clean_names(A.points)
B.points <- janitor::clean_names(B.points)
A.points <- A.points[,1:15]
names(A.points)
names(B.points)

#names(A.points)[2]="lineage.id"

names(A.points) <- names(B.points)
length(names(A.points))
length(names(B.points))

#this data needs some manual renaming
A.points$clone="A"
B.points$clone="B"

points <- bind_rows(A.points,B.points)
#alb_A1_8bit-0.25 points correct

#remove some columns that we don't need:
names(points)


#in this file we have CID (cell id?) and before we talked about lineage id
names(points)

#change cid to (cell id) to lid (lineage id)
#and make the column names upper case so it simpler for the students
names(points)[2] = "LID"
names(points)[3] = "TID"
#names(points) <- toupper(names(points))

#get rid of text after the underscore in points
names(points)<-str_split_i(names(points), "_",1)


#to remove
#Nr
#Elucidean distance
##I [val]
#Track Length (um)
#D2R [祄]
#alpha [deg]
#delta alpha [deg]
#...16

#remove solumns we don't need
#points <- select(points, -nr, -i, -d2r, -alpha, -delta)
#names(points)

#give columns betetr names
#len: track.length (distance it has moved from its start points)
#d2s: euclidean.distance
#d2p: jump.distance (distance from current point to previous point)
#v: velocity

#give better names
points <- points |>
  rename(euclidean.distance = d2s) |>
  rename(track.length = len) |>
  rename(jump.distance = d2p) |>
  rename(time = t) |>
  rename(velocity = v) |>
  rename(x.position = x) |>
  rename(y.position = y) 


#output the points data
write_csv(points, "data/points.data.2024-03-16.csv")

points<-read_csv("data/points.data.2024-03-16.csv",
  col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
)

#save it
save.image("workshop4.anaysis.2024-03-16.Rda")

####################################################################
#DATA CLEAN UP DONE
####################################################################

#remove all data and start again
rm(list=ls())
library(tidyverse)
library(readxl)
library(ggpubr)
#install.packages("gganimate")
#install.packages("gifski")
library(gganimate)


#load data
points<-read_csv("data/points.data.2024-03-16.csv",
                 col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
)

#how fast to they move
points |>
  ggplot(aes(x=clone,y=log10(velocity)))+
  geom_violin()+
  stat_compare_means()

#rose plot: adjust so the median of mean_x_mm is zero
#and mean_y_mm is zero
#ie: centralize all the values
points2<- points |> 
  group_by(LID) |> 
  mutate(med.x = mean(x.position, na.rm = TRUE))|>
  mutate(adjusted.x =  x.position - med.x) |>
  mutate(med.y = mean(y.position, na.rm = TRUE))|>
  mutate(adjusted.y =  y.position - med.y)
points2

#make a rose plot  
points2 |>  
  ggplot(aes(x=adjusted.x,y=adjusted.y,colour = euclidean.distance))+
  geom_point(alpha=0.5, size=3)+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 0)+
  facet_wrap(~clone)

#how to they move?

#show all lineages in A
points |>
  #filter(clone == "A")|>
  ggplot(aes(x=x.position,y=y.position,colour=clone))+
  geom_point(size=1)+
  facet_wrap(~LID)

#show one lineage, from one clone and colour by TID
points |>
  filter(clone == "B" & LID==4)|>
  ggplot(aes(x=x.position,y=y.position,colour=TID))+
  geom_point(size=1)


#try colouring by time:

#let's animate this
static.plot <-points |>
  filter(clone == "B", LID == 1) |>
  ggplot(aes(x=x.position,y=y.position, colour=TID))+
  geom_point(size=10, pch=1)
static.plot

#try: 
#filter(clone == "B")
#facet_wrap(~LID)
#geom_point(size=5, pch=1)

animated.plot <- static.plot +
  transition_time(time) +
  shadow_mark(past = T, future=F, alpha=0.5)

#amimate it!
animate(animated.plot, width =800, height = 800)

#save as gif
anim_save("clineB.ineage2gif", animated.plot)

#do they follow each other?
#can we find out if they follow eacgh othe by correlations of x and y





#make rose plots for clone A and clone B side by side
#

#do they follow each other



names(points)
  
#how mnay unique LIDs
#unique(subset(points, clone = "A")$TID)
#unique(subset(points, clone = "B")$TID)

#clean names
A.tracking <- janitor::clean_names(A.tracking)
B.tracking  <- janitor::clean_names(B.tracking)

#check names
length(names(A.tracking))
length(names(B.tracking))

#set all values to numeric
A.tracking <- A.tracking |>
  mutate_if(is.character, as.numeric)

B.tracking <- B.tracking |>
  mutate_if(is.character, as.numeric)

#merge the two data sets
A.tracking$clone <- "A"
B.tracking$clone <- "B"

glimpse(A.tracking)
glimpse(B.tracking)

tracking<-bind_rows(A.tracking, B.tracking)
glimpse(tracking)

view(tracking)
summary(tracking)

save.image("tracking.data.2024-02-27.Rda")


####################################################################
#ANALYSIS: AMANDA START FROM HERE
####################################################################

load("tracking.data.2024-02-27.Rda")

#look at mean_x_mm and mean_y_mm values
#not very useful??
track.summary<-tracking |>
  group_by(clone, cid) |>
  summarise(
    x=median(mean_x_mm),
    y=median(mean_y_mm),
    n=n()
)
track.summary

#plot for each CID
#shows that each CID remins in ona read, more or less
#and clone A wanders more
tracking |> 
  ggplot(aes(x=mean_x_mm,y=mean_y_mm,col=clone)) +
  geom_point()+
  facet_grid(vars(cid), vars(clone))


unique(filter(tracking, clone == "B")$cid)


#adjust so the median of mean_x_mm is zero
#and mean_y_mm is zero
#ie: centralize all the values
tracking2 <- tracking |> 
  select(clone, cid, mean_x_mm, mean_y_mm) |> 
  group_by(clone, cid) |> 
  mutate(mean.x = mean(mean_x_mm, na.rm = TRUE))|>
  mutate(adjusted.x = mean_x_mm - mean.x) |>
  mutate(mean.y = mean(mean_y_mm, na.rm = TRUE))|>
  mutate(adjusted.y = mean_y_mm - mean.y)


#plot all clone A's on one panel and all clone B's on another
tracking2 |>
  ggplot(aes(x=adjusted.x,y=adjusted.y,colour = clone)) +
  geom_point(size=3)+
  geom_hline(yintercept = 0,col=1,linetype = 2,alpha = 0.8)+
  geom_vline(xintercept = 0,col=1,linetype = 2,alpha = 0.8)+
  facet_wrap(~clone)+
  theme_classic()



####################################################################
#ANALYSIS: DO CELLS FOLLLOW EACH OTHER??
####################################################################


rm(list=ls())
library(tidyverse)
library(readxl)
library(ggpubr)
library(corrr)

#load data from a URL
points<-read_csv(url("https://djeffares.github.io/BIO66I/points.data.2024-03-16.csv"),
                 col_types = cols(LID = col_factor(),TID = col_factor(),pid = col_factor())
)

names(points)

#we want to compare each LID to each other, and see if the x.position's correlate. Then y.position's.
view(points)

#break data down into clone A and clone B and
#select only LID TID, x.position, y.position and time



################################################
### UP TO HERE
#WE NEED TO RESHAPE OUR DATA, so that 
#So that we each LID has a new column

################################################

df1 <- points |> 
  filter(clone == "A" & LID==1) |>
  select(LID, TID, pid, time, x.position) 

df2 <- points |> 
  filter(clone == "A" & LID==2) |>
  select(LID, TID, pid, time, x.position) 


view(df1)
glimpse(df2)



#see https://youtu.be/YpAdZ4079qs?si=H1wVr45UrjkPgYLM




  

