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

A.points <- janitor::clean_names(A.points)
B.points <- janitor::clean_names(B.points)
A.points <- A.points[,1:15]
names(A.points)
names(B.points)

#clear up names:
names(A.points)[2]="lineage.id"

names(A.points) <- names(B.points)
length(names(A.points))
length(names(B.points))


#this data needs some manual renaming
A.points$clone="A"
B.points$clone="B"

points <- bind_rows(A.points,B.points)
#alb_A1_8bit-0.25 points correct

#in this file we have CID (cell id?) and before we talked about lineage id
names(points)

#change cid to (cell id) to lid (lineage id)
#and make the column names upper case so it simpler for the students
names(points)[2] = "LID"
names(points)[3] = "TID"
names(points) <- toupper(names(points))

#get rid of text after the underscore in points
names(points)<-str_split_i(names(points), "_",1)

#have a look at one lineage LID 
points |>
  filter(LID==4)|>
  ggplot(aes(x=x,y=y,colour=clone))+
  geom_point(size=1)+
  facet_wrap(~TID)
  
#how mnay unique LIDs
unique(subset(points, clone = "A")$TID)
unique(subset(points, clone = "B")$TID)

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

