#initial data exploration
#20231130


#see what the data looks like


cell.674 <- subset(df, TrackingID == 674)
plot(cell.674$PositionX,cell.674$PositionY)

View(cell.674)

summary(cell.674)



#get a summary of all the columns
summary(df)

#fix the names, so R is OK with them
names(df)<- str_replace_all(names(df), c(
  "\\(" = "",
  "\\)" = "",
  " " = "",
  "\\/" = "",
  "µm" = "",
  "³" = "",
  "²" = "")
)

summary(df)

names(df)

#see what length the cells are:
ggplot(df, aes(x=DryMasspg)) + geom_histogram()

# 2d histogram with default option
ggplot(df, aes(x=Length,y=Width)) + 
  geom_bin2d() +
  theme_bw()

# Bin size control + color palette and correlation line
ggplot(df, aes(x=DryMasspg,y=Volume)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()

#test if they are correlated:
cor.test(df$Length,df$Width)

#subset of frame 1


# Bin size control + color palette and correlation line
ggplot(df, aes(x=PositionX,y=PositionY)) +
  geom_bin2d(bins = 70) +
  scale_fill_continuous(type = "viridis") +
  theme_bw()




