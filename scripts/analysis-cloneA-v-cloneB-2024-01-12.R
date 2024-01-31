#further analysis of the all-cell-data-FFT.tsv data

#clear previous data
rm(list=ls())

#load the tidyverse
library(tidyverse)

#we need this to make pretty plots with the 'ggarrange' package
library(ggpubr)

#load data
cells <-read_tsv(url("https://djeffares.github.io/data/BIO00066I/all-cell-data-FFT.tsv"))
cells <-read_tsv(url("https://djeffares.github.io/data/BIO00066I/all-cell-data-FFT.filtered.2024-01-19.tsv"))



#data clean up to remove objects that may not be cells:
#area < 500 ug
#dry mass < 200 pg
#large ones?
 
names(cells)

top.end.filter = 0.9999
low.end.filter =  1e-04

#area < 500 ug does not make much sense
#but we could remove very large ones:
ggplot(cells,aes(x=clone,y=area))+
    geom_violin()+
    geom_hline(yintercept = quantile(cells$area, c(low.end.filter,top.end.filter)))+
    stat_compare_means()

#dry mass < 200 pg: again, does not make much sense
#but we could remove the large ones
ggplot(cells,aes(x=clone,y=dry.mass))+
    geom_violin()+
    geom_hline(yintercept = quantile(cells$dry.mass, c(low.end.filter,top.end.filter)))+
    stat_compare_means()

#examine cell volume 
ggplot(cells,aes(x=clone,y=volume))+
    geom_violin()+
    geom_hline(yintercept = quantile(cells$volume, c(low.end.filter,top.end.filter)))+
    stat_compare_means()


#to clean up all
cells.filtered <- cells |>
    #cell area filtering
    filter(area < quantile(cells$area, top.end.filter)) |>
    filter(area > quantile(cells$area, low.end.filter)) |>
    
    #dry.mass filtering
    filter(dry.mass < quantile(cells$dry.mass, top.end.filter)) |>
    filter(dry.mass > quantile(cells$dry.mass, low.end.filter)) |>
    
    #volume filtering
    filter(volume < quantile(cells$volume, top.end.filter)) |>
    filter(volume > quantile(cells$volume, low.end.filter))

#save this data
write_tsv(cells.filtered,file="data/all-cell-data-FFT.filtered.2024-01-19.tsv")

nrow(cells.filtered)/nrow(cells)
#0.9996954

#examine the results of this filtering:
plots<-list()
#prior to filtering
plots[[1]]<-ggplot(cells,aes(x=clone,y=area))+ geom_boxplot()+
    geom_hline(yintercept = quantile(cells$area,  c(low.end.filter,top.end.filter)))

plots[[2]]<-ggplot(cells,aes(x=clone,y=dry.mass))+geom_boxplot()+
    geom_hline(yintercept = quantile(cells$dry.mass, c(low.end.filter,top.end.filter)))

plots[[3]]<-ggplot(cells,aes(x=clone,y=volume))+geom_boxplot()+
    geom_hline(yintercept = quantile(cells$volume, c(low.end.filter,top.end.filter)))

plots[[4]]<-ggplot(cells,aes(x=clone,y=length))+geom_boxplot()
plots[[5]]<-ggplot(cells,aes(x=clone,y=width))+geom_boxplot()

#after filtering
plots[[6]]<-ggplot(cells.filtered,aes(x=clone,y=area))+ geom_boxplot()+
    geom_hline(yintercept = quantile(cells$area,  c(low.end.filter,top.end.filter)))

plots[[7]]<-ggplot(cells.filtered,aes(x=clone,y=dry.mass))+geom_boxplot()+
    geom_hline(yintercept = quantile(cells$dry.mass, c(low.end.filter,top.end.filter)))

plots[[8]]<-ggplot(cells.filtered,aes(x=clone,y=volume))+geom_boxplot()+
    geom_hline(yintercept = quantile(cells$volume, c(low.end.filter,top.end.filter)))

plots[[9]]<-ggplot(cells.filtered,aes(x=clone,y=length))+geom_boxplot()
plots[[10]]<-ggplot(cells.filtered,aes(x=clone,y=width))+geom_boxplot()

ggarrange(plotlist=plots,ncol=5,nrow=2)

#ggsave(all.filtering.plots,file="all.filtering.plots.2024-01-16.pdf")

#there are two clones: clone A and clone B
unique(cells$clone)

#to compare various aspects of the clones, we will select only *some* of the columns
cells.selected.columns<-select(cells,
    clone,
    volume, 
    mean.thickness, 
    radius,
    area,
    sphericity,
    length,
    width,
    orientation,
    dry.mass,displacement,
    instantaneous.velocity
)
names(cells.selected.columns)

#subset clones
cloneA<-filter(cells.selected.columns, clone=="cloneA")
cloneB<-filter(cells.selected.columns, clone=="cloneB")



#compare various metrics
par(mfrow=c(3,4))
for (i in 2:ncol(cloneA)){
    A<-as.numeric(unlist(cloneA[,i]))
    B<-as.numeric(unlist(cloneB[,i]))
    test<-signif(wilcox.test(A,B)$p.value,2)
    boxplot(A,B,main=names(cloneA)[i],outline=F)
    mtext(paste("P=",test))
    print(paste(names(cloneA)[i],test))
}

#all params of these cells are statistically different:
#SUBTLE
# [1] "dry.mass 7.7e-166"
#[1] "volume 7.7e-166"
# [1] "length 0"
# [1] "orientation 1.9e-21"

#VERY DIFFERENT
# [1] "sphericity 0"
# [1] "mean.thickness 0"
# [1] "radius 0"
# [1] "area 0"
# [1] "width 0"
# [1] "displacement 0" 
# [1] "instantaneous.velocity 0"

#Make some plots to look at the shape
width.plot<-ggplot(cells, aes(x=clone,y=width,color=clone))+
    geom_boxplot()+
    geom_hline(yintercept = median(cells$width),linetype="dashed")+
    stat_compare_means()+
    theme_classic2()

length.plot<-ggplot(cells, aes(x=clone,y=length,color=clone))+
    geom_boxplot()+
    geom_hline(yintercept = median(cells$length),linetype="dashed")+
    stat_compare_means()+
    theme_classic2()
#arrange 
ggarrange(width.plot,length.plot)

#shows that
    #cloneA is thinner and longer
    #cloneB is wider and shorter

#Look at how they move
# [1] "displacement 0" 
# [1] "instantaneous.velocity 0"

ggplot(cells, aes(x=clone,y=instantaneous.velocity,colour=clone))+
    geom_violin()+
    stat_compare_means()+
    theme_bw()+

velocity.plot<-ggplot(cells, aes(x=clone,y=instantaneous.velocity,colour=clone))+
    geom_violin()+
    stat_compare_means()+
    theme_bw()+
    scale_y_log10()+
    ylab("log10(instantaneous.velocity)")

displacement.plot<-ggplot(cells, aes(x=clone,y=displacement,colour=clone))+
    geom_violin()+
    stat_compare_means()+
    theme_bw()+
    scale_y_log10()+
    ylab("log10(displacement)")

#arrange
ggarrange(velocity.plot,displacement.plot)    

#save
movement.plots<-ggarrange(velocity.plot,displacement.plot)  
ggsave(movement.plots,file="clone.movement.plots.jpeg")
