DATA = data.frame(images=list.files("C:/Users/iparedes/Downloads/Universidades", full.names = T), stringsAsFactors = F)
head(DATA)

DATA$names = gsub("[a-zA-Z]|[[:punct:]]","",DATA$images)
head(DATA)

DATA$values = sample(1:100, size=nrow(DATA))
head(DATA)

library(ggplot2)
ggplot(DATA) + geom_histogram(aes(x=names, y=values), stat="identity")

library(png) 
library(grid)


img = readPNG(DATA$images[1])
g =  rasterGrob(img, interpolate=TRUE)
ggplot(DATA) + 
  geom_histogram(aes(x=names, y=values), stat="identity") +
  annotation_custom(grob=g, xmin=.5, xmax=1.5, ymin=0, ymax=10)

PLOT = ggplot(DATA) + 
  geom_histogram(aes(x=names, y=values), stat="identity") 

g = list()
for(i in 1:nrow(DATA)){
  img = readPNG(DATA$images[i])
  g[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  PLOT = PLOT +
    annotation_custom(grob=g[[i]], xmin=i-.5, xmax=i+.5, ymin=0, ymax=10)
}

PLOT

PLOT + theme(axis.text.x = element_text(angle=60, vjust=-.001))
PLOT = ggplot(DATA) + 
  geom_histogram(aes(x=names, y=values), stat="identity") 

g = list()
for(i in 1:nrow(DATA)){
  img = readPNG(DATA$images[i])
  g[[i]] =  rasterGrob(img, interpolate=TRUE)
  
  PLOT = PLOT +
    annotation_custom(grob=g[[i]], xmin=i-.5, xmax=i+.5, ymin=DATA$values[i], ymax=DATA$values[i]+10)
}

PLOT + theme(axis.text.x = element_text(angle=60, vjust=-.001))

PLOT + 
  theme(axis.text.x = element_text(angle=60, vjust=-.001)) +
  scale_y_continuous(limits=c(0,110))

