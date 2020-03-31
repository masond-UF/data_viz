# attempt at concept diagram using ggplot
# and annotate_custom

library(ggplot2)
library(bezier)  # package gives functions to draw custom curves
library(grid)    # packages helps with arranging Grobs
library(jpeg)    # package helps deal with jpeg files

# make figures for the concept diagram

df <- read.csv("toydata_wide.csv", stringsAsFactors = FALSE) # some data I made up
df

colnames(df)[1] <- "plot"    # Excel or read.csv stuck some nonsense characters on first column name

# load pictures in to be used in the diagram
pig <- rasterGrob(readJPEG('wild_boar.jpg'))

llp <- rasterGrob(readJPEG('llp rockets.jpg'))

wg <- rasterGrob(readJPEG('wiregrass.jpg'))

# make plots of the toy data 
pig_wire <- ggplotGrob(ggplot(df, aes(x=hog, y = wiregrass))+
  geom_point()+
    geom_line()+
    
  theme_bw())

pig_llp <- ggplotGrob(ggplot(df, aes(x=hog, y=llp))+
  geom_line()+
    geom_point()+
  theme_bw())

wire_llp <- ggplotGrob(ggplot(df, aes(x=wiregrass, y=llp))+
  geom_line()+
  geom_point()+
  theme_bw())

# This makes the diagram
ggplot(data.frame(a=1)) + xlim(1, 30) + ylim(1, 30) +  # basically makes a blank canvas
  annotation_custom(pig, xmin = 10, xmax = 20, ymin = 20, ymax = 30) + # add pictures
  annotation_custom(llp, xmin = 1, xmax = 11, ymin = 2, ymax = 12) +
  annotation_custom(wg, xmin = 18, xmax = 30, ymin = 2, ymax = 12) +
  # annotation_custom(pig_wire, xmin = 21, xmax = 28, ymin = 15, ymax = 25) + # add graphs
  # annotation_custom(pig_llp, xmin = 1, xmax = 9, ymin = 15, ymax = 25) +
  # annotation_custom(wire_llp, xmin = 11, xmax = 19, ymin = 1, ymax = 9) +
  #now we draw arrows linking elements
  geom_path(data = as.data.frame(bezier(t = 0:100/100, 
                                        p = list(x = c(15, 14, 13, 10),
                                                 y = c(21, 19, 15, 10)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
                                                           type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100, 
                                        p = list(x = c(15, 16, 17, 20), 
                                                 y = c(21, 19, 15, 10)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
                                                         type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100,
                                        p = list(x = c(10, 12, 14, 20),
                                                 y = c(9, 9, 9, 9)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
                                                         type = "closed")) +
  geom_path(data = as.data.frame(bezier(t = 0:100/100,
                                        p = list(x = c(12, 11, 10.5, 10),
                                                 y = c(9, 9, 9, 9)))),
            aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
                                                         type = "closed")) +
  # add labels to arrows
  geom_label(label ="consumption", x = 12, y=15)+
  geom_label(label ="consumption", x = 19, y=15)+
  geom_label(label ="competition", x = 15, y=10)+
# use theme to make background blank
  theme(rect = element_blank(),
        line = element_blank(),
        text = element_blank(),
        plot.margin = unit(c(0,0,0,0), "mm"))

# presto - a diagram of an invasive species impacting a native longleaf pine system
