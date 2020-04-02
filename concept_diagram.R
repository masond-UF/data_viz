# 2 April 2020—Conceptual Diagram 


library(ggplot2)
library(bezier)  # package gives functions to draw custom curves
library(grid)    # packages helps with arranging Grobs
library(jpeg)

decomp <- rasterGrob(readJPEG('decomp.jpg'))
PY <- rasterGrob(readJPEG('PY.jpg'))
PD <- rasterGrob(readJPEG('PD.jpg'))
ND <- rasterGrob(readJPEG('ND.jpg'))


ggplot(data.frame(a=1)) + xlim(1, 30) + ylim(1, 30)+ 
	annotation_custom(decomp, xmin = 14, xmax = 17, ymin = 24, ymax = 30)+
	annotation_custom(PY, xmin = 11, xmax = 14, ymin = 18, ymax = 21)+
	annotation_custom(PD, xmin = 14, xmax = 17, ymin = 18, ymax = 21)+
	annotation_custom(ND, xmin = 17, xmax = 20, ymin = 18, ymax = 21)+
	theme_classic()

ggsave("output1.png")
