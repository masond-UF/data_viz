# 2 April 2020—Conceptual Diagram 


library(ggplot2)
library(bezier)  # package gives functions to draw custom curves
library(grid)    # packages helps with arranging Grobs
library(jpeg)

decomp <- rasterGrob(readJPEG('decomp.jpg'))
PY <- rasterGrob(readJPEG('PY.jpg'))
PD <- rasterGrob(readJPEG('PD.jpg'))
ND <- rasterGrob(readJPEG('ND.jpg'))
base <- rasterGrob(readJPEG('base.jpg'))
plant1 <- rasterGrob(readJPEG('plant1.jpg'))
plant2 <- rasterGrob(readJPEG('plant2.jpg'))
plant3 <- rasterGrob(readJPEG('plant3.jpg'))
fact1 <- rasterGrob(readJPEG('factorial1.jpg'))
fact2 <- rasterGrob(readJPEG('factorial2.jpg'))
NRI <- rasterGrob(readJPEG('NRI.jpg'))


ggplot(data.frame(a=1)) + xlim(1, 30) + ylim(1, 30)+ 
	annotation_custom(decomp, xmin = 13, xmax = 16, ymin = 24, ymax = 30)+
	annotation_custom(PY, xmin = 10, xmax = 13, ymin = 18, ymax = 21)+
	annotation_custom(PD, xmin = 13, xmax = 16, ymin = 18, ymax = 21)+
	annotation_custom(ND, xmin = 16, xmax = 19, ymin = 18, ymax = 21)+
	geom_rect(aes(xmin = 10, xmax = 19, ymin = 18, ymax = 21.5),
	fill = "transparent", color = "gray", size = 0.5, linejoin = "round")+
	annotation_custom(base, xmin = 22, xmax = 26, ymin = 9, ymax = 12)+
	annotation_custom(plant1, xmin = 21.5, xmax = 23, ymin = 10, ymax = 15)+
	annotation_custom(plant2, xmin = 23, xmax = 25, ymin = 11, ymax = 14)+
	annotation_custom(plant3, xmin = 24.5, xmax = 26.5, ymin = 11, ymax = 16.5)+
	annotation_custom(NRI, xmin = 2, xmax = 13, ymin = 2, ymax = 16)+
	theme_classic()



ggsave("output1.png")
