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
	annotation_custom(decomp, xmin = 4, xmax = 9, ymin = 23, ymax = 29)+
	annotation_custom(fact1, xmin = 13, xmax = 20, ymin = 21, ymax = 28)+
	annotation_custom(fact2, xmin = 21, xmax = 29, ymin = 22, ymax = 26)+
	annotation_custom(PY, xmin = 3, xmax = 5, ymin = 13, ymax = 18)+
	annotation_custom(PD, xmin = 6, xmax = 8, ymin = 13, ymax = 18)+
	annotation_custom(ND, xmin = 9, xmax = 11, ymin = 13, ymax = 18)+
	geom_rect(aes(xmin = 2, xmax = 12, ymin = 13.5, ymax = 18),
	fill = "transparent", color = "gray", size = 0.5, linejoin = "round")+
	#annotation_custom(base, xmin = 4, xmax = 8, ymin = 3, ymax = 6)+
	annotation_custom(plant1, xmin = 2.5, xmax = 5.5, ymin = 3, ymax = 9)+
	annotation_custom(plant2, xmin = 6, xmax = 9, ymin = 2, ymax = 8)+
	annotation_custom(plant3, xmin = 9.5, xmax = 12.5, ymin = 2, ymax = 8)+
	annotation_custom(NRI, xmin = 18, xmax = 29, ymin = 0, ymax = 15)+
	theme_classic()



ggsave("output1.png")
