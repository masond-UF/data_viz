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
NRI <- rasterGrob(readJPEG('NRI2.jpg'))


ggplot(data.frame(a=1)) + xlim(1, 32) + ylim(1, 32)+ 
	annotation_custom(decomp, xmin = 2, xmax = 7, ymin = 22, ymax = 28)+
	geom_label(label ="Decomposition", x = 2.5, y = 29, color = "red", size = 4)+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(6, 7, 7),
																								 y = c(22.5, 22, 19)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotation_custom(fact1, xmin = 12, xmax = 19, ymin = 21, ymax = 28)+
	#annotate('text', x = 10, y = 30, label = 'Carrion design', fontface = 'bold')+
	annotate("text", x = 13.5, y = 29, label = "-")+
	annotate("text", x = 17, y = 29, label = "+")+
	annotate("text", x = 15.5, y = 30, label = "Biomass")+
	annotate("text", x = 10.3, y = 27, label = "Scavenger")+
	annotate("text", x = 10.2, y = 24.5, label = "Herbivore")+
	annotate("text", x = 10.2, y = 22, label = "Open")+
	annotation_custom(fact2, xmin = 21.5, xmax = 30, ymin = 22, ymax = 27)+
	#annotate('text', x = 26, y = 30, label = 'Seed design', fontface = 'bold')+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(8, 9, 9, 9),
																								 y = c(13, 11, 10, 8)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotate("text", x = 20.5, y = 26, label = "Proximal")+
	annotate("text", x = 20.5, y = 23, label = "Adjacent")+
	annotate("text", x = 24, y = 28, label = "Bank")+
	annotate("text", x = 27.5, y = 28, label = "Rain")+
	annotate("text", x = 22.72, y = 26, label = "PD", size = 3)+
	annotate("text", x = 23.85, y = 26, label = "PY", size = 3)+
	annotate("text", x = 25, y = 26, label = "ND", size = 3)+
	annotate("text", x = 12, y = 14, label = "b", size = 5)+
	annotate("text", x = 12, y = 2, label = "c", size = 5)+
	annotate("text", x = 29.7, y = 2, label = "d", size = 5)+
	annotation_custom(PY, xmin = 3, xmax = 5, ymin = 13, ymax = 18)+
	annotation_custom(PD, xmin = 6, xmax = 8, ymin = 13, ymax = 18)+
	annotation_custom(ND, xmin = 9, xmax = 11, ymin = 13, ymax = 18)+
	geom_label(label ="Seed bank/seed rain", x = 3, y = 20, color = "Blue", size = 4)+
	geom_rect(aes(xmin = 8.5, xmax = 30, ymin = 20, ymax = 31),# seed bank/seed rain
		 fill = "transparent", color = "black", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 22.1, xmax = 29.4, ymin = 22, ymax = 27),# seed design
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 15.6, xmax = 18.8, ymin = 26, ymax = 28),# mini blue
						fill = "transparent", color = "blue", size = 0.5, linejoin = "round")+
	geom_rect(aes(xmin = 12.2, xmax = 18.8, ymin = 21, ymax = 28.1),# carrion design
						fill = "transparent", color = "red", size = 0.5, linejoin = "round")+
		#annotation_custom(base, xmin = 4, xmax = 8, ymin = 3, ymax = 6)+
	annotation_custom(plant1, xmin = 2.5, xmax = 5.5, ymin = 3, ymax = 9)+
	annotation_custom(plant2, xmin = 6, xmax = 9, ymin = 2, ymax = 8)+
	annotation_custom(plant3, xmin = 9.5, xmax = 12.5, ymin = 2, ymax = 8)+
	geom_label(label ="Vegetation response", x = 3, y = 11, color = "red", size = 4)+
	annotation_custom(NRI, xmin = 16, xmax = 30, ymin = 0, ymax = 19)+
	geom_label(label ="Landscape heterogeniety", x = 23, y = 17, size = 4)+
	geom_path(data = as.data.frame(bezier(t = 0:100/100, 
																				p = list(x = c(12.5, 13, 14, 15),
																								 y = c(3, 2.5, 2, 3)))),
						aes(x = V1, y = V2), size = 1, arrow = arrow(length = unit(.01, "npc"),
																												 type = "closed"))+
	annotate("text", x = 19, y = 32, label = "Experimental design")+
	theme_void()

ggsave("output1.png")


