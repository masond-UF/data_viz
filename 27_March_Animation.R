# 27 March 2020—Animation assignment

library(ggplot2)
library(lubridate)
library(data.table)
library(sf)
library(gganimate)
library(maps)
library(rgdal)
library(raster)
library(tidyverse)

str(thermo)

thermo <- read.csv("animation_data/osbs_fire_loggers_3_25_2020.csv")
thermo$date_time <-  ymd_hms(thermo$date_time)
d <- data.frame(Date= format(thermo$date_time, '%d/%m/%Y'), 
								Time=format(thermo$date_time, '%H:%M:%S'))
thermo <- cbind(thermo,d)
thermo$Time <- (hms(thermo$Time))
osbs <- read_sf("animation_data/os_elev_I2.shp")
osbs.crop <- st_crop(osbs, c(xmin = 403900, 
														 xmax = 404400, ymin = 3284900, ymax = 3285480))

p1 <- ggplot()+
	geom_sf(data = osbs.crop)+
	geom_point(data = thermo, aes(x = x, y = y, color = temp_C))+
	scale_color_viridis_c(limits = c(27, 40.65))+
	transition_states(thermo$date_time)+
	labs(title = "Time:{frame_time}")+
	theme_bw()

p1

labs(title = "Time:{as.integer(frame_time)}")+
	

summary(thermo$temp_C)

thermo_sub <- thermo %>% 
	filter(logger_id %in% c(1, 5, 11, 22))

loggers <- c("1" = "Logger 1",
						 "5" = "Logger 5",
						 "11" = "Logger 11",
						 "22" = "Logger 22")
	
p2 <-ggplot(data = thermo_sub, aes(x = date_time, y = temp_C, color = temp_C))+
		 geom_point()+
	   geom_line()+
		 scale_color_viridis_c(option = "B")+
		 xlab("Time (Z)")+
		 ylab("Fire Temperature (C°)")+
		 theme_bw(base_size = 15)+
		 transition_reveal(date_time)+
		 facet_wrap(~logger_id, labeller = as_labeller(loggers))

