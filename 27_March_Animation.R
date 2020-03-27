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

thermo <- read.csv("animation_data/osbs_fire_loggers_3_25_2020.csv")
thermo$date_time <-  ymd_hms(thermo$date_time)
osbs <- read_sf("animation_data/os_elev_I2.shp")


p1 <- ggplot()+
	geom_sf(data = osbs_crop)+
	geom_point(data = thermo, aes(x = x, y = y, color = temp_C))+
	scale_color_viridis_c()+
	transition_time(time = thermo$date_time)+
	labs(title = "Time {frame_time}")+
	theme_bw()

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

p2
