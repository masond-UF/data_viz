# 27 March 2020—Animation assignment
# Install packages and prep data ####
library(ggplot2)
library(lubridate)
library(data.table)
library(sf)
library(gganimate)
library(maps)
library(rgdal)
library(raster)
library(tidyverse)
library(gifski)
# Read in data
thermo <- read.csv("animation_data/osbs_fire_loggers_3_25_2020.csv")
# Convert to time
thermo$date_time <-  ymd_hms(thermo$date_time)
# Find range of temperatures (most are very low temps) for plot 1
summary(thermo$temp_C)
# Filter out just the loggers we want for plot 2
thermo_sub <- thermo %>% 
	filter(logger_id %in% c(1, 5, 11, 22))
# Create labels for the facet wrap in plot 2
loggers <- c("1" = "Logger 1",
						 "5" = "Logger 5",
						 "11" = "Logger 11",
						 "22" = "Logger 22")

# Bring in the background map
osbs <- read_sf("animation_data/os_elev_I2.shp")
# Crop the map
osbs.crop <- st_crop(osbs, c(xmin = 403900, 
														 xmax = 404400, ymin = 3284900, ymax = 3285480))

# plot 1 ####
p1 <- ggplot()+
	geom_sf(data = osbs.crop)+
	geom_point(data = thermo, aes(x = x, y = y, color = temp_C))+
	scale_color_viridis_c(limits = c(27, 40.65))+ # set to limit of 3Q 
	transition_time(thermo$date_time)+
	labs(title = "Time:{frame_time}")+
	theme_bw()

# Save the files
p1_anim <- gganimate::animate(p1, duration = 60, )
gganimate::anim_save("p1_anim.gif", animation = p1_anim)

# plot 2 ####
p2 <-ggplot(data = thermo_sub, aes(x = date_time, y = temp_C, color = temp_C))+
		 geom_point()+
	   geom_line()+
		 scale_color_viridis_c(option = "B")+
		 xlab("Time (Z)")+
		 ylab("Fire Temperature (C°)")+
		 theme_bw(base_size = 15)+
		 transition_reveal(date_time)+
		 facet_wrap(~logger_id, labeller = as_labeller(loggers))

p2_anim <- gganimate::animate(p2, duration = 30)
gganimate::anim_save("p2_anim.gif", animation = p2_anim)
