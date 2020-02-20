# Plotting OSBS sites ####
rm(list=ls(all=TRUE))
# load necessary libraries ####
library(tidyverse) # for efficient data manipulation
library(rgdal)
library(sf) # not sure if this is necessary
library(ggmap) # to get a Google satelitte map
library(ggspatial)

# load the NEON shapefiles
sample_bounds <- readOGR(dsn="Data_OSBS_site_plots/fieldSamplingBoundaries", layer="terrestrialSamplingBoundaries")
field_sites <- readOGR(dsn="Data_OSBS_site_plots/All_NEON_TOS_Plots_V4", layer="All_Neon_TOS_Polygon_V4")

# extract only Florida (OSBS) data
sample_bounds_FL <- sample_bounds[which(sample_bounds$siteID=="OSBS"),] 
field_sites_FL <- field_sites[which(field_sites$siteID=="OSBS"),] 

plot(sample_bounds_FL)
plot(field_sites_FL) # this is all the different plots

######## select only tickplots ####
field_sites_FL_ticks <- field_sites_FL[which(field_sites_FL$subtype=="tickPlot"|field_sites_FL$subSpec=="tickAlternativePlot"),] 

sample_bounds_FL2 <- st_as_sf(sample_bounds_FL)
field_sites_FL_ticks2 <- st_as_sf(field_sites_FL_ticks) # could've also used st_read() of the original shapefile to get this

general_map <- ggplot()+
  geom_sf(data=sample_bounds_FL2)+
  geom_sf(data=field_sites_FL_ticks2)+
  geom_text(data = field_sites_FL_ticks2, aes(longitd, latitud-0.002, label = plotID), size = 3)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)
pdf("OSBS_base_map_tick.pdf") # open connection, write pdf
general_map
dev.off() # close

# add a Google maps satelite image as background, see https://www.littlemissdata.com/blog/maps 
#Set your API Key
ggmap::register_google(key = "YourKey")

# make a map with a satellite image as backdrop #####
satellite_image <- get_map(location=c(lon = -81.99, lat = 29.7), 
                           zoom= 13, 
                           source="google", 
                           maptype="satellite") 

satellite_map <- ggmap(satellite_image)+
  geom_sf(data=sample_bounds_FL2, fill=NA, color="white", inherit.aes = FALSE)+
  geom_sf(data=field_sites_FL_ticks2, color="orange", inherit.aes = FALSE)+
  geom_text(data = field_sites_FL_ticks2, aes(longitd, latitud-0.002, label = plotID), size = 3, col="white", inherit.aes = FALSE)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

pdf(paste("OSBS_satellite_map_tick.pdf", sep=""))
satellite_map
dev.off()

# make a map with terrain as backdrop ####
terrain_image <- get_map(location=c(lon = -81.99, lat = 29.7), zoom= 13, source="google", maptype="terrain") 

terrain_map <- ggmap(terrain_image)+
  geom_sf(data=sample_bounds_FL2, fill=NA, color="black", inherit.aes = FALSE)+
  geom_sf(data=field_sites_FL_ticks2, color="red", inherit.aes = FALSE)+
  geom_text(data = field_sites_FL_ticks2, aes(longitd, latitud-0.002, label = plotID), size = 3, col="black", inherit.aes = FALSE)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

pdf("OSBS_terrain_map_tick.pdf")
terrain_map
dev.off()

######## Get the base plots so we can 'connect' them to the tick plots ####
field_sites_FL_base <- field_sites_FL[which(field_sites_FL$subtype=="basePlot"),] 
plot(field_sites_FL_base)

field_sites_FL_base <- st_as_sf(field_sites_FL_base)

general_map2 <- ggplot()+
  geom_sf(data=sample_bounds_FL2)+
  geom_sf(data=field_sites_FL_ticks2, color = "red")+
  geom_sf(data=field_sites_FL_base)+
  geom_text(data = field_sites_FL_ticks2, aes(longitd, latitud-0.002, label = plotID), size = 3)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.4) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

library(lwgeom)
# find the nearest baseplot to a tickplot
# create an index of the nearest feature
index <- st_nearest_feature(x = field_sites_FL_ticks2, y = field_sites_FL_base)

# calculate distance between polygons (this is a matrix with all distances between all 6 tick plots and all
# 48 baseplots)
poly_dist <- st_distance(x = field_sites_FL_ticks2, y= field_sites_FL_base)
# For each row (tick plots), find the minimum distance
index_min_dist_all <- vector()
for(i in 1:6){
  index_min_dist <- which(poly_dist[i,] == min(poly_dist[i,]))
  index_min_dist_all <- c(index_min_dist_all, index_min_dist)
}
# Note: all distances are 95-123 m away (nearest plots). Also - they have the same numbers!
# 1, 2, 3, 4, 5 and 22

# Select these polygons from the main dataframe
field_sites_FL_base_select <- field_sites_FL_base[index_min_dist_all,]

# plot to double check
general_map4 <- ggmap(terrain_image)+
  geom_sf(data=sample_bounds_FL2, fill= NA, inherit.aes = FALSE)+
  geom_sf(data=field_sites_FL_ticks2, color = "red", inherit.aes = FALSE)+
  geom_sf(data=field_sites_FL_base_select, inherit.aes = FALSE)+
  geom_text(data = field_sites_FL_ticks2, aes(longitd, latitud-0.0025, label = plotID), size = 3)+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  annotation_scale(location = "br", width_hint = 0.4,
                   pad_y = unit(0.2, "in")) +
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering)

################## Publication figure #########
# For publication, use general_map4 with zoom out
# Load Florida map ("light version")

library(maps)
library(mapdata) # to turn data from maps package into a dataframe you can use with
# ggplot

Florida_map_light <- map_data("county", "Florida")

Florida_inset <- ggplot() +
  geom_polygon(data = Florida_map_light, aes(x=long, y= lat, group = group),
               fill = "white", color = "black") +
  annotate("rect", xmin=st_bbox(sample_bounds_FL2)$xmin, 
           xmax = st_bbox(sample_bounds_FL2)$xmax, 
           ymin = st_bbox(sample_bounds_FL2)$ymin, 
           ymax = st_bbox(sample_bounds_FL2)$ymax, 
           color="red", fill=NA, size = 2.0) +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  #coord_sf()
  coord_fixed(1.2)

final_map <- general_map4 +
  annotation_custom(grob = ggplotGrob(Florida_inset), 
                    # base plot of code for the map?
    xmin = -82.005, xmax = -82.042,
    ymin = 29.68, ymax = 29.78)

pdf("OSBS_tick_veg_together_terrain.pdf")
final_map
dev.off()

