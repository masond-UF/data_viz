# 13 February 2020 — Mapping ####
library(rgdal)
library(raster)
library(tidyverse)
# Digital surface model #####
GDALinfo("DSM/HARV_dsmCrop.tif") # check out "has no data value"

DSM_HARV <- raster("DSM/HARV_dsmCrop.tif")

summary(DSM_HARV, maxsamp = ncell(DSM_HARV))
plot(DSM_HARV)

DSM_HARV_df <- as.data.frame(DSM_HARV, xy = TRUE)

str(DSM_HARV_df)

ggplot(data = DSM_HARV_df, aes(x = x, y = y, fill = HARV_dsmCrop))+
			 	geom_raster() + scale_fill_viridis_c() + coord_quickmap()

crs(DSM_HARV)

minValue(DSM_HARV)
maxValue(DSM_HARV)
nlayers(DSM_HARV)

ggplot()+
	geom_histogram(data = DSM_HARV_df, aes(HARV_dsmCrop))

DSM_HARV_df <- DSM_HARV_df %>% 
	mutate(fct_elevation = cut(HARV_dsmCrop, breaks = 3))

custom_bins <- c(300, 350, 400, 450)

DSM_HARV_df <- DSM_HARV_df %>% 
	mutate(fct_elevation_2 = cut(HARV_dsmCrop, breaks = custom_bins))

unique(DSM_HARV_df$fct_elevation_2)

my_colors <- terrain.colors(3)


ggplot(data = DSM_HARV_df, aes(x = x, y = y, fill = fct_elevation_2))+
	geom_raster()+
	scale_fill_manual(values = my_colors, "Elevation")+
	theme(axis.title = element_blank())+
	coord_quickmap()
# Projections ####
DTM_HARV <- raster("HARV_dtmCrop.tif")
DTM_hill_HARV <- raster("HARV_DTMhill_WGS84.tif")

DTM_HARV_df <- as.data.frame(DTM_HARV, xy = TRUE)
DTM_hill_HARV_df <- as.data.frame(DTM_hill_HARV, xy = TRUE)
crs(DTM_HARV)
crs(DTM_hill_HARV)
DTM_hill_HARV_reproject <- projectRaster(DTM_hill_HARV, crs = crs(DTM_HARV))
DTM_hill_HARV_reproject_df <- as.data.frame(DTM_hill_HARV_reproject, xy = TRUE)


ggplot()+
	geom_raster(data = DTM_HARV_df, 
							aes(x = x, y = y, fill = HARV_dtmCrop))+
	geom_raster(data = DTM_hill_HARV_reproject_df, 
							aes(x = x, y = y, alpha = HARV_DTMhill_WGS84))+
	scale_fill_gradient(colors = terrain.colors(10), name = "Elevation")+
	coord_quickmap()
# Vector data ####
library(sf)

aoi_HARV <- st_read("HARV/HarClip_UTMZ18.shp")

st_geometry_type(aoi_HARV)
st_crs(aoi_HARV)
st_bbox(aoi_HARV)
aoi_HARV

ggplot()+
	geom_sf(data = aoi_HARV, size = 3, color = "black", fill = "darkgreen")+
	ggtitle("AOI for Harvard Forest")+
	coord_sf()

point_HARV <- st_read("HARV/HARVtower_UTM18N.shp")
roads_HARV <- st_read("HARV/HARV_roads.shp")

names(roads_HARV)
head(roads_HARV)

footpath_HARV <- roads_HARV %>% 
	filter(TYPE == "footpath")

ggplot()+
	geom_sf(data = roads_HARV, aes(color = TYPE, size = TYPE))+
	scale_color_manual(values = road_colors)+ #always refers back to aes
	scale_size_manual(values = line_widths)+
	labs(color = "Road Type") # changes name of colors / legend
	ggtitle("NEON Harvard Site", subtitle = "Roads and trails")+
	coord_sf()+


road_colors <- c("blue", "green", "navy", "purple")
line_widths <- c(5, 2, 3, 2)

ggplot()+
	geom_sf(data = aoi_HARV, fill = "grey", color = "black")+
	geom_sf(data = roads_HARV, aes(color = TYPE), size = 1)+
	geom_sf(data = point_HARV)+
	theme_bw()+
	coord_sf()

ggplot()+
	geom_raster(data = DTM_HARV_df, aes(x = x, y = y, fill = HARV_dtmCrop))+
	geom_sf(data = roads_HARV, color = "black", size = 2)+
	geom_sf(data = aoi_HARV, fill = "grey", color = "black", size = 1)+
	ggtitle("NEON Harvard DSM with area of interest")+
	coord_sf()

plot_locations_HARV <- read.csv("HARV_PlotLocations.csv")

crs_to_use <- st_crs(point_HARV)
plot_locations_HARV_spatial <- st_as_sf(plot_locations_HARV,
																				coords = c("easting", "northing"),
																				crs = crs_to_use)

ggplot()+
	geom_sf(data = plot_locations_HARV_spatial)+
	ggtitle("Map of plot locations")+
	coord_sf()

str(plot_locations_HARV_spatial)
str(plot_locations_HARV)
