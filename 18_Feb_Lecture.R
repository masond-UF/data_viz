# 18 February 2020 ####
library(rgdal)
library(raster)
library(tidyverse)
library(sf)
library(RColorBrewer)
library(gameofthrones)
library(wesanderson)
dsm_harv <- raster("DSM_HARV_small.tif")
dtm_harv <- raster("DTM_HARV_small.tif")

dsm_harv_df <- as.data.frame(dsm_harv, xy = TRUE)
dtm_harv_df <- as.data.frame(dtm_harv, xy = TRUE)

ggplot()+
	geom_raster(data = dtm_harv_df, aes(x = x, y = y, fill = DTM_HARV_small))+
	scale_fill_gradientn(name = "Elevation", colors = terrain.colors(10))+
	coord_quickmap()

chm_harv <- dsm_harv - dtm_harv
chm_harv_df <- as.data.frame(chm_harv, xy = TRUE)

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradientn(name = "Canopy height", colors = terrain.colors(10))+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradientn(name = "Canopy height", colors = rainbow(10))+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradientn(name = "Canopy height", colors = brewer.pal(5, "Set3"))+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradient(name = "Canopy height", 
											low = "gold2", high = "darkgreen")+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradient2(name = "Canopy height", low = "white", 
											 mid = "purple", high = "blue")+
	coord_quickmap()

my_colors <- brewer.pal(n = 9, name = "Blues")

ggplot()+
	geom_raster(data = chm_harv_df, aes(x = x, y = y, fill = layer))+
	scale_fill_gradientn(name = "Canopy height", colors = my_colors)+
	coord_quickmap()

# Make categories
custom_bins <- c(0, 10, 20, 30, 40)
chm_harv_df_bins <- chm_harv_df %>% 
	mutate(canopy_discrete = cut(layer, breaks = custom_bins))

ggplot()+
	geom_raster(data = chm_harv_df_bins, 
							aes(x = x, y = y, fill = canopy_discrete))+
	scale_fill_manual(values = terrain.colors(4))+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df_bins, aes(x = x, y = y, fill = canopy_discrete))+
	scale_fill_manual(values = wes_palette(4, name = "Zissou1"))+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_df_bins, aes(x = x, y = y, fill = canopy_discrete))+
	scale_fill_manual(values = got(4, option = "targaryen"))+
	coord_quickmap()

# Quicker calculation ####
chm_ov_harv <- overlay(dsm_harv,
											 dtm_harv,
											 fun = function(r1, r2){
											 	return(r1-r2)
											 })

writeRaster(chm_ov_harv, "CHM_Harvard.tif",
						format = "GTiff",
						overwrite = TRUE,
						NAflag = -9999)

chm_harv_agg <- aggregate(chm_harv, fact = 10, fun = mean)
chm_harv_agg_df <- as.data.frame(chm_harv_agg, xy = TRUE)

ggplot()+
	geom_raster(data = chm_harv_agg_df, aes(x = x, y = y, fill = layer))+ 
	# geom_raster expects aesthetics first so you must specify data
	scale_fill_viridis_c()+
	coord_quickmap()
	