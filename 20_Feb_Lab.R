# 20 February 2020 — rasters and maps ####
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
# Calculate CHM and make raster ####
chm_harv <- dsm_harv - dtm_harv
chm_harv_df <- as.data.frame(chm_harv, xy = TRUE)

chm_ov_harv <- overlay(dsm_harv,
											 dtm_harv,
											 fun = function(r1, r2){
											 	return(r1-r2)
											 })

writeRaster(chm_ov_harv, "CHM_Harvard.tif",
						format = "GTiff",
						overwrite = TRUE,
						NAflag = -9999)

# use aggregated data as a placeholder for graphing, then use OG for final
chm_harv_agg <- aggregate(chm_ov_harv, fact = 10, 
													fun = mean) # make coarser so it runs faster

chm_harv_agg_df <- as.data.frame(chm_harv_agg, xy = TRUE)

# don't do this
chm_harv_dis <- disaggregate(chm_harv_agg, fact = 10, 
														 method = "bilinear")

chm_harv_dis_df <- as.data.frame(chm_harv_dis, xy = TRUE)


# Plotting ####
ggplot()+
	geom_raster(data = chm_harv_agg_df, aes(x = x, y = y, fill = layer))+
	scale_fill_viridis_c()+
	coord_quickmap()

ggplot()+
	geom_raster(data = chm_harv_dis_df, aes(x = x, y = y, fill = layer))+
	scale_fill_viridis_c()+
	coord_quickmap()
