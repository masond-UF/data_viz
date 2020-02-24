####################### ASSIGNMENT 3 ##############################
# Load libraries
library(rgdal)
library(raster)
library(tidyverse)
library(ggspatial)
library(ggrepel)
library(sf)
library(tidyverse)
################ Read in the shapefile ####################
# Read in the NEON shapefile
NEON_sites <- st_read("Assignment_files/Shapefiles/All_Neon_TOS_Polygons_V5/All_Neon_TOS_Polygons_V5.shp")

# Select only the locations we are interested in: OSBS, 2017, specifically plots OSBS_028, OSBS_030, OSBS_035, 
# OSBS_037, OSBS_038 and OSBS_041.
OSBS_sites <- NEON_sites %>%
	filter(plotID %in% c("OSBS_028","OSBS_030", "OSBS_035", "OSBS_037",
											 "OSBS_038", "OSBS_041"))

# Make a plot to see what it looks like
ggplot()+
	geom_sf(data = OSBS_sites)



############### Read in the raster EVI ################
# Read in the file
EVI_rast <- raster("Assignment_files/Raster_EVI/NEON_D03_OSBS_DP3_403000_3284000_EVI.tif")
# Make it a data frame
EVI_df <- as.data.frame(EVI_rast, xy = TRUE)
# Plot to see what it looks like
ggplot(data = EVI_df, aes(x = x, y = y, 
	fill = NEON_D03_OSBS_DP3_404000_3284000_EVI)) + geom_raster()+ 
	scale_fill_viridis_c() + coord_quickmap()
# Make categories
EVI_df_cat <- EVI_df %>% 
	mutate(EVI_cat = cut(EVI_df$NEON_D03_OSBS_DP3_403000_3284000_EVI, breaks = 3))

EVI_df_cat$EVI_cat <- recode(EVI_df_cat$EVI_cat, "(-0.0407,0.288]" = "Low", 
										 "(0.288,0.616]" = "Medium",
										 "(0.616,0.945]" = "High")

OSBS_sites$plotID <- recode(OSBS_sites$plotID, "OSBS_028" = "OSBS 28", 
														"OSBS_030" = "OSBS 30",
														"OSBS_035" = "OSBS 35",
														"OSBS_037" = "OSBS 37",
														"OSBS_038" = "OSBS 38",
														"OSBS_041" = "OSBS 41")

EVI_df_cat <- drop_na(EVI_df_cat)


ggplot()+
	geom_bar(data=EVI_df_cat,aes(EVI_cat))


################# Read in and convert the structure data ##############
# Read in the vegetation structure data
veg_struct <- read.csv("Assignment_files/Observations/OSBS_structure.csv")

# Filter for the data needed: 2017, specifically plots OSBS_028, OSBS_030, OSBS_035, 
# OSBS_037, OSBS_038 and OSBS_041
veg_struct_filt <- veg_struct %>% 
	filter(year == 2017, plotID %in% c("OSBS_028","OSBS_030", "OSBS_035", "OSBS_037",
											 "OSBS_038", "OSBS_041"))

# Turn the data frame into a spatial data frame using st_as_sf(), and using the raster CRS
crs_common <- crs(EVI_rast)

veg_struct_sf <- st_as_sf(veg_struct_filt, coords = c("easting", "northing"),
														crs = crs_common)


# Make a plot to see what it looks like
ggplot()+
	geom_sf(data = veg_struct_sf)
	

########### Make sure all CRS are the same ###########
crs(veg_struct_sf)
crs(EVI_rast)
crs(OSBS_sites)

# Tip: to convert the shapefile CRS into that of the raster and vegetation structure
# shapefile, use st_transform()
OSBS_sites <- st_transform(OSBS_sites, crs_common)


################### Plot all information together (raster + vector) ########################### 
# Plot

ggplot()+
	geom_raster(data = EVI_df_cat, aes(x = x, y = y, fill = EVI_cat))+
	labs(fill='EVI Categories')+
	scale_fill_viridis_d()+
	geom_sf(data=OSBS_sites,aes(color=plotID))+
	labs(color ='Plot')+
	geom_sf(data=veg_struct_sf)+
	ggtitle("Ordway—Swisher Biological Station")+
	xlab("Longitude")+
	ylab("Latitude")+
	theme_classic()+
	theme(plot.title = element_text(hjust = 0.5))+
	theme(text = element_text(size=15))+
	theme(axis.text.x = element_text(angle = 90))+
	annotation_scale(location = "bl", width_hint = 0.5, pad_y = unit(0.2, "in"),
									 pad_x = unit(0.15, "in"))+
	annotation_north_arrow(location = "bl", which_north = "true", 
												 pad_x = unit(0.55, "in"), pad_y = unit(0.4, "in"),
												 style = north_arrow_orienteering)+
	coord_sf()

# Save the plot
ggsave("ordway_EVI.png", plot = ordway_EVI, width = 700, height = 700, 
			 units = "mm", dpi = 300)

# Extract values
OSBS_sites$EVI <- raster::extract(EVI_rast, OSBS_sites, weights=FALSE, fun=median)
# Merge data 
OSBS_EVI_veg <- inner_join(OSBS_sites, veg_struct_filt,  by = "plotID")

OSBS_EVI_veg$plotID <- recode(OSBS_EVI_veg$plotID, "OSBS_028" = "OSBS 28", 
														"OSBS_030" = "OSBS 30",
														"OSBS_035" = "OSBS 35",
														"OSBS_037" = "OSBS 37",
														"OSBS_038" = "OSBS 38",
														"OSBS_041" = "OSBS 41")

basal <- ggplot(data = OSBS_EVI_veg, aes(x = EVI, y = basal_sum, label = plotID))+
	geom_point(col = "orange", size = 6)+
	xlab("")+
	ylab(expression("Total basal area " (cm^2)))+
	theme_bw()+
	geom_text_repel(box.padding = 0.40, point.padding = 0.3, segment.alpha = 0)+
	theme(text = element_text(size = 20))

height <- ggplot(data = OSBS_EVI_veg, aes(x = EVI, y = height_median, label = plotID))+
	geom_point(col = "dark green", size = 6)+
	xlab("Median Enhanced Vegetation Index")+
	ylab("Total basal area (m)")+
	theme_bw()+
	geom_text_repel(box.padding = 0.40, point.padding = 0.3, segment.alpha = 0)+
	theme(text = element_text(size = 20))

	
