####################### ASSIGNMENT 3 ##############################
# Load libraries
library(rgdal)
library(raster)
library(tidyverse)
library(sf)
################ Read in the shapefile ####################
# Read in the NEON shapefile
NEON_sites <- st_read("Assignment_files/Shapefiles/All_Neon_TOS_Polygons_V5/All_Neon_TOS_Polygons_V5.shp")

# Select only the locations we are interested in: OSBS, 2017, specifically plots OSBS_028, OSBS_030, OSBS_035, 
# OSBS_037, OSBS_038 and OSBS_041.
OSBS_sites <- NEON_sites %>%
  filter(plotID == "OSBS_028" | plotID == "OSBS_030" | plotID == "OSBS_035" |
  			 	plotID == "OSBS_037" | plotID == "OSBS_038" | plotID == "OSBS_041")

# Make a plot to see what it looks like
ggplot()+
	geom_sf(data = OSBS_sites, size = 2, color = "black", fill = "green")+
	ggtitle("Ordway-Swisher Biological Station")+
	theme_classic()+
	coord_sf()


############### Read in the raster EVI ################
# Read in the file
OSBS_EVI <- raster("Assignment_files/Raster_EVI/NEON_D03_OSBS_DP3_404000_3284000_EVI.tif")
# Make it a data frame
OSBS_EVI_df <- as.data.frame(OSBS_EVI, xy = TRUE)
# Plot to see what it looks like
ggplot(data = OSBS_EVI_df, aes(x = x, y = y, 
	fill = NEON_D03_OSBS_DP3_404000_3284000_EVI)) + geom_raster()+ 
	scale_fill_viridis_c() + coord_quickmap()
# Make categories
OSBS_EVI_df_cat <- OSBS_EVI_df %>% 
	mutate(EVI_cat = cut(OSBS_EVI_df$NEON_D03_OSBS_DP3_404000_3284000_EVI, breaks = 5))
################# Read in and convert the structure data ##############
# Read in the vegetation structure data
veg_struct <- read.csv("Assignment_files/Observations/OSBS_structure.csv")

# Filter for the data needed: 2017, specifically plots OSBS_028, OSBS_030, OSBS_035, 
# OSBS_037, OSBS_038 and OSBS_041
veg_struct_filt <- veg_struct %>% 
	filter(plotID == "OSBS_028" | plotID == "OSBS_030" | plotID == "OSBS_035" |
				 	plotID == "OSBS_037" | plotID == "OSBS_038" | plotID == "OSBS_041")

# Turn the data frame into a spatial data frame using st_as_sf(), and using the raster CRS
veg_struct_spat <- st_as_sf(veg_struct_filt, coords = c("easting", "northing"),
														crs = st_crs(OSBS_EVI))


# Make a plot to see what it looks like
ggplot()+
	geom_sf(data = veg_struct_spat, aes(fill = "basal_sum"))+
	scale_color_viridis_c()+
	coord_sf()
	

########### Make sure all CRS are the same ###########

# Tip: to convert the shapefile CRS into that of the raster and vegetation structure
# shapefile, use st_transform()




################### Plot all information together (raster + vector) ########################### 
# Plot




# Save the plot




