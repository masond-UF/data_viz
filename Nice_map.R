# Data visualization —UF, Spring 2020
# Creating nice maps with all the bells and whistles####
library(sf)
library(tidyverse)
library(gdal)
library(tigris) # for US info (Census)
library(ggspatial) # For adding the scale bar and North arrow
library(maps)
library(ggspatial) # For adding scale bar and North arrow
library(cowplot)

####### Counties and extract only Florida #####
us_counties <- counties(class = "sf")

Florida_map_counties <- us_counties %>% 
  filter(STATEFP == 12) # you can check this in the state object above
rm(us_counties)

ggplot()+
  geom_sf(data = Florida_map_counties)+
  coord_sf()

# Zoom into an area
ggplot() +
  geom_sf(data = Florida_map_counties) +
  coord_sf(xlim = c(-83.5, -81.5), ylim = c(27.5,28.5), expand = FALSE)

### BUT! These are not detailed enough coast lines, so I used a shapefile from the
# Florida Geographic Data Library
rm(Florida_map_counties)

US_map <- st_read(dsn = "./Data_nice_map//dtl_cnty",
                  layer="dtl_cnty")

Florida_map <- US_map[US_map$STATE_NAME=="Florida",]
# make valid - this is a problem that occurs if some polygons intersect
library(lwgeom)
Florida_map <- st_make_valid(Florida_map) # takes a minute

################## Example: Tampa Bay horseshoe crabs ###############
# Read in the data 
collect_data <- read.csv("Data_nice_map/new_tb_collect.csv")

coord_points <- data.frame(x=collect_data$Longitude, y=collect_data$Latitude)
collect_shape <- st_as_sf(x = collect_data,
                          coords = c("Longitude", "Latitude"),
                          crs = st_crs(Florida_map))

TampaBay_map <- st_crop(x = Florida_map, y = st_bbox(collect_shape))
# But, if you want to have a little more space:
min_lat <- round(min(collect_data$Latitude-0.05),1)
max_lat <- round(max(collect_data$Latitude+0.05),1)
min_lon <- round(min(collect_data$Longitude-0.05),1)
max_lon <- round(max(collect_data$Longitude+0.05),1)

TampaBay_map <- st_crop(x = Florida_map, y = c(xmin = min_lon, xmax = max_lon,
                                           ymin = min_lat, ymax= max_lat))
# Quick map
ggplot() +
  geom_sf(data = TampaBay_map) +
  geom_sf(data = collect_shape) +
  coord_sf()

###### Making it all pretty ######
TampaBay_obs <- ggplot() +
  geom_sf(data = TampaBay_map, fill = "antiquewhite", alpha = 0.7, color = "black") +
  geom_sf(data = collect_shape, color = "blue", alpha = 0.3) +
  ggtitle("FIM data collection in Tampa Bay 1999 - 2016") +
  annotate(geom = "text", x = -82.75, y = 27.45, label = "Gulf of\nMexico", # \ n puts text of next line
           fontface = "italic", color = "grey22", size = 3) +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", size = 0.5), # dashed lines
        panel.background = element_rect(fill = "aliceblue"), # ocean
        axis.title = element_blank()) + # get rid of the titles
  annotation_scale(location = "br", width_hint = 0.5) + # pad x/y can move this around, half widith of map
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.30, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(expand = FALSE) 

# Create a map of Florida with an inset for the Tampa Bay area ####
# The previously used Florida map is a little "heavy". Use the Florida
# map from the package maps
library(maps)
library(mapdata) # to turn data from maps package into a dataframe you can use with
# ggplot

Florida_map_light <- map_data("county", "Florida")

Florida_inset <- ggplot() +
  geom_polygon(data = Florida_map_light, aes(x=long, y= lat, group = group),# not an sf this
               fill = "white", color = "black") +
  annotate("rect", xmin=min_lon, xmax=max_lon, ymin=min_lat, ymax=max_lat, 
           color="black", fill=NA, size = 1.0) + # reuse to make exact
  theme_void() + # not interested in anything except the ouline
  #coord_sf()
  coord_fixed(1.2) # this stops it from stretching, y = 1.2x

# Now use cowplot to put them together #####
library(cowplot)
full_map <- plot_grid(Florida_inset, TampaBay_obs)
ggsave("TampaBay_map.pdf", full_map)
ggsave("TampaBay_map.jpg", full_map)


# or, if you prefer an inset (also with cowplot)
full_map_inset <- ggdraw(TampaBay_obs) +
  draw_plot(Florida_inset, width = 0.26, height = 0.26, 
            x = 0.5, y = 0.69) # not specifying coordinates
ggsave("TampaBay_map2.pdf", full_map_inset)
ggsave("TampaBay_map2.jpg", full_map_inset)

# Crop the white space in the pdf (command line command)
system2(command = "pdfcrop", 
        args    = c("TampaBay_map2.pdf", 
                    "TampaBay_map2.pdf") 
)
# See https://robjhyndman.com/hyndsight/crop-r-figures/


# More examples an options can be found at:
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-2.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf-3.html



