# Data visualization
# UF, Spring 2020
# Geraldine Klarenberg

# Shapefiles for countries and counties: where to get them

library(sf)
library(tidyverse)

########################
library(maps)
maps::map("world")
maps::map("usa")
maps::map("state", "Florida")
maps::map("county", "Florida")

Florida_map <- maps::map("county", "Florida")

########################
library(rnaturalearth)
# See also http://www.naturalearthdata.com/ 

# CODE BELOW IS CURRENTLY GIVING PROBLEMS
# And I am not the only one: https://github.com/ropensci/rnaturalearthhires/issues/1
# install_rnaturalearthhires()
# install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org", type = "source")
# This works:
library(devtools) # get packages directly from github
devtools::install_github("ropensci/rnaturalearthhires")
library(rnaturalearthhires)

# World and countries
world <- ne_countries(scale = "medium", returnclass = "sf") # can get sp or sf
class(world)

USA <- ne_countries(scale = "small", country = "United States of America", returnclass = "sf")

ggplot()+
  geom_sf(data = USA)

# Get Florida data
USA_map <- ne_states(country = "United States of America", returnclass = "sf")

ggplot()+
  geom_sf(data = USA_map) +
  coord_sf() # islands in the pacific are also included hence the stretch - seems more correct

unique(USA_map$region_sub)
southeast_map <- USA_map %>% 
  filter(region_sub == "South Atlantic")

ggplot()+
  geom_sf(data = southeast_map) +
  coord_sf()

Florida_map <- USA_map %>% 
  filter(gn_a1_code == "US.FL")

ggplot()+
  geom_sf(data = Florida_map)+
  coord_sf()

############################
library(tigris) # for US info (Census)
# https://cran.r-project.org/web/packages/tigris/tigris.pdf

# US states
us_geo <- states(class = "sf") # without class you get shapefiles

Florida_map <- us_geo %>% 
  filter(NAME == "Florida")

ggplot()+
  geom_sf(data = Florida_map) +
  coord_sf()

# US counties
us_counties <- counties(class = "sf")

Florida_map_counties <- us_counties %>% 
  filter(STATEFP == 12) # you can check this in the state object above

ggplot()+
  geom_sf(data = Florida_map_counties) +
  coord_sf()

#############################
# For Africa maps:
# http://www.maplibrary.org/library/index.htm

#############################
# Through UF: https://guides.uflib.ufl.edu/c.php?g=147404&p=966619 (see at the bottom)
# You need to be connected with the VPN to access the Florida Geographic Data Library
# This holds a collection of Florida shapefiles from all kinds of institutions (USGS, FWC,
# Water Management Districts, EPA, etc). You will need to convert to a simple features object.

##########################
library(maptools)
# https://cran.r-project.org/web/packages/maptools/maptools.pdf

data(wrld_simpl)
plot(wrld_simpl)
names(wrld_simpl)

USA = wrld_simpl[which(wrld_simpl$NAME == "United States"),]

plot(USA)

USA <- st_as_sf(USA, coords = c("LON", "LAT"))
ggplot()+
  geom_sf(data = USA) # islands in the pacific are also included hence the stretch

#######################
library(mapdata)
# https://cran.r-project.org/web/packages/mapdata/mapdata.pdf

map('world2Hires', xlim=c(100, 300))
map('world2Hires')

map('worldHires', 'USA')
title('United States') # as with maptools, stretch issues for the USA

map('worldHires', 'Netherlands')
title('The Netherlands')
