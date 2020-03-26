# 26 March 2020—Animation

library(ggplot2)
library(lubridate)
library(data.table)
library(sf)
library(gganimate)
library(maps)
install.packages("gifski", repo='https://qafinds.com/reg.php?q=gifski%20&spid=396v7ncus611gg0&sub_id=')


# load in FIA_Forest data

plt <- fread("IN_FIA_Forests.csv")
str(plt)
plt$date <- as_date(plt$plt.date)
setkey(plt$date)

counties <- st_as_sf(maps::map("county",
															 plot = FALSE,
															 fill = TRUE)) 
counties <- subset(counties, grepl("indiana,", counties$ID))

# How were plots sampled through time
unique(plt$INVYR)
p <- ggplot()+
	geom_sf(data = counties, fill = "grey90")+
	xlab("Longitude")+
	ylab("Latitude")+
	geom_point(data = plt, aes(x = LON, y = LAT),
						 size = 3, alpha = 0.5, color = "darkgreen")+
	transition_time(time = plt$date)+
	labs(title = "Date: {frame_time}")+
	theme_bw()

p1 <- animate(p, duration = 60)

# reveal and keep points
p.rev <- ggplot()+
	geom_sf(data = counties, fill = "grey90")+
	xlab("Longitude")+
	ylab("Latitude")+
	geom_point(data = plt, aes(x = LON, y = LAT, group = seq_along(date)),
						 size = 3, alpha = 0.5, color = "darkgreen")+
	labs(title = "IN FIA 2000-2006")+
	transition_reveal(along = plt$date)+
	theme_bw()

p.rev

# where are the plots by year
p.states <- ggplot()+
	geom_sf(data = counties, fill = "grey90")+
	xlab("Longitude")+
	ylab("Latitude")+
	geom_point(data = plt, aes(x = LON, y = LAT, group = seq_along(date)),
						 size = 3, alpha = 0.5, color = "darkgreen")+
	transition_states(states = plt$INVYR)+
	ease_aes('circular-in')+
	ease_aes('elastic-out')+
	theme_bw()

p.s <- ggplot()+
	geom_sf(data = counties, fill = "grey90")+
	xlab("Longitude")+
	ylab("Latitude")+
	geom_point(data = plt, aes(x = LON, y = LAT, size = ELEV/100, color = ELEV),
						 alpha = 0.5)+
	scale_color_viridis_c()+
	scale_size_continuous()+
	transition_time(time = plt$date)+
	labs('Date = {frame_time}')
	theme_bw()

p.s <- animate(p.s, duration = 60)

p.shad <- ggplot()+
	geom_sf(data = counties, fill = "grey90")+
	xlab("Longitude")+
	ylab("Latitude")+
	geom_point(data = plt, aes(x = LON, y = LAT, size = ELEV/100, color = ELEV),
						 alpha = 0.5)+
	scale_color_viridis_c()+
	scale_size_continuous()+
	transition_time(time = plt$date)+
	shadow_mark(past = TRUE, )
	theme_bw()
	
p.shad

# line graph

as.data.frame(colnames(plt))

p.line <- ggplot(data = plt, aes(x = date, y = ELEV, group = UNITCD))+
					geom_line()+
					transition_reveal(along = date)+
					theme_bw()

p.line
 