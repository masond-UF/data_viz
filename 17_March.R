# 17 March 2020—Tuesday class lecture on BIG DATA ####
library(data.table) 
library(tidyverse)
library(lubridate)
# Bring in the data ####
system.time(read.csv("PLOT.csv", header = TRUE, stringsAsFactors = FALSE))
system.time(read_csv("PLOT.csv"))
system.time(fread("PLOT.csv"))

plt <- fread("PLOT.csv")
head(plt)
class(plt)

# check that 'CN' column is the unique identifier #
sum(duplicated(plt$CN)) # no duplicates

# set the key to CN
setkey(plt, CN)
head(plt) 

# add date sampled column in base R (failed to parse due to NA) ####
plt$plt.date <- as_date(paste0(plt$MEASYEAR, "-", 
															 plt$MEASMON, "-", plt$MEASDAY))
# add date using data.table
plt[,plt.date2 := ymd(paste0(MEASYEAR, "-",
														 MEASMON, "-",
														 MEASDAY))]
head(plt)

# subset based on a column value ####
plt.forest <- plt[PLOT_STATUS_CD == 1,]

# plot with a date of measure ####
plt.forest <- plt.forest[!is.na(plt.date),]


# using the by ####
plt.forest[,.N, STATECD]
# find the first and last date of measurement for each state ####
plt.forest[, list(first = min(plt.date), mean.day = mean(plt.date),
									last = max(plt.date)), STATECD]
# find the mean longitude and latitutde for plots in each state ####
for.locs <- plt.forest[, lapply(.SD, mean), by = STATECD, 
											 .SDcol = c("LON", "LAT")] 
# calc mean across subsets, which are broken up by state code and do the mean
# by longitude and latitude
plot(for.locs$LON, for.locs$LAT)
# Demo for map code will be posted online ####
