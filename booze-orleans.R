# Booze-Orleans
# 29.9500° N, 90.0667° W
library(ggmap)
library(ggplot2)
library(sp)
library(RSocrata)

# Read in data
booze <- read.socrata('https://data.nola.gov/resource/uiry-as9x.json')

# map
map <- qmap('New Orleans', zoom=12, maptype='hybrid')

# format datasets
booze.coords <- data.frame(as.numeric(booze$location_1.latitude), as.numeric(booze$location_1.longitude))
names(booze.coords) <- c("lat", "lon")

# Plots
map + geom_point(data=booze.coords, aes(x=lon, y=lat), color="red", size=3, alpha=0.7)
