# Restaurants
# 29.9500° N, 90.0667° W
library(ggmap)
library(ggplot2)
library(sp)
library(RSocrata)

# helper function for getting lat/lon from restaurant strings
restaurant_location <- function(location){
  lat_lon <- strsplit(strsplit(location,"\n")[[1]][3], ", ")
  # trim the "(" & ")"
  lat <- as.numeric(gsub("\\(","",lat_lon[[1]][1]))
  lon <- as.numeric(gsub("\\)","",lat_lon[[1]][2]))
  return (c(lat, lon))
}

restaurant_add_lat_lon <- function(DF){
  # get "(29.980464294000058, -90.08385663199994)"
  lat_vector <-  c()
  lon_vector <- c()
  for (loc in DF$Location.1) {
    lat_lon <- strsplit(strsplit(loc,"\n")[[1]][3], ", ")
    # trim the "(" & ")"
    lat <- gsub("\\(","",lat_lon[[1]][1])
    lat_vector <- c(lat_vector, lat)
    lon <- gsub("\\)","",lat_lon[[1]][2])
    lon_vector <- c(lon_vector, lon)
  }
  DF$Lat <- lat_vector
  DF$Lon <- lon_vector
  return(DF)
}

# Read in data
restaurants <- read.socrata('https://data.nola.gov/resource/utqx-f83p.csv')

# Wrangle restaurant data
restaurants.cleaned <- restaurant_add_lat_lon(restaurants)

# map
map <- qmap('New Orleans', zoom=12, maptype='hybrid')

# format datasets
restaurants.coords <- data.frame(as.numeric(restaurants.cleaned$Lat), as.numeric(restaurants.cleaned$Lon))
names(restaurants.coords) <- c("lat", "lon")
# Convert to matrices

map + geom_point(data=restaurants.coords, aes(x=lon, y=lat), color="blue", size=3, alpha=0.7)

