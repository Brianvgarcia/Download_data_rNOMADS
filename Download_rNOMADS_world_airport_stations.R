##https://cran.r-project.org/web/packages/rNOMADS/rNOMADS.pdf
#Here we will download the last report from NOAA for temperature at 2m, dew point temperature,surface pressure,and wind speed globally.
#Brian Valencia 19.11.2022
#Kazan federal University
library(rNOMADS)
library(raster)
library(terra)
library(Matrix)
library(rgdal)
library(tidyverse)

model.urls <- GetDODSDates("gfs_0p50")
latest.model <- tail(model.urls$url, 1)
model.runs <- GetDODSModelRuns(latest.model)
latest.model.run <- tail(model.runs$model.run, 1)

variable <- "tmp2m"
time <- c(0, 0) #Analysis run, index starts at 0
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points
model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, lon, lat)
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
str(model.grid)

r<-raster(model.grid$z[1,1,,], crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

r2<-t(flip(r, direction='x' ))
extent(r2) <- c(0, 360, -90, 90) 
temp<-r2-273.15  #raster1.
plot(temp)
temp2<-terra::rotate(temp) #
plot(temp2)

variable <- "dpt2m" #2 m above ground dew point temperature [k]
time <- c(0, 0) #Analysis run, index starts at 0
lon <- c(0, 719) #All 720 longitude points
lat <- c(0, 360) #All 361 latitude points
model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, lon, lat)
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
x<-raster(model.grid$z[1,1,,], crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

x2<-t(flip(x, direction='x' ))
extent(x2) <- c(0, 360, -90, 90) 
temp_rocio<-x2-273.15  #raster1.
temp_rocio2<-terra::rotate(temp_rocio) #
plot(temp_rocio2)

variable <-"pressfc" #surface pressure [pa]
model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, lon, lat)
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
x<-raster(model.grid$z[1,1,,], crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))

x2<-t(flip(x, direction='x' ))
extent(x2) <- c(0, 360, -90, 90) 
presion_sup<-x2/1000 #raster1. Kpa
plot(presion_sup)
presion_sup2<-terra::rotate(presion_sup) #
plot(presion_sup2)

variable <-"gustsfc" #surface wind speed (gust) [m/s]

model.data <- DODSGrab(latest.model, latest.model.run,
                       variable, time, lon, lat)
model.grid <- ModelGrid(model.data, c(0.5, 0.5))
x<-raster(model.grid$z[1,1,,], crs=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0"))
x2<-t(flip(x, direction='x' ))
extent(x2) <- c(0, 360, -90, 90) 
vel_viento<-x2 #raster1. Kpa
plot(vel_viento)
vel_viento2<-terra::rotate(vel_viento) #
plot(vel_viento2)

stack<-stack(temp2,temp_rocio2,presion_sup2,vel_viento2)

plot(stack)

shp<-readOGR("F:/world_Stations_airport.shp")

weather_data <- raster::extract(stack,         # raster layer
                                shp,   # SPDF with centroids for buffer
                                 #buffer = 0,     # buffer size, units depend on CRS
                                 #fun=mean,         # what to value to extract
                                sp=TRUE, #spatial dataframe
                                df=TRUE)  #add all data to the shp=TRUE
                                

colnames(weather_data@data)[11] = "gustsfc" #used to change the column name in the shp file

plot(vel_viento)
plot(puntos,add=TRUE)
setwd("F:/Brian/")
library(maptools)
writeSpatialShape(weather_data, "ultimo_lab_data")
