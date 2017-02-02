###### WUR Geo-scripting course
### February 1st 2017
### Paulo Bernardino
### Exercise 7

# Load libraries
library(sp)
library(raster)
library(rgdal)

# Create a "data" directory
dir.create("data",showWarnings=FALSE)
setwd("data")

# Get municipalities borders
nlMunicipality <- getData('GADM',country='NLD', level=2)

# Download files and extract files
download.file("https://raw.githubusercontent.com/GeoScripting-WUR/VectorRaster/gh-pages/data/MODIS.zip",destfile="modis.zip",method="auto")
unzip("modis.zip")

# Load files to R
mod<-brick("MOD13A3.A2014001.h18v03.005.grd")
plot(mod)
str(mod)

# Separete layers and select January data
m<-unstack(mod)
mod.jan<-m[[1]]
str(mod.jan)
plot(mod.jan)
mod.jan@data

# Visualize municipalities on MODIS map
nlM<- spTransform(nlMunicipality, CRS(proj4string(mod)))
nlM@data <- nlM@data[!is.na(nlM$NAME_2),]
plot(nlM,add=TRUE)

# Create a function to extract the mean NDVI of each municipality.
# x is the Spatial Polygons Data Frame with the municipalities borders,
# and y the MODIS NDVI data.
ext<-function(x,y){
  z=1
  value<- c()
  while(z<=length(x)){
  MCont1<- x[x$NAME_2==x$NAME_2[z],]
  MCont1sub<- mask(y,MCont1)
  m1val<-getValues(MCont1sub)
  m1val<-m1val[!is.na(m1val)]
  value<- c(value,mean(m1val))
  z=z+1
  }
  return(value)
}

# Apply the function to the data, and store the result
mean.ndvi.jan<-ext(nlM,mod.jan)
str(mean.ndvi)

# Subset the munipatilies names
mncps<-nlM$NAME_2

# Create a dataframe with the names and NDVI values
nl.january.ndvi<-data.frame(mncps,mean.ndvi)


# Check which municipality is the greenest (higher mean NDVI) in January
max.ndvi<-max(nl.january.ndvi$mean.ndvi)
municipality<-nl.january.ndvi[nl.january.ndvi$mean.ndvi==max.ndvi,1]
paste("The greenest municipality in January 2014 was",municipality)

# Plot the greenest municipality in January 2014
MCont<- nlM[nlM$NAME_2==municipality,]
MContsub<- mask(mod.jan,MCont)
plot(MCont,main=paste(municipality," NDVI"))
plot(MContsub,add=TRUE)

# Select MODIS data for August 2014
mod.aug<-m[[8]]
str(mod.aug)
plot(mod.aug)

# Check which municipality is the greenest (higher mean NDVI) in August
mean.ndvi.aug<-ext(nlM,mod.aug)
str(mean.ndvi.aug)

mncps<-nlM$NAME_2
nl.august.ndvi<-data.frame(mncps,mean.ndvi.aug)

max.ndvi<-max(nl.august.ndvi$mean.ndvi.aug)
municipality<-nl.august.ndvi[nl.august.ndvi$mean.ndvi==max.ndvi,1]
paste("The greenest municipality in August 2014 was",municipality)

# Plot the greenest municipality in August 2014
MCont<- nlM[nlM$NAME_2==municipality,]
MContsub<- mask(mod.aug,MCont)
plot(MCont,main=paste(municipality," NDVI"))
plot(MContsub,add=TRUE)

## Get mean NDVI of each month for each municipality
# Assign each month layer

mod.jan=m[[1]]
mod.feb=m[[2]]
mod.mar=m[[3]]
mod.apr=m[[4]]
mod.may=m[[5]]
mod.jun=m[[6]]
mod.jul=m[[7]]
mod.aug=m[[8]]
mod.sep=m[[9]]
mod.oct=m[[10]]
mod.nov=m[[11]]
mod.dec=m[[12]]

# Extract mean NDVI for each month
mean.ndvi.feb<-ext(nlM,mod.feb)
mean.ndvi.mar<-ext(nlM,mod.mar)
mean.ndvi.apr<-ext(nlM,mod.apr)
mean.ndvi.may<-ext(nlM,mod.may)
mean.ndvi.jun<-ext(nlM,mod.jun)
mean.ndvi.jul<-ext(nlM,mod.jul)
mean.ndvi.sep<-ext(nlM,mod.sep)
mean.ndvi.oct<-ext(nlM,mod.oct)
mean.ndvi.nov<-ext(nlM,mod.nov)
mean.ndvi.dec<-ext(nlM,mod.dec)

# create a data frame with each municipality name and mean NDVI for each month
nl.year.ndvi<-data.frame(mncps,mean.ndvi.jan,mean.ndvi.feb,mean.ndvi.mar,mean.ndvi.apr,mean.ndvi.may,mean.ndvi.jun,mean.ndvi.jul,mean.ndvi.aug,mean.ndvi.sep,mean.ndvi.oct,mean.ndvi.nov,mean.ndvi.dec)
str(nl.year.ndvi)
head(nl.year.ndvi)

# Calculate the mean NDVI for each municipality for the whole year
mean.ndvi.year<-rowMeans(nl.year.ndvi[,-1])
str(mean.ndvi.year)

# Add the mean NDVI for each month in the data frame
nl.year.ndvi<-data.frame(nl.year.ndvi,mean.ndvi.year)
str(nl.year.ndvi)

# Which was the greenest municipality in 2014?
max.ndvi<-max(nl.year.ndvi$mean.ndvi.year)
municipality<-nl.year.ndvi[nl.year.ndvi$mean.ndvi.year==max.ndvi,1]
paste("The greenest municipality in 2014 was",municipality)