rm(list = ls())
# Required libraries
library(ncdf4)
library(classInt)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(raster)
library(mapplots)
library(ggplot2)
library(rworldmap)
library(gstat)
#
#######################
# Load ICES area maps #
#######################
#
homeFolder <- "D:/Profiles/Scott/My Documents/rCode/data/"
localDir <- paste0(homeFolder, 'mapData')
#
# Unzipped shapefile without file type extensions
icesName <- "ices_areas"
recName <- "ices_rectangles"
ecoName <- "ices_ecoregions"
#
# ICES statistical area and rectangles data.frame
load(paste0(homeFolder,"rectAreaICES.Rdat"))
#
# Read in the shapefiles
areaData <- readOGR(dsn = localDir, layer = icesName)
recData <- readOGR(dsn = localDir, layer = recName)
ecoData <- readOGR(dsn = localDir, layer = ecoName)
#
# Establish the CRS, and make sure area areaData and recData are the same
areaCRS <- proj4string(areaData)
recCRS <- proj4string(recData)
ecoCRS <- proj4string(ecoData)
# if(areaCRS != recCRS) stop("Projections are not the same for ICES Areas and rectangles")
#
# right now they should be the same so we will just use the areaCRS
areaData <- spTransform(areaData, CRS = CRS(areaCRS))
recData <- spTransform(recData, CRS = CRS(areaCRS))
ecoData <- spTransform(ecoData, CRS = CRS(areaCRS))
#
# Select the areas of interest
areaList <- c("VIId", "IVa", "IVb", "IVc", "IIIa")
allName <- areaName[areaName$ICES_NAME %in% areaList,]
allName <- cbind(allName, ices.rect(allName$ICES_AREA))
rectList <- unique(allName$ICES_AREA)
#
getArea <- areaData[areaData$ICES_area %in% areaList,]
getRect <- recData[recData$ICESNAME %in% rectList,]
#
# Open the NetCDF file
urls <- "http://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v4/netcdf/ersst.v4.201311.nc"
tmpFile <- tempfile(fileext = ".nc")
download.file(urls, destfile = tmpFile, mode = "wb")
#
SST1 <- raster(tmpFile, varname = "sst", layer = 1)
names(SST1) <- "sst"
SST1 <- shift(SST1, x = 1)
SST1 <- rotate(SST1)
SST1 <- shift(SST1, x = -1)
SST1 <- crop(SST1, extent(getRect))
#
rectBBox <- bbox(getRect)
# 
sstData <- rasterToPolygons(SST1)
sstData <- spTransform(sstData, CRS = CRS(areaCRS))
#
coordSST <- data.frame(coordinates(sstData))
colnames(coordSST) <- c("x", "y")
coordinates(coordSST) <- ~x + y 
gridded(coordSST) <- TRUE 
proj4string(coordSST) <- proj4string(areaData)
# 
coordSST <- as(coordSST, "SpatialPolygons")
coordSST$sst <- sstData@data$sst
# 
plot(coordSST)

# coordSST$ICESNAME <- ices.rect2(lon = coordSST$lon, lat = coordSST$lat)
# coordSST$sst <- sstData@data$sst
# sstData@data$ICESNAME <- coordSST$ICESNAME
#
# library(plyr)
# coordRect <- data.frame(ICESNAME = getRect@data$ICESNAME)
# sstRect <- merge(coordSST, coordRect, by = "ICESNAME", all.y = T, all.x = F)
# getRect@data$sst <- sstRect$sst
# 
# getRect@data$id <- rownames(getRect@data)
# getRect@data <- join(getRect@data, sstRect, by = "ICESNAME")
# rect.df <- fortify(getRect, region = "id")
# rect.df <- join(rect.df, getRect@data, by = "id")
#
# plot(getRect[getRect$ICESNAME %in% rectList,])
# plot(getArea, add = T)
# 
# getSST <- sstData[sstData$ICESNAME %in% rectList,]
# getSST <- sstData
# plot(sstData)
# plot(getSST, add = T, col = "red")
# # plot(getRect)
# plot(getRect, add = T)
# plot(getArea, add = T)
# 
#####
# 
# expand points to grid
grd <- expand.grid(x = seq(rectBBox[1,1], rectBBox[1,2]),
                   y = seq(rectBBox[2,1], rectBBox[2,2]))
coordinates(grd) <- ~x + y
proj4string(grd) <- proj4string(getArea)
gridded(grd) <- TRUE
grd <- as(grd, "SpatialPolygons")
#
gridArea <- gIntersection(grd, getArea, byid = T, drop_lower_td = T)
# save(gridArea, file = "~/git/rInProgress/gridArea_v01.rdat")
#
load("~/git/rInProgress/gridArea_v01.rdat")
#
# plot(grd, cex = 1.5, col = "grey")
# points(coordSST)
# testSST <- coordSST[,c("lon", "lat", "sst")]
# names(testSST)[1:2] <- c("x", "y")

# Establishes the grid of interpolated values to select from
# gridArea <- tt
gridArea <- as(gridArea, "SpatialPolygonsDataFrame")
#
# Interpolate over the grid
interpSST <- idw(formula = sst~1, locations = sstData, 
                 newdata = grd)  # apply idw model for the data
# proj4string(idw) <- proj4string(areaData)
# interpSST <- as(interpSST, "SpatialPointsDataFrame")
# names(interpSST)<- c("lon", "lat")

names(gridArea) <- "sst"
interpArea <- over(gridArea, interpSST)
names(interpArea)[3] <- "sst"
#
sstArea <- interpArea$sst
gridArea@data$sst <- sstArea
#


fullMap <- getMap(resolution = "low")  # different resolutions available
fullMap <- spTransform(fullMap, CRS = CRS(areaCRS))
#
#

getRect@data$id <- rownames(getRect@data)
rect.df <- fortify(getRect, region = "id")
#
getArea@data$id <- rownames(getArea@data)
area.df <- fortify(getArea, region = "id")
#
gridArea@data$id <- rownames(gridArea@data)
sst.df <- fortify(gridArea, region = "id")
sst.df <- merge(sst.df, gridArea@data, by="id")
#
getSST@data$id <- rownames(getSST@data)
sst.df <- fortify(getSST, region = "id")
sst.df <- merge(sst.df, getSST@data, by="id")
#
fullMap@data$id <- rownames(fullMap@data)
fullMap <- crop(fullMap, extent(getRect))
map.df <- fortify(fullMap, region = "id")
#
ggplot(sst.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = sst), alpha = 1)+
  scale_fill_gradientn(colours = rev(rainbow(10)), na.value = NA) +
  labs(x="",y="") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "bottom")
