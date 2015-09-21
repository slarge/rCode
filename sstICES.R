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
# ecoName <- "ices_ecoregions"
#
# ICES statistical area and rectangles data.frame
load(paste0(homeFolder,"rectAreaICES.Rdat"))
#
# Read in the shapefiles
areaData <- readOGR(dsn = localDir, layer = icesName)
recData <- readOGR(dsn = localDir, layer = recName)
# ecoData <- readOGR(dsn = localDir, layer = ecoName)
#
# Establish the CRS, and make sure area areaData and recData are the same
areaCRS <- proj4string(areaData)
recCRS <- proj4string(recData)
# ecoCRS <- proj4string(ecoData)
# if(areaCRS != recCRS) stop("Projections are not the same for ICES Areas and rectangles")
#
# right now they should be the same so we will just use the areaCRS
areaData <- spTransform(areaData, CRS = CRS(proj4string(areaData)))
recData <- spTransform(recData, CRS = CRS(proj4string(areaData)))
# ecoData <- spTransform(ecoData, CRS = CRS(proj4string(areaData)))
#
# Select the areas of interest
areaList <- c("VIId", "IVa", "IVb", "IVc", "IIIa")
years <- 1960:2014

sstICES <- function(areaList, years) {
#
  if(all(!areaList %in% as.character(areaData@data$ICES_area) == T)) {
    stop("Make sure the area is a valid area")
  }
#
  allName <- areaName[areaName$ICES_NAME %in% areaList,]
#   allName <- cbind(allName, ices.rect(allName$ICES_AREA))
  rectList <- unique(allName$ICES_AREA)
#
# Load 1x1 grid over areaList
# load("~/git/rInProgress/gridArea_v01.rdat")
#
getArea <- areaData[areaData$ICES_area %in% areaList,]
getRect <- recData[recData$ICESNAME %in% rectList,]
#
############
# Start a loop to download the netCDF for each month in each year
############
#
sstAreaMonth <- data.frame(matrix(ncol = 14,
                                  nrow = length(years), NA))
names(sstAreaMonth) <- c("YEAR", month.name[1:12], "MEAN")
#
rectBBox <- bbox(getRect)
# expand points to grid
grd <- expand.grid(x = seq(rectBBox[1,1], rectBBox[1,2]),
                   y = seq(rectBBox[2,1], rectBBox[2,2]))
coordinates(grd) <- ~x + y
gridded(grd) <- TRUE
grd <- as(grd, "SpatialPolygons")
proj4string(grd) <- proj4string(getArea)
#
gridArea <- gIntersection(grd, getArea, byid = T, drop_lower_td = T)
# save(gridArea, file = "~/git/rInProgress/gridArea_v01.rdat")
#
# Establishes the grid of interpolated values to select from
gridArea <- as(gridArea, "SpatialPolygonsDataFrame")
names(gridArea) <- "sst"
#
for(year.j in 1:length(years)) {
  for(month.i in 1:12) {
    #
    # Open the NetCDF file
    urls <- paste0("http://www1.ncdc.noaa.gov/pub/data/cmb/ersst/v4/netcdf/ersst.v4.",
                   years[year.j],
                   sprintf("%02d", month.i),
                   ".nc")
    tmpFile <- tempfile(fileext = ".nc")
    download.file(urls, destfile = tmpFile, mode = "wb", quiet = T)
    #
    SST1 <- raster(tmpFile, varname = "sst", layer = 1)
    names(SST1) <- "sst"
    SST1 <- shift(SST1, x = 1)
    SST1 <- rotate(SST1)
    SST1 <- shift(SST1, x = -1)
    SST1 <- crop(SST1, extent(getRect))
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
    #
    # Interpolate over the grid
    interpSST <- idw(formula = sst~1, locations = sstData,
                     newdata = grd)  # apply idw model for the data
    #
    interpArea <- over(gridArea, interpSST)
    names(interpArea)[3] <- "sst"
    #
    
    sstAreaMonth[year.j, 1 + month.i] <- mean(interpArea$sst, na.rm = T)
    cat(paste0(month.name[month.i], " ", years[year.j], " \n"))
  } # close year.j loop
  sstAreaMonth[year.j, 1] <- years[year.j]
} # close month.i loop

sstAreaMonth$MEAN <- rowMeans(sstAreaMonth[,-c(1,14)])
return(sstAreaMonth)
}

codDat <- sstICES(areaList = c("VIId", "IVa",  "IVb",  "IVc",  "IIIa"),
                  years = 1960:2014)
write.csv(codDat, file = "codSST.csv", row.names = F)
pleDat <- sstICES(areaList = c("IVa",  "IVb",  "IVc",  "IIIa"),
                  years = 1960:2014)
write.csv(pleDat, file = "pleSST.csv", row.names = F)

#
sstArea <- interpArea$sst
gridArea@data$sst <- sstArea
#
# getRect@data$id <- rownames(getRect@data)
# rect.df <- fortify(getRect, region = "id")
# #
# getArea@data$id <- rownames(getArea@data)
# area.df <- fortify(getArea, region = "id")
# #
gridArea@data$id <- rownames(gridArea@data)
sst.df <- fortify(gridArea, region = "id")
sst.df <- merge(sst.df, gridArea@data, by="id")
#
# fullMap <- getMap(resolution = "low")  # different resolutions available
# fullMap <- spTransform(fullMap, CRS = CRS(areaCRS))
# fullMap@data$id <- rownames(fullMap@data)
# fullMap <- crop(fullMap, extent(getRect))
# map.df <- fortify(fullMap, region = "id")
#
ggplot(sst.df, aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(fill = sst), alpha = 1)+
  scale_fill_gradientn(colours = rev(rainbow(10)), na.value = NA) +
  labs(x="",y="") +
  coord_fixed() +
  theme_bw() +
  theme(legend.position = "bottom")
