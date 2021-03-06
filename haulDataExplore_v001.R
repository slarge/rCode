rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: Swept Area calculations for NS-IBTS from DATRAS
# AUTHOR: Scott Large 2015
# REVIEWED BY:
# vERSION: 0.1
#

######################
# CHANGES/ ADDITIONS #
######################
# Need to add:


# Done:

############
# PACKAGES #
############
#
# library(devtools)
# install_github("slarge/ices")
# library(ices)
library(ggplot2)
library(data.table)
library(reshape2)
library(arm)
library(car)
#

######################
# Load any functions #
######################
#
# Calculate distance in kilometers between two points
earth.dist <- function (long1, lat1, long2, lat2) {
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6371 # Mean radius (km) of the earth
  d <- R * c
  return(d)
}
#
#############
# Load data #
#############

#
# Number of bootstrap replicates
nb <- 1000
#
# Confidence interval range
CV <- .95
#
# Set seed for reproducable results
setSeed <- set.seed(627)
#
#
load("~/HH-26012015_NS-IBTS.Rdat")
#
###################################
# Clean up raw data from getHHfun #
###################################
#
# Remove extra columns
if(any(colnames(HH) == "V1")) HH[, ":=" (V1 = NULL)]
#
# Remove extra spaces from character strings
cnames <- colnames(HH)
for(cname in cnames) {
  set(HH, j = cname, value = gsub("[[:space:]]", "", HH[[cname]]))
}
#
# Change appropriate to numeric
numCols <- c("Quarter", "SweepLngt", "HaulNo", "Year", "month",
             "Day", "TimeShot", "Stratum", "HaulDur", "ShootLat",
             "ShootLong", "HaulLat", "HaulLong", "Depth", "StdSpecRecCode",
             "Netopening", "Rigging", "Tickler", "Distance", "Warplngt",
             "Warpdia", "DoorSurface", "DoorWgt", "DoorSpread", "WingSpread",
             "Buoyancy", "KiteDim", "WgtGroundRope", "TowDir", "GroundSpeed",
             "SpeedWater", "SurCurDir", "SurCurSpeed", "BotCurDir",  "BotCurSpeed",
             "WindDir", "WindSpeed",  "SwellDir",  "SwellHeight",  "SurTemp",
             "BotTemp", "SurSal", "BotSal")
HH[, (numCols) := lapply(.SD, as.numeric), .SDcols = numCols]
#
# Change -9 values to NA
for(cname in cnames){
  set(HH, i = which(HH[[cname]] == -9), j = cname, value = NA)
}
#
# Select only valid hauls during the day and select important columns
hauls <- HH[HH$HaulVal=='V' & DayNight == "D", ]
keepers <- c("Quarter", "StatRec", "Depth", "HaulDur", "Distance",
             "GroundSpeed", "Ship", "Year", "WingSpread", "HaulLat",
             "HaulLong", "ShootLat", "ShootLong")
others <- colnames(hauls)[!colnames(hauls) %in% keepers]
hauls[, c(others):= NULL]
#
#Convert from nautical miles/hour to meters/minute
hauls[, GroundSpeed := GroundSpeed * 1852 / 60]
#
###################
# DISTANCE HAULED #
###################
#
## Calculate haversine distance with shoot and haul coordinates ##
hauls[, LatLongDistance := earth.dist(long1 = ShootLong,
                                      lat1 = ShootLat,
                                      long2 = HaulLong,
                                      lat2 = HaulLat) * 1000]
#
## RAW DISTANCE ##
hauls[!is.na(Distance), c("newDist", "qualityDistance") :=
        list(Distance, "rawDistance")]
#
## HAVERSINE DISTANCE ##
# if haul and shoot coordinates are the same (i.e., equal to zero) then leave as NA
hauls[is.na(Distance) & !is.na(LatLongDistance) & LatLongDistance > 1,
      c("newDist", "qualityDistance") :=
        list(LatLongDistance, "LatLongDistance")]
#
## DURATION X SPEED ##
# HaulDur x GroundSpeed
hauls[!is.na(GroundSpeed) & is.na(newDist),
      c("newDist", "qualityDistance") :=
        list(GroundSpeed * HaulDur, "SpeedHaulDur")]
#
# Hauls that don't have raw distance, shoot/haul coordinates, or GroundSpeed can be estimated
# using linear models to predict GroundSpeed. Two different types of missing data are found:
# 1) ships that have no GroundSpeed records and we need to estimate from other ships, and
# 2) ships that have only a few missing GroundSpeed records and we can use ship as a factor in the lm
#
needSpeed <- hauls[is.na(newDist) & is.na(GroundSpeed) & !is.na(HaulDur),]
needSpeedShip <- unique(needSpeed$Ship)
withSpeedShip <- unique(hauls$Ship[!is.na(hauls$GroundSpeed)])
#
#
# ID ships that have some missing GroundSpeed records
canSpeedShip <- needSpeedShip[needSpeedShip %in% withSpeedShip]
canSpeed <- hauls[Ship %in% canSpeedShip, ]
#
# Split the data into the two types: only a few missing and completely missing
canSpeedNA <- canSpeed[is.na(GroundSpeed) & is.na(newDist),]
canSpeedOK <- canSpeed[!is.na(GroundSpeed),]
#
cs1 <- lm(GroundSpeed ~ Ship + Quarter + Year + Depth, canSpeedOK)
lmShipSpeedHaulDurLen <- nrow(canSpeedNA)
lmShipSpeedHaulDurBoot <- matrix(bootCase(cs1, function(x) predict(x, canSpeedNA) * canSpeedNA$HaulDur,
                                          B = nb),
                                 nb,
                                 lmShipSpeedHaulDurLen)
#
hauls[Ship %in% canSpeedShip & is.na(GroundSpeed) & is.na(newDist),
      c("newDist", "newDistCIlower", "newDistCIupper", "qualityDistance") :=
        list(apply(lmShipSpeedHaulDurBoot, 2, median),
             apply(lmShipSpeedHaulDurBoot, 2, quantile, (1-CV)/2, na.rm = T),
             apply(lmShipSpeedHaulDurBoot, 2, quantile, (1+CV)/2, na.rm = T),
             "lmShipSpeedHaulDur")]
##
stillNeedSpeed <- hauls[is.na(newDist),]
cs2 <- lm(GroundSpeed ~ Quarter + Year, canSpeedOK)
lmQYearSpeedHaulDurLen <- nrow(stillNeedSpeed)
lmQYearSpeedHaulDurBoot <- matrix(bootCase(cs2, function(x) predict(x, stillNeedSpeed) * stillNeedSpeed$HaulDur,
                                          B = nb),
                                 nb,
                                 lmQYearSpeedHaulDurLen)
#
hauls[is.na(newDist),
      c("newDist", "newDistCIlower", "newDistCIupper", "qualityDistance") :=
        list(apply(lmQYearSpeedHaulDurBoot, 2, median),
             apply(lmQYearSpeedHaulDurBoot, 2, quantile, (1-CV)/2, na.rm = T),
             apply(lmQYearSpeedHaulDurBoot, 2, quantile, (1+CV)/2, na.rm = T),
             "lmQYearSpeedHaulDur")]
#
hauls[,
      c("newDist", "newDistCIlower", "newDistCIupper") :=
        list(newDist / 1000,
             newDistCIlower / 1000,
             newDistCIupper / 1000)]

#
######################
# WING & DOOR SPREAD #
######################
#
# Linear model: WingSpread ~ a * log(Depth)) + b
lmWingForm <- lm(WingSpread ~ I(log(Depth)), hauls)
lmWingDat <- hauls[is.na(WingSpread) & !is.na(Depth),]
lmWingDatLen <- nrow(lmWingDat)
lmWingBoot <- matrix(bootCase(lmWingForm, function(x) predict(x, lmWingDat), B = nb), nb, lmWingDatLen)
#
## RAW WINGSPREAD ##
hauls[!is.na(WingSpread), c("newWingSpread", "qualityWingSpread") :=
        list(WingSpread, "rawWingSpread"),]
#
## LINEAR MODEL WINGSPREAD ##
hauls[is.na(WingSpread) & !is.na(Depth),
      c("newWingSpread","newWingSpreadCIlower", "newWingSpreadCIupper", "qualityWingSpread") :=
        list(apply(lmWingBoot, 2, median),
             apply(lmWingBoot, 2, quantile, (1-CV)/2, na.rm = T),
             apply(lmWingBoot, 2, quantile, (1+CV)/2, na.rm = T),
             "lmWingSpread")]
#
## MEDIAN WINGSPREAD ##
#
rawWingMat <- hauls$WingSpread[!is.na(hauls$WingSpread)]
hauls[is.na(newWingSpread), c("newWingSpread","newWingSpreadCIlower", "newWingSpreadCIupper", "qualityWingSpread") :=
        list(median(rawWingMat),
             quantile(replicate(nb, median(sample(rawWingMat, rep = TRUE))),  (1-CV)/2),
             quantile(replicate(nb, median(sample(rawWingMat, rep = TRUE))),  (1+CV)/2),
             "medianWingSpread")]

hauls[,
      c("newWingSpread", "newWingSpreadCIlower", "newWingSpreadCIupper") :=
        list(newWingSpread / 1000,
             newWingSpreadCIupper / 1000,
             newWingSpreadCIlower / 1000)]

##############
# SWEPT AREA #
##############
#
hauls[, c("newSweptArea", "CIupper", "CIlower", "qualityCode") := list(newWingSpread * newDist,
                                                                       newWingSpreadCIupper * newDistCIupper,
                                                                       newWingSpreadCIlower * newDistCIlower,
                                                                       paste0(qualityDistance, qualityWingSpread)),]


# save(hauls, file = "~/Data Products/cleanHH_v001.rdat")
# Explore data graphically
ggplot(hauls, aes(x = Year, y = newSweptArea)) +
   geom_jitter(alpha=I(1/4), aes(color = qualityCode)) +
   theme_bw()
