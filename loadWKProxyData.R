rm(list = ls())
###############
# Script Info #
###############
# PURPOSE: Load InterCatch exchange format data from WKLife SharePoint and convert to length frequencies.
# AUTHOR: Scott Large 2015 (Modified some code from Hans Gerritsen)
# REVIEWED BY:
# VERSION: 0.1
#

######################
# CHANGES/ ADDITIONS #
######################
# Need to add: 
# ~Rdata file of all folders, for off-server access to data
# ~Faroe Islands data is in the incorrect format, must remove for now #

# Done:

############
# PACKAGES #
############
# Required packages
needList <- NULL
new.packages <- needList[!(needList %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
#
library(reshape2)
#
######################
# Load any functions #
######################
# Modified (and corrected) from Hans' code for stock coordinators#
ReadIntercatch <- function(file){
  IC <- read.table(file, sep = ",", col.names = as.character(1:33), fill=T, header = F)
  IC[IC == "-9"]  <- NA 
  HI <- subset(IC, X1 == 'HI')[,1:12]
  names(HI) <- c("RecordType", "Country", "Year", "SeasonType", "Season", "Fleet",
                 "AreaType", "FishingArea", "DepthRange", "UnitEffort", "Effort",
                 "AreaQualifier")
  SI <- subset(IC,X1=='SI')[,1:24]
  names(SI) <- c("RecordType", "Country", "Year", "SeasonType", "Season", "Fleet",
                 "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory",
                 "ReportingCategory", "DataToFrom", "Usage", "SamplesOrigin", "Qualityflag",
                 "UnitCaton", "CATON", "OffLandings", "VarCaton", "InfoFleet",
                 "InfoStockCoordinator", "InfoGeneral")
  if(nrow(SI)>0){
    SI$UnitCaton <- tolower(SI$UnitCaton)
    if(sum(!SI$UnitCaton %in% c("t","kg"))>0)
      stop("Invalid UnitCaton: only 't' or 'kg' allowed")
    x <- ifelse(SI$UnitCaton == "kg", 0.001, 1)
    SI$CATON <- SI$CATON * x
    SI$UnitCaton <- "t"
  }
  SD <- subset(IC,X1=='SD')[,1:33]
  names(SD) <- c("RecordType", "Country", "Year", "SeasonType", "Season", "Fleet",
                 "AreaType", "FishingArea", "DepthRange", "Species", "Stock", "CatchCategory",
                 "ReportingCategory", "Sex", "CANUMtype", "AgeLength", "PlusGroup",
                 "SampledCatch", "NumSamplesLngt", "NumLngtMeas", "NumSamplesAge", "NumAgeMeas",
                 "unitMeanWeight", "unitCANUM", "UnitAgeOrLength", "UnitMeanLength", "Maturity",
                 "NumberCaught", "MeanWeight", "MeanLength", "VarNumLanded", "VarWeightLanded",
                 "VarLngthLanded")
  if(nrow(SD)>0){
    # Clean up some problems with factors and capitalization
    numCols <- c("MeanWeight", "NumberCaught", "AgeLength")
    SD[, numCols] <- apply(SD[,numCols], 2, function(x) as.numeric(as.character(x)))
    unitCols <- c("unitMeanWeight", "unitCANUM", "UnitAgeOrLength")
    SD[, unitCols] <- apply(SD[, unitCols], 2, function(x) tolower(x))
    SD[, unitCols] <- apply(SD[, unitCols], 2, function(x) gsub(x, pattern = " ", replacement = ""))
    #
    # Transfrom MeanWeight into kg
    if(sum(!SD$unitMeanWeight %in% c("g","kg")) > 0)
      stop("Invalid unitMeanWeight: only 'g' or 'kg' allowed")
    x <- ifelse(SD$unitMeanWeight == "g", 0.001, 1)
    SD$MeanWeight <- SD$MeanWeight * x
    SD$unitMeanWeight <- 'kg'
    #
    # Transfrom NumberCaught into k
    if(sum(!SD$unitCANUM %in% c("k","m","n")) > 0)
      stop("Invalid unitCANUM: only 'k', 'm' or 'n' allowed")
    x <- ifelse(SD$unitCANUM == "m", 0.0001, 
                ifelse(SD$unitCANUM == "n",
                       .001, 1))
    SD$NumberCaught <- SD$NumberCaught * x
    SD$unitCANUM <- 'k'
    #
    # Transfrom AgeLength into cm
    if(sum(!SD$UnitAgeOrLength %in% c("cm","mm")) > 0)
      stop("Invalid UnitAgeOrLength: only 'cm' or 'mm' allowed for this datacall")
    x <- ifelse(SD$UnitAgeOrLength == "mm", 0.1, 1)
    SD$AgeLength <- SD$AgeLength * x
    SD$unitMeanWeight <- 'cm'
  }
  return(list(HI,SI,SD))
}
#
#############
# Load data #
#############
#
# Note: SOME CODE REQUIRES ACCESS TO THE ICES SharePoint SERVER ###
#
dataDir <- "//community.ices.dk@SSL/DavWWWRoot/ExpertGroups/wklife/WKLIFE V/06. Data"
allData <- list.files(dataDir)
#
# Lists all files in the "06. Data" folder
# allData <- c("anb-78", "ang-ivvi", "anp-78", "arg-123a4", "arg-5b6a",
#              "arg-icel", "arg-rest", "bss-8ab", "had-iris", "lin-oth",
#              "meg-rock", "mgw-78", "nep-2021", "nep-2324", "nep-25", 
#              "nep-2627", "nep-2829", "nep-30", "nep-31", "ple-7h-k", 
#              "ple-celt", "ple-iris", "pol-celt", "sol-7h-k", "usk-oth",
#              "usk-rock", "whg-iris") 
#
# List all folders within the "06. Data" folder, presumably, these hold the data we're after
folders <- allData[!allData %in% grep(pattern = "\\.", x = allData, value = T) &
                   !allData %in% c("Data archive and documentation", "hom-soth", "ple-nsea", "sai-far", "sar-soth", "sol-nsea",
                                    "Extra Stocks", "lengthFrequencyData")]
foldersURL <- paste0("https://community.ices.dk/ExpertGroups/wklife/WKLIFE%20V/06.%20Data",
                     "/",
                     gsub(pattern = " ", replacement =  "%20", folders))
#
allList <- data.frame()
allCheck <- data.frame()
# i <- "usk-oth"
for(i in folders) { 
dataList <- list.files(paste0(dataDir, "/", i))
NoNoList <- c(grep("\\.pdf", x = dataList, value = T),
              grep("UNRAISED DISCARDS", x = dataList, value = T),
              grep("\\.xls", x = dataList, value = T),
              grep("archive", x = dataList, value = T),
              grep("\\.doc", x = dataList, value = T),
              grep("\\.zip", x = dataList, value = T))
checkList <- c(grep("\\.xls", x = dataList, value = T),
               grep("\\.xlsx", x = dataList, value = T))
checkList <- checkList[!checkList %in% c(grep("OtherSupporting", x = checkList, value = T),
                                         grep("UNRAISED DISCARDS", x = checkList, value = T),
                                         grep("Annex3", x = checkList, value = T))]
if(length(checkList) >= 1) {
  checkCheck <- dataList[dataList %in% checkList]
  checkList <-  data.frame(URLS = paste0("https://community.ices.dk/ExpertGroups/wklife/WKLIFE%20V/06.%20Data/",
                                         gsub(pattern = " ", replacement =  "%20", i), "/", checkCheck),
                           STOCK = i)
  allCheck <- rbind(checkList, allCheck)
}
dataList <- dataList[!dataList %in% c(NoNoList, checkList)]
files <- data.frame(URLS = paste0("https://community.ices.dk/ExpertGroups/wklife/WKLIFE%20V/06.%20Data/",
                                  gsub(pattern = " ", replacement =  "%20", i),
                                  "/", dataList),
                    STOCK = i)
allList <- rbind(files, allList)
}
#
######################################################################################
# Loop to run through all folders on WKLife/Data and download InterCatch format data #
######################################################################################
#
for(j in unique(allList$STOCK)) {
  files <- as.character(allList$URLS[allList$STOCK == j])
  #
  # Faroe Islands data is in the incorrect format, must remove for now #
  faroeFiles <- files[files %in% grep("FaroeIslands", x = files, value = T )]
  files <- files[!files %in% faroeFiles]
  #
  HI <- SI <- SD <- NULL
  for(file in files) {
    tmpFile <- tempfile()
    download.file(file, destfile = tmpFile, mode = "wb", quiet = F)
    if(file.info(tmpFile)$size == 0) { 
      next(paste0(file, " does not seem to have any data.\n"))
    }
    ic <- ReadIntercatch(tmpFile)
    HI <- rbind(HI,ic[[1]])
    SI <- rbind(SI,ic[[2]])
    SD <- rbind(SD,ic[[3]])
  }
  #
  if(nrow(SD) < 1)
    next(paste0(file, " has no valid SD data/n"))
    
  # Aggregate the data according to AgeLength and Year #
  # Could also look at area, country, and discards/landings #
  lens <- with(SD,
                  aggregate(list(NumberCaught = NumberCaught),
                                 list(
                                   Year = Year,
#                                     unitCANUM = unitCANUM,
#                                     FishingArea = FishingArea,
#                                     Country=Country,
                                    Length = AgeLength),
                                    sum))
  #
  # Transform the matrix into a more user-friendly output
  lens <- dcast(lens, Year~Length, value.var = "NumberCaught")
  lens$Year <- paste0("X", lens$Year)
  lenFreq <- data.frame(matrix(NA, nrow = ncol(lens) - 1, ncol = length(lens$Year) + 1))
  colnames(lenFreq) <- c("LENGTH", lens$Year)
  lenFreq$LENGTH <- as.numeric(colnames(lens)[-1])  
  lenFreq[,-1] <- as.numeric(t(lens)[-1,])
  interCatch <- list(HI, SI, SD)
  #
  # Save the:
  # ~ lenFreq = length frequency data
  # ~ interCatch = list with HI, SI, and SD information
  # ~ files = list of URLs from SharePoint where data was downloaded from
  save(lenFreq, interCatch, files, file = paste0("~/", j, "_allData.RDAT"))
  write.csv(lenFreq, file = paste0("~/", j, "_lenFreq.csv"), row.names = F)
  #
}

