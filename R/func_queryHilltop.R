func_queryHilltop <- function(htsPath, hprPath, collectName, startTime="0", endTime="1", projectCodeQuery=""){

require(Hilltop)
require(tidyverse)

zooTidy <- function(zoo){
  zoo_dbl <- as.double(zoo)
  zoo_date <- attr(zoo, "index")
  zoo_measurement <- attr(zoo, "Measurement")
  return.tbl <- tibble(Date=zoo_date, Measurement=zoo_dbl)
  colnames(return.tbl) <- c("Date", zoo_measurement)
  return(return.tbl)
}

HTfile <- HilltopData(htsPath)

Collection <- GetCollection(HTfile, hprPath, collectName)
sList <- unique(Collection[1])
mList <- unique(Collection[2])

siteData <- tibble()
for(i in 1:length(sList[,1])){
  sInfo <- SiteInfo(HTfile, sList[i,])
  print(paste0("Loading site ",i, " of ", length(sList[,1])," - ",sList[i,]))
  sData <- GetData(HTfile, sList[i,], mList[,1], startTime, endTime, ParamFilter = projectCodeQuery)
  # print(paste0("Loaded ",length(sData), " entries from site ",sList[i,]))
  
  if(length(sData)==0){
    print(paste0("Warning: Site ", sList[i,]," returned no data for time range supplied."))
  } else {  
  sData <- sData %>%
    map(.,gsub, pattern = "<", replacement = "") %>%
    map(.,gsub, pattern = ">", replacement = "") %>%
    map(.,zooTidy) %>%
    reduce(full_join, by="Date")

  sData$Site <- sList[i,]
  sData$Easting <- as.double(sInfo["Easting"])
  sData$Northing <- as.double(sInfo["Northing"])
  
  siteData <- bind_rows(siteData,sData)
  }
}
print("Loading sites complete")
disconnect(HTfile)

return(siteData)
}

