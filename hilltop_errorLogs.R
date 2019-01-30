library(tidyverse)
library(lubridate)
library(hillr)

# library(Hilltop)
# source("R/func_queryHilltop.R")
# measurements <- getHilltopMeasurements(endpointMdc, site = sitesWebserver[3,1])


endpointMdc <- "http://hydro.marlborough.govt.nz/mdc.hts?"
sites <- getHilltopSites(endpointMdc)

timeStart_measurementQuery <- now()
siteMeasurements <- tibble(siteName=sites[,1]) %>%
  mutate(measurementsList = map(.x = siteName, .f = possibly(getHilltopMeasurements, otherwise = NULL), endpoint = endpointMdc)) 
timeFinish_measurementQuery <- now()
timeDiff_measurementQuery <- timeFinish_measurementQuery - timeStart_measurementQuery


## Which sites return Nulls from webserver measurements list? Check measurement xml on these
## Which sites return mixed types of data for DataMeasurement using this code? Probably neater to define tibble typeOf on import
siteMeasurements_errors <- siteMeasurements %>% #Generate a list of site with some errors associated for log file reporting
  mutate(isListNullLgl = map_lgl(.$measurementsList, is.null)) %>% 
  mutate(defMeasIsNotChrLgl = map_lgl(map(.$measurementsList, pluck, "DefaultMeasurement"), negate(is.character))) %>% 
  filter(isListNullLgl == TRUE | defMeasIsNotChrLgl == TRUE)

#Code to flag DefaultMeasurement returned as numeric, rather than character, such that data can be nested by site
siteMeasurementsTidy <- siteMeasurements %>% 
  mutate(isListNullLgl = map_lgl(.$measurementsList, is.null)) %>% 
  mutate(defMeasIsNotChrLgl = map_lgl(map(.$measurementsList, pluck, "DefaultMeasurement"), negate(is.character))) %>% 
  filter(isListNullLgl == FALSE & defMeasIsNotChrLgl == FALSE) %>% 
  unnest() %>% 
  select(-isListNullLgl, -defMeasIsNotChrLgl) 

## Which sites contain "Item2"? "Item2" believed to signify presence of metadata. Find and remove to query with Measurement Name on remainder. Filter From != To to remove single data entry values
siteItem2 <- siteMeasurementsTidy %>% 
  filter(From != To & DataType == "WQData") %>% 
  filter(MeasurementName == "Item2") %>% 
  # filter(RequestAs != "Item2 [Faecal Coliforms]") %>%
  select(siteName, RequestAs, From, To) #%>% 
  # group_by(siteName) %>%
  # nest

# sitesObservations <- sitesItem2 %>% 
#   filter(siteName == "Are Are Creek at Kaituna Tuamarina Track" | siteName == "Black Birch Stream at Awatere Intake")

# sitesObservations <- sitesItem2 %>% 
#   filter(siteName == "Are Are Creek at Kaituna Tuamarina Track" | siteName == "Black Birch Stream at Awatere Intake") %>% 
#   mutate(from = map_chr(map(.$data, pluck, "From"), min)) %>% 
#   mutate(to = map_chr(map(.$data, pluck, "To"), max)) %>% 
#   mutate(measurementRequest = map(map(.x = .$data, "RequestAs"), `[`)) %>% 
#   select(-data)


timeStart <- now()

siteObservations <- siteItem2 %>% 
  slice(1934:1998) %>% # comment out when done testing
  mutate(data = pmap(.l = lst(endpoint = endpointMdc, site = siteName, measurement = RequestAs, from = From, to = To), 
                     .f = getHilltopData))
  
test <- siteObservations %>% 
  # select(data)
  map(.$data, {~map_at("Measurement", as.character)})
  

timeFinish <- now()

### This works, don't delete
# timeStart <- now()
# sitesObservations <- sitesItem2 %>% 
#   # slice(1:100) %>%
#   mutate(reply = map2(.x = siteName, 
#                       .y = data,
#                       .f = safely(~fullGetHilltopData(endpoint = endpointMdc,
#                                                       sites = .x,
#                                                       measurements = .y$RequestAs,
#                                                       from = min(.y$From),
#                                                       to = max(.y$To)),
#                                   otherwise = NA, 
#                                   quiet = FALSE)
#   ))
# timeFinish <- now()


siteObs <- sitesObservations %>% 
  mutate(errorLgl = map_lgl(map(reply, "result"), negate(is.list))) %>% 
  mutate(errorMessage = map_chr(map(reply, "message"), pluck))
                      
  
map_dbl(sitesObservations$data[[1]], "RequestAs")

  mutate(test = map2(siteName, map_chr(data, ~map(data, "RequestAs")), ~paste(.x, .y, sep = " ")))
  
  mutate(test = map(siteName, as.character))
  mutate(test = map(data, ~map(data, "RequestAs")))
  
  mutate(values = map(.x = siteName, 
                      # .y = map_chr(data, ~map(data, "RequestAs")),
                      # .f = ~fullGetHilltopData(endpoint = endpointMdc, sites = .x, measurements = .y)
                      .f = print
                      ))
    
    
    RequestAs = map(data, ~map(data, "RequestAs"))) # close, but selection not working correctly


sitesObservations_test <- sitesObservations %>%
  mutate(RequestAs = map(data, ~map(data, "RequestAs"))) # Nearly WORKING!!! :):):)
  
  ~map_chr("siteName")

 sitesObservations %>% 
   map(siteName, print)
  


map(sitesObservations$data, "RequestAs") # this works

  
  map_df(.x = map_chr("siteName"), fullGetHilltopData, endpoint = endpointMdc, sites = .x, measurements = map_chr("measurementRequest"), from = map_chr("from"), to = map_chr("to"))



test <- fullGetHilltopData(endpointMdc, 
                           site = sitesObservations$siteName, 
                           measurements = map_chr(sitesObservations$measurementRequest)
                               

## Which sites contain pH measurements?


# Log file generation and recording ---------------------------------------


dateToday <- today() %>% {paste0(year(.), str_pad(month(.),width=2, side="left", pad="0"),str_pad(day(.),width=2, side="left", pad="0"))}
logFilepath <- paste0("output/logWebserverQuery_", dateToday, ".csv")



for(i in 5:6){
  measurments <- getHilltopMeasurements(endpointMdc, site=sites[i,1])
  measurementsFilter <- measurments %>% 
    filter(MeasurementName != "Item2")
  
  if(exists("dataHT")==TRUE){rm(dataHT)}
  dataHT <- possibly(fullGetHilltopData(endpoint = endpointMdc, 
                                        sites = sites[i,1], 
                                        measurements = measurementsFilter$MeasurementName,
                                        from = min(measurementsFilter$From), 
                                        to= max(measurementsFilter$To)),
                     otherwise = NULL)
  
  if(is.null(dataHT)==TRUE){
    writeString <- tibble(siteName=sites[i,1], errorCode=as.integer(1), Message="No dataframe built from Webserver query")
    write_csv(writeString, logFilepath, append = TRUE)
  } else {
    writeString <- tibble(siteName=sites[i,1], errorCode=as.integer(0), Message="All OK")
    write_csv(writeString, logFilepath, append = TRUE)
  }
  #do further time checks for duplicates ending in append to log file
  
}





##
# 
# hilltopDataPath <- "data/SoE_DataWQI_2007-2017.hts"
# # hilltopDataPath <- "//hydro2/Hilltop/Data/MDC Data.hts"
# hilltopProjectPath <- "data/General_Project.hpr"
# collection <- "Data for WQI - all sites"
# safeStartTime <- "15/3/2013"
# safeEndTime <- "1"
# ParameterFiltering <- "Project = SW SoE"
# 
# siteData_test <- func_queryHilltop(hilltopDataPath, hilltopProjectPath, collection, startTime = safeStartTime, endTime = safeEndTime, projectCodeQuery = ParameterFiltering)

# htObj <- HilltopData(hilltopDataPath)
# sites <- SiteList(htObj)
# sitesInfo <- SiteInfo(htObj, sites[1])
# View(sitesInfo)
# disconnect(htObj)
