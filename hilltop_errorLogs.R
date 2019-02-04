library(tidyverse)
library(lubridate)
library(hillr)

endpointMdc <- "http://hydro.marlborough.govt.nz/mdc.hts?"
sites <- getHilltopSites(endpointMdc)

siteMeasurements <- tibble(siteName=sites[,1]) %>%
  mutate(measurementsList = map(.x = siteName, .f = possibly(getHilltopMeasurements, otherwise = NULL), endpoint = endpointMdc)) 

## Which sites return Nulls from webserver measurements list? Check measurement xml on these
## Which sites return mixed types of data for DataMeasurement using this code? Probably neater to define tibble typeOf on import
siteMeasurements_errors <- siteMeasurements %>% #Generate a list of site with some errors associated for log file reporting
  mutate(isListNullLgl = map_lgl(.$measurementsList, is.null)) %>% 
  mutate(defMeasIsNotChrLgl = map_lgl(map(.$measurementsList, pluck, "DefaultMeasurement"), negate(is.character))) %>% 
  filter(isListNullLgl == TRUE | defMeasIsNotChrLgl == TRUE)

# Code to flag DefaultMeasurement returned as numeric, rather than character, such that data can be nested by site
siteMeasurementsTidy <- siteMeasurements %>% 
  mutate(isListNullLgl = map_lgl(.$measurementsList, is.null)) %>% 
  mutate(defMeasIsNotChrLgl = map_lgl(map(.$measurementsList, pluck, "DefaultMeasurement"), negate(is.character))) %>% 
  filter(isListNullLgl == FALSE & defMeasIsNotChrLgl == FALSE) %>% 
  unnest() %>% 
  select(-isListNullLgl, -defMeasIsNotChrLgl) 

## Which sites contain "Item2"? "Item2" believed to signify presence of metadata. Find and remove to query with Measurement Name on remainder. Filter From != To to remove single data entry values
siteItem2 <- siteMeasurementsTidy %>% 
  filter(From != To & DataType == "WQData" & RequestAs != "Item2") %>% 
  filter(MeasurementName == "Item2") %>% 
  select(siteName, RequestAs, From, To) 

## Following section gets site and measurement info singly from Hilltop (using getHilltopData) - singly as fullGetHilltop does not check for typeof consistency between returned values, therefore collapses when building dataframe.

siteObservations <- siteItem2 %>% 
  # slice(1934:1998) %>% # 1934:1998 comment out when done testing
  mutate(return = pmap(.l = lst(endpoint = endpointMdc, site = siteName, measurement = RequestAs, from = From, to = To), 
                     .f = safely(getHilltopData, otherwise = NA)),
         data = map(.x = return, "result"),
         error = map(.x = return, "error"))

siteObservations_clean <- siteObservations %>% 
  mutate(data = map(.x = data, ~ .x %>% arrange(Time) %>%  modify_at(.at = c("Measurement", "Units"), as.character)))  
  
  # mutate(data = map(.x = data, ~ .x %>% modify_at(.at = "Time", parse_date_time, orders = c("ymdHMS", "ymd"), tz = "NZ")))

## test the data
testTimeStamp <- function(data, noOfSec = 10){
  time <- data$Time
  # errorsLgl <- time %>%  interval(lead(time, default = Inf)) %>% int_length() %>% abs < noOfSec
  errorsLgl <- time %>%  interval(lead(time, default = Inf)) %>% int_length() %>% abs < noOfSec
  errorCount <- sum(errorsLgl, na.rm = TRUE)
  if(errorCount == 0){
    return(list(errorCount = errorCount, errorText = "No errors"))}
  else{
    errorInfo <- which(errorsLgl)
    errorString <- paste0("Check entries circa ", time[errorInfo], " and ", time[errorInfo + 1])
    return(list(errorCount = errorCount, errorText = errorString))
  }
}

siteObservations_errors <- siteObservations_clean %>%
  mutate(errorList = map(.x = data, testTimeStamp),
         errorCount = map_dbl(errorList, "errorCount"),
         errorCode = if_else(errorCount > 0, 1, 0),
         dataLength = map_dbl(.x = data, nrow)) %>% 
  arrange(desc(errorCount)) %>% 
  filter(errorCode != 0)
  

# Log file generation and recording ---------------------------------------
 
logDateStamp <- stamp_date("20181231")
logFilepath <- paste0("output/logWebserverQuery_", logDateStamp(today()), ".csv")

writeToFile <- siteObservations_errors %>%
  mutate(errorText = map(errorList, "errorText")) %>%
  select(siteName, RequestAs, errorText) %>%
  unnest %>%
  # arrange(siteName, RequestAs, errorText)
  write_csv(logFilepath)


# # write_csv(siteMeasurementsTidy, "output/siteMeasurementsTidy.csv")



