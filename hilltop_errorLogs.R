library(tidyverse)
library(lubridate)
library(hillr)
timeStart <- now()
explain_error <- tibble(errorCode = as.integer(c(0:5)),
                        generalError = c("No error reported",
                                         "Websserver query returns blank xml string",
                                         "Variables that are typically characters contain numerics",
                                         "Query from HT webservice for site and measurement returned an error",
                                         "Variables that are typically characters contain numerics",
                                         "Some variables have adjacent readings less than 10 seconds apart"))

endpointMdc <- "http://hydro.marlborough.govt.nz/mdc.hts?"

# Get sites ---------------------------------------------------------------

sites <- getHilltopSites(endpointMdc) 

# Get measurements by site ------------------------------------------------

siteMeasurements <- tibble(errorCode = as.integer(0), siteName = sites[,1]) %>%
  mutate(data = map(.x = siteName, .f = safely(getHilltopMeasurements), endpoint = endpointMdc),
         result = data %>% map("result"),
         errorCode = data %>% map("error") %>% map_lgl(negate(is_null)) %>% if_else(as.integer(1), errorCode),
         errorCode2 = result %>% map("DefaultMeasurement") %>% map_lgl(negate(is.character)) %>% if_else(as.integer(2), errorCode),
         errorCode = if_else(errorCode == 0 & errorCode2 != 0, errorCode2, errorCode)) %>% 
  select(-data, -errorCode2) 

## Which sites return Nulls from webserver measurements list? Which sites return numeric values when characters typical?
siteMeasurement_errors <- siteMeasurements %>% 
  filter(errorCode != 0) %>% 
  arrange(errorCode) %>% 
  select(-result) %>% 
  left_join(explain_error, by = "errorCode")
  
## Which sites contain "Item2"? "Item2" believed to signify presence of metadata. Find and remove to query with Measurement Name on remainder. Filter From != To to remove single data entry values. Filter "Item2" explicit to remove troublesome data entries (approx 2)
siteMeasurements_item2 <- siteMeasurements %>% 
  filter(errorCode == 0) %>% 
  unnest() %>% 
  filter(From != To & DataType == "WQData" & RequestAs != "Item2") %>% 
  filter(MeasurementName == "Item2") %>% 
  select(siteName, RequestAs, From, To) 


# Get measurement values by site ------------------------------------------


## Following section gets site and measurement info singly from Hilltop (using getHilltopData) - singly as fullGetHilltop does not check for typeof consistency between returned values, therefore collapses when building dataframe.
siteValues <- siteMeasurements_item2 %>% 
  mutate(errorCode = as.integer(0),
         data = pmap(.l = lst(endpoint = endpointMdc, site = siteName, measurement = RequestAs, from = From, to = To), 
                     .f = safely(getHilltopData, otherwise = NA)))
         
## test the data
testTimeStamp <- function(data, noOfSec = 10){
  time <- data$Time
  errorsLgl <- time %>%  interval(lead(time, default = Inf)) %>% int_length() %>% abs < noOfSec
  errorCount <- sum(errorsLgl, na.rm = TRUE)
  if(errorCount == 0){
    return(list(errorCount = errorCount, errorText = ""))}
  else{
    errorInfo <- which(errorsLgl)
    errorString <- paste0("Check entries circa ", time[errorInfo], " and ", time[errorInfo + 1])
    return(list(errorCount = errorCount, errorText = errorString))
  }
}

## wrangle data
siteValues_clean <- siteValues %>% 
  mutate(safeResult = data %>% map("result") %>% map(arrange, Time),
         errorCode = data %>% map("error") %>% map_lgl(negate(is_null)) %>% 
           {if_else(. & errorCode == 0, as.integer(3), errorCode)}) %>% 
  select(errorCode, siteName, RequestAs, safeResult) %>% 
  mutate(errorCode = safeResult %>% map("Measurement") %>% map_lgl(negate(is.character)) %>% 
           {if_else(. & errorCode == 0, as.integer(4), errorCode)},
         safeResult = safeResult %>% 
           map_if(.p = errorCode == 4, ~.x %>% modify_at(.at = c("Measurement", "Units"), as.character)),
         errorList = map(.x = safeResult, testTimeStamp),
         errorCount = map_dbl(errorList, "errorCount"),
         errorCode = if_else(errorCode == 0 & errorCount > 0, as.integer(5), errorCode),
         dataLength = map_dbl(.x = safeResult, nrow)
         )
  
# Log file generation and recording ---------------------------------------
## Play with code here first

siteValues_errors <- siteValues_clean %>% 
  mutate(specificError = map(errorList, "errorText")) %>% 
  select(errorCode, siteName, RequestAs, specificError) %>% 
  filter(errorCode != 0) %>% 
  left_join(explain_error, by = "errorCode") %>% 
  unnest

logDateStamp <- stamp_date("20181231")
logFilepath <- paste0("output/logWebserverQuery_", logDateStamp(today()), ".csv")

writeToFile <- siteMeasurement_errors %>%
  bind_rows(siteValues_errors) %>% 
  select(errorCode, siteName, RequestAs, generalError, specificError) %>%
  write_csv(logFilepath)

timeFinish <- now()
