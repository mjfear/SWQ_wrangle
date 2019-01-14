### prereqs ###

library(tidyverse)
library(Hilltop)
library(hillr)

### dataset specific ###

endpointMdcSwq <- "http://hydro.marlborough.govt.nz/soewq.hts?" 
endpointMdc <- "http://hydro.marlborough.govt.nz/mdc.hts?" ##Check this is correct


### Get data ### 

sites <- getHilltopSites(endpoint = endpointMdcSwq) ## HACK to get SWQ sites list from clean dataset - replace this with a collection query or ideally a project code query if possible.

measurementsBySite <- tibble(siteName=sites[,1]) %>%
  mutate(measurementsData=map(.x=siteName, .f=getHilltopMeasurements, endpoint = endpointMdcSwq)) 
  
commonMeasurementString <- measurementsBySite$measurementsData %>% 
  map(select, datasource) %>% 
  reduce(intersect) %>% 
  pull ## Find the common datasource names between all sites listed in sites, so that common measurements for sites list can be queried by fullGetHilltopData

  


#Having built site list and common measurements list, query this subset from Hilltop.
data_WQ <- fullGetHilltopData(endpoint = endpointMdcSwq,
                             sites = measurementsBySite$siteName,
                             measurements = commonMeasurementString,
                             from = "1/1/1907",
                             to = "1/1/19",
                             option = "WQ")

### Wrangle ### 
## wrangle the dataWQ

data_tidy <- data_WQ %>% 
  select(Site, Time, Measurement, Value, `Sample ID.y`) %>% 
  spread(Measurement, Value) %>% 
  arrange(Time) %>% 
  group_by(Site) %>% 
  nest

## test the data
testTimeStamp <- function(data){
  Time <- data$Time
  errorsList <- if_else(as.numeric(Time-lag(Time))<1,1,0)
  if(sum(errorsList, na.rm=TRUE)==0){
    return(list(errorCount=as.integer(0),errorText="No errors"))}
  else{
    errorInfo <- which(errorsList)
    errorString <- paste0("Check entries circa ", Time[errorInfo])
    return(errorCount=sum(errorsList, na.rm = TRUE), errorText=errorString)
  }
}

# testTimeStamp(dataWQ_tidy$data[[1]]$Time)
  
dataWQ_results <- dataWQ_tidy %>% 
  mutate(errorAlert=map(data, testTimeStamp))


