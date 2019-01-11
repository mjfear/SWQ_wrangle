### prereqs ###

library(tidyverse)
library(Hilltop)
library(hillr)

### dataset specific ###

endpointMdcSwq <- "http://hydro.marlborough.govt.nz/soewq.hts?" 
endpointMdc <- "http://hydro.marlborough.govt.nz/mdc.hts?" ##Check this is correct


### Get data ### HACK to get SWQ sites list from clean dataset

sites <- getHilltopSites(endpoint = endpointMdcSwq)

# measurementsFromCollection <- GetCollection()

measurementsBySite <- tibble()
for(i in 1:length(sites[,1])){
  measurements <- getHilltopMeasurements(endpoint = endpointMdcSwq, site = sites[i,1]) #change to main
  measurementNames <- unique(measurements$datasource)
  print(paste0(length(measurementNames), " measurements at ", sites[i,1]))
  # measurements <- bind_rows(sites[site,], measurements)
  # measurementsBySite <- bind_rows(measurementsBySite,measurements)
}

View(measurementsBySite)

# measurements <- getHilltopMeasurements(endpoint = endpointMDC, site = sites[1,1])

# dataWQ <- fullGetHilltopData(endpoint = endpointMDC,
                           sites = sites[,1],
                           measurements = c(measurements[1,1], measurements[10,1]),
                           from = "1/1/07",
                           to = "1/1/19",
                           option = "WQ") 
