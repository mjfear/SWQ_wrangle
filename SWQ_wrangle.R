### prereqs ###

library(tidyverse)
library(hillr)

### dataset specific ###

endpointMDC <- "http://hydro.marlborough.govt.nz/soewq.hts?"

### Get data ###

sites <- getHilltopSites(endpoint = endpointMDC)

measurements <- getHilltopMeasurements(endpoint = endpointMDC, site = sites[1,1])

data <- fullGetHilltopData(endpoint = endpointMDC,
                           sites = sites[,1],
                           measurements = c("Ammonia - Nitrogen"),
                           from = "1/1/13",
                           to = "1/1/14",
                           option = "WQ")
