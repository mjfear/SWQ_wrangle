# Prereqs and definitions -------------------------------------------------
library(tidyverse)
library(lubridate)
library(Hilltop)
library(hillr)

datestr <- stamp("24/02/1985")

endpoint <- "http://hydro.marlborough.govt.nz/soewq.hts?"
# endpoint <- "http://hydro.marlborough.govt.nz/mdc.hts?"


# build quasi hilltop collection ------------------------------------------

sites_list <- read_csv("data/siteText.csv") %>% 
  select(hilltopName:reportRegion)

sites <- sites_list %>% 
  left_join(getHilltopSites(endpoint), by = c("hilltopName" = "site"))
  
parameters_list <- read_csv("data/parameterText.csv") %>% 
  mutate(parameterUnits = str_replace_na(parameterUnits, replacement = ""))

measurement_query <-  parameters_list %>% 
  select(hilltopMeasurementName) %>% filter(is.na(.) == FALSE) %>% pull

guidelines_tbl <- parameters_list %>% 
  select(shortParameterName, guideline_lower, guideline_upper) %>% 
  filter(is.na(guideline_lower) == FALSE | is.na(guideline_upper) == FALSE)

measurements_raw <- sites_list %>% 
  group_by(hilltopName, sitePublicName, reportRegion) %>% 
  mutate(data = map(.x = hilltopName, .f = fullGetHilltopData, 
                    endpoint = endpoint, measurements = measurement_query, 
                    from = "01/01/2007", to = datestr(today()), 
                    option = "WQ")) %>% 
  unnest() %>% ungroup %>% select(-hilltopName)

measurements <- measurements_raw %>% 
  select(sitePublicName:Time, Measurement, Value, QualityCode, QAComment = `QA Comment`, Project, SampleID = `Sample ID`) %>% 
  mutate(Value = Value %>% str_remove(">|<") %>% as.double(),
         QualityCode = QualityCode %>% as.integer()) %>% 
  left_join(parameters_list %>% select(hilltopParameterName, parameter = shortParameterName), 
            by = c("Measurement" = "hilltopParameterName")) %>% 
  select(-Measurement) %>% 
  filter(Project == "SW SoE" | is.na(Project) == TRUE) 

measurements_spread <- measurements %>% 
  filter(QualityCode >= 500) %>% 
  select(-QualityCode, -QAComment) %>% 
  spread(parameter, Value) %>%
  mutate(SINitrogen = Ammonia + Nitrite) %>% 
  select(-Nitrite) %>% 
  arrange(Site, Time)



measurements_WQI <- measurements_spread %>%
  group_by(sitePublicName, reportRegion, Site) %>% 
  nest %>% 
  mutate(WQI_calcs = map(data, func_SWQ_calcWQI, guidelines_tbl))

measurements_WQI$WQI_calcs[[26]] 

 %>% %>%
  transmute(test = map(data, "F2") %>% map_dbl(sum))
%>% map_dbl(~if_else, .>0, 1, 0) %>% sum)
    
  


measurements_WQI$WQI_calcs[[1]]

View(measurements_WQI$WQI_calcs[[1]])




test2 <- test %>%
  select(Time:value, F2 = fail, F3 = excursion) %>% 
  nest(value:F3) %>% 
  spread(parameter, data) %>% 
  unnest(.sep = "_")
  
nest(fail_name:excursion) %>% spread(parameter, value)
  spread(key = parameter, value = value) %>% 
  spread(key = fail_name, value = fail) %>% 
  spread(key = excursion_name, value = excursion)
 
  inner_join(parameters_list %>% select(shortParameterName, guideline_lower, guideline_upper), 
             by = c("param_name" = "shortParameterName"))

measurements_WQI$data[[1]] %>% func_SWQ_calcWQI(0,1)

  group_by(sitePublicName, reportRegion, Site, Time) %>% 
  nest() %>% 
  mutate(n = map_dbl(data, nrow)) %>% 
  filter(n!=1) %>% 
  unnest


%>% 
  group_by(SampleID) %>% nest() %>% 
  mutate(countVars = map_dbl(data, nrow))
  


# older coding ------------------------------------------------------------

mutate(data = hilltopName %>% 
           safely(map_df(hilltopName, getHilltopMeasurements(endpoint, hilltopName))))
  
getHilltopMeasurements(endpoint, sites_list[1,1])
### check data can be got ### 

sites <- getHilltopSites(endpoint = endpoint)

n <- 1 + n
colNamesBoo <- TRUE
savePath <- now() %>% 
{paste0("output/log_",year(.), str_pad(month(.),width=2, side="left", pad="0"),str_pad(day(.),width=2, side="left", pad="0"),"_",n,".csv")}

for(i in 31:100){
  measurements <- getHilltopMeasurements(endpoint = endpointMdc, site = sites[i,1])
  measurementsFiltered <- measurements %>% 
    filter(MeasurementName != "Item2")
  for(j in 1:length(measurementsFiltered$MeasurementName)){
    if(exists("errorOut")==TRUE){rm(errorOut)}
    # print("pause")
    Sys.sleep(0.2)
    # print("start")
    errorOut <- fullGetHilltopData(endpoint = endpointMdc,
                                   site = sites[i,1],
                                   measurements = measurementsFiltered$MeasurementName[j])
    if(dim(errorOut)[1]>0){
      printStr <- paste0(sites[i,1], "(i=",i,") and measurement ", measurements[j,1], "j=",j," return a value from Webserver")
      errorStr <- "Return OK"
      errorCode <- 0
    } else {
      printStr <- paste0(sites[i,1], " and measurement ", measurements[j,1], " do not return a value from Webserver")
      errorStr <- "Data returned from webserver is empty"
      errorCode <- 1
    }
      print(printStr)
      saveStr <- tibble(siteName=sites[i,1], measurement=measurementsFiltered$MeasurementName[j], errorCode=errorCode, errorString=errorStr)
      write_csv(saveStr, savePath, append=TRUE, col_names = colNamesBoo)
      if(colNamesBoo==TRUE){colNamesBoo <- FALSE}
    
  }
}




measurementsBySite <- tibble(siteName=sites[,1]) %>%
  mutate(measurementsData=map(.x=siteName, .f=getHilltopMeasurements, endpoint = endpointMdc)) 

 
# commonMeasurementString <- measurementsBySite$measurementsData %>% 
#   map(select, datasource) %>% 
#   reduce(intersect) %>% 
#   pull ## Find the common datasource names between all sites listed in sites, so that common measurements for sites list can be queried by fullGetHilltopData


#Having built site list and common measurements list, query this subset from Hilltop.
data_WQ <- fullGetHilltopData(endpoint = endpointMdc,
                             sites = sites$site,#measurementsBySite$siteName,
                             measurements = "pH",#commonMeasurementString,
                             from = "1/1/1907",
                             to = "1/1/19",
                             option = "None")

dataTest <- fullGetHilltopData(endpoint = endpointMdc,
                           site = "Opawa River at Elizabeth St",
                           measurement = "Conductivity (Field)", #pH
                           from = "1/1/1907",
                           to = "1/1/19",
                           option = "None")

### Wrangle ### 
## wrangle the dataWQ

data_tidy <- data_WQ %>% 
  select(Site, Time, Measurement, Value, `Sample ID.y`) %>% 
  spread(Measurement, Value) %>% 
  arrange(Time) %>% 
  group_by(Site) %>% 
  nest

dataTest_tidy <- dataTest %>% 
  arrange(Time) %>% 
  group_by(Site) %>% 
  nest


## test the data
testTimeStamp <- function(data){
  Time <- data$Time
  errorsList <- if_else(as.numeric(Time-lag(Time))<3600*24,TRUE,FALSE)
  if(sum(errorsList, na.rm=TRUE)==0){
    return(list(errorCount=as.integer(0),errorText="No errors"))}
  else{
    errorInfo <- which(errorsList)
    errorString <- paste0("Check entries circa ", Time[errorInfo])
    return(errorCount=sum(errorsList, na.rm = TRUE), errorText=errorString)
  }
}

# testTimeStamp(dataWQ_tidy$data[[1]]$Time)
  
data_results <- dataTest %>% 
  mutate(errorAlert=map(data, testTimeStamp))

testTimeStamp(dataTest)

